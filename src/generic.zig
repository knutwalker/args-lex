// SPDX-License-Identifier: MIT

const Arg = @import("arg.zig").Arg;

pub fn verifyIter(comptime Iter: type) void {
    if (@hasDecl(Iter, "next") == false) {
        @compileLog(Iter);
        @compileError("The Iter type must have a `pub fn next` method.");
    }

    switch (@TypeOf(Iter.next)) {
        fn (*Iter) ?[:0]const u8,
        fn (Iter) ?[:0]const u8,
        fn (*Iter) ?[:0]u8,
        fn (Iter) ?[:0]u8,
        => {},
        else => {
            @compileLog(Iter, Iter.next);
            @compileError("The Iter.next method must be `pub fn next(*Self) ?[:0] const u8`.");
        },
    }

    if (@hasDecl(Iter, "deinit")) {
        switch (@TypeOf(Iter.deinit)) {
            fn (*Iter) void,
            fn (*const Iter) void,
            fn (Iter) void,
            => {},
            else => {
                @compileLog(Iter, Iter.deinit);
                @compileError("The Iter.deinit method must be `pub fn deinit(Self) void`.");
            },
        }
    }

    if (@hasDecl(Iter, "reset")) {
        switch (@TypeOf(Iter.reset)) {
            fn (*Iter) void,
            fn (*const Iter) void,
            fn (Iter) void,
            => {},
            else => {
                @compileLog(Iter, Iter.reset);
                @compileError("The Iter.reset method must be `pub fn reset(Self) void`.");
            },
        }
    }
}

/// A generic lexer over command line arguments.
/// The `Iter` type returns the next command line argument.
/// Any quoting, e.g. turning `"foo bar"` into a single argument must be done
/// by the Iter implementation.
pub fn GenericArgs(comptime Iter: type) type {
    comptime verifyIter(Iter);

    return struct {
        iter: Iter,
        peeked: ?RawArg = null,
        returned: ?RawArg = null,

        const Self = @This();
        pub const Inner = Iter;

        /// Create a new Args lexer using the provided iterator.
        /// This can be used to control exactly how the arguments are read.
        /// The iterator must have a function:
        ///
        /// ```
        /// pub fn next(self: *Self) ?[:0]const u8 {}
        /// ```
        pub fn init(iter: Iter) Self {
            return .{ .iter = iter };
        }

        /// Free the internal iterator's buffers.
        /// Invalidates all slices and pointers returned from this Args.
        pub fn deinit(self: *Self) void {
            self.peeked = null;
            self.returned = null;
            if (@hasDecl(Iter, "deinit")) {
                self.iter.deinit();
            }
        }

        /// Reset the underlying iterator, if it is supported.
        pub fn reset(self: *Self) void {
            if (!@hasDecl(Iter, "reset")) {
                @compileLog(Iter);
                @compileError("Underlying Iter does not support reset.");
            }

            self.iter.reset();
        }

        /// Peek at the next `Arg` without consuming it.
        /// Repeated calls will return the same `Arg`.
        /// Calling any form of `next` will invalidate this pointer.
        /// However, calling `peek` will *not* invalidate a pointer
        /// returned from `next`.
        pub fn peek(self: *Self) ?*Arg {
            if (self.peeked == null) {
                const next_raw = self.iter.next() orelse return null;
                const next_arg = parse(next_raw);
                self.peeked = .{ .arg = next_arg, .raw = next_raw };
            }
            return &(self.peeked.?.arg);
        }

        /// Return the next `Arg`.
        /// Calling this will invalidate any pointer previously returned
        /// from any `next` or `peek` method.
        pub fn next(self: *Self) ?*Arg {
            _ = self.peek() orelse return null;
            self.returned = self.peeked;
            self.peeked = null;
            return &(self.returned.?.arg);
        }

        /// Return the next arg as a value, even if it looks like a flag.
        /// Calling this will invalidate any pointer returned from `peek`,
        /// but will not invalidate pointers returned from other `next` methods.
        pub fn nextValue(self: *Self) ?[:0]const u8 {
            if (self.peeked == null) {
                return self.iter.next();
            }
            defer self.peeked = null;
            return self.peeked.?.raw;
        }

        /// Returns the next args only if it is a value.
        /// Calling this will invalidate any pointer returned from `peek`,
        /// and non-null return values will invalidate any pointer returned
        /// from any `next` method.
        pub fn nextIfValue(self: *Self) ?[:0]const u8 {
            const peeked = self.peek() orelse return null;
            switch (peeked.*) {
                .value => return self.nextValue(),
                else => return null,
            }
        }

        /// Skips to next argument without parsing it.
        /// Returns true if an argument was skipped,
        /// false if there are no more arguments.
        pub fn skip(self: *Self) bool {
            return self.nextValue() != null;
        }

        fn parse(arg: [:0]const u8) Arg {
            if (std.mem.startsWith(u8, arg, "--")) {
                const long_arg = arg[2..];
                if (long_arg.len == 0) {
                    return .{ .escape = {} };
                }

                if (std.mem.indexOfScalar(u8, long_arg, '=')) |eql_index| {
                    const flag = long_arg[0..eql_index];
                    const value = long_arg[eql_index + 1 ..];

                    return .{ .long = .{ .flag = flag, .value = value } };
                } else {
                    return .{ .long = .{ .flag = long_arg } };
                }
            }

            if (std.mem.startsWith(u8, arg, "-") and arg.len > 1) {
                const short_args = arg[1..];
                return .{ .shorts = .{ .flags = short_args } };
            }

            return .{ .value = arg };
        }

        const RawArg = struct {
            arg: Arg,
            raw: [:0]const u8,
        };
    };
}

const t = std.testing;
const mkArgs = @import("args.zig").SliceArgs.init;

test "stdio" {
    var args = mkArgs(&.{ "bin", "-" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualStrings("-", args.next().?.value);
    try t.expect(args.next() == null);
}

test "escape" {
    var args = mkArgs(&.{ "bin", "--" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualDeep(.escape, args.next().?.*);
    try t.expect(args.next() == null);
}

test "long with no value" {
    var args = mkArgs(&.{ "bin", "--long" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualDeep(Arg.Long{ .flag = "long" }, args.next().?.long);
    try t.expect(args.next() == null);
}

test "long with value" {
    var args = mkArgs(&.{ "bin", "--long=value" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualDeep(Arg.Long{ .flag = "long", .value = "value" }, args.next().?.long);
    try t.expect(args.next() == null);
}

test "long with value and equals" {
    var args = mkArgs(&.{ "bin", "--long=value=with=equals" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualDeep(Arg.Long{ .flag = "long", .value = "value=with=equals" }, args.next().?.long);
    try t.expect(args.next() == null);
}

test "long with empty value" {
    var args = mkArgs(&.{ "bin", "--long=" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualDeep(Arg.Long{ .flag = "long", .value = "" }, args.next().?.long);
    try t.expect(args.next() == null);
}

test "long with space separated value" {
    var args = mkArgs(&.{ "bin", "--long", "space" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualDeep(Arg.Long{ .flag = "long" }, args.next().?.long);
    try t.expectEqualStrings("space", args.nextValue().?);
    try t.expect(args.next() == null);
}

test "single short" {
    var args = mkArgs(&.{ "bin", "-a" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualDeep(Arg.Shorts{ .flags = "a" }, args.next().?.shorts);
    try t.expect(args.next() == null);
}

test "combined shorts" {
    var args = mkArgs(&.{ "bin", "-abc" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualDeep(Arg.Shorts{ .flags = "abc" }, args.next().?.shorts);
    try t.expect(args.next() == null);
}

test "value" {
    var args = mkArgs(&.{ "bin", "value" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualStrings("value", args.next().?.value);
    try t.expect(args.next() == null);
}

test "combined long, shorts, and value" {
    var args = mkArgs(&.{ "bin", "--long=value", "-abc", "value" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualDeep(Arg.Long{ .flag = "long", .value = "value" }, args.next().?.long);
    try t.expectEqualDeep(Arg.Shorts{ .flags = "abc" }, args.next().?.shorts);
    try t.expectEqualStrings("value", args.nextValue().?);
    try t.expect(args.next() == null);
}

test "peek" {
    var args = mkArgs(&.{ "bin", "a" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualStrings("a", args.peek().?.value);
    try t.expectEqualStrings("a", args.peek().?.value);
    try t.expectEqualStrings("a", args.next().?.value);
    try t.expect(args.peeked == null);

    try t.expect(args.next() == null);
}

test "nextValue" {
    var args = mkArgs(&.{ "bin", "--a", "-b", "--", "c" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualStrings("--a", args.nextValue().?);
    try t.expectEqualStrings("-b", args.nextValue().?);
    try t.expectEqualStrings("--", args.nextValue().?);
    try t.expectEqualStrings("c", args.nextValue().?);
    try t.expect(args.next() == null);
}

test "nextIfValue" {
    var args = mkArgs(&.{ "bin", "--a", "b", "--c", "--d" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualDeep(Arg.Long{ .flag = "a" }, args.next().?.long);
    try t.expectEqualStrings("b", args.nextIfValue().?);
    try t.expectEqualDeep(Arg.Long{ .flag = "c" }, args.next().?.long);
    try t.expect(args.nextIfValue() == null);
    try t.expectEqualDeep(Arg.Long{ .flag = "d" }, args.next().?.long);
    try t.expect(args.next() == null);
}

test "skip" {
    var args = mkArgs(&.{ "bin", "--flag1", "value", "--flag2" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualDeep(Arg.Long{ .flag = "flag1" }, args.next().?.long);
    try t.expectEqual(true, args.skip());
    try t.expectEqualDeep(Arg.Long{ .flag = "flag2" }, args.next().?.long);
    try t.expectEqual(false, args.skip());
    try t.expect(args.next() == null);
}

test "reset" {
    var args = mkArgs(&.{ "bin", "a", "b", "c" });
    defer args.deinit();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualStrings("a", args.next().?.value);
    try t.expectEqualStrings("b", args.next().?.value);
    try t.expectEqualStrings("c", args.next().?.value);
    try t.expect(args.next() == null);

    args.reset();

    try t.expectEqualStrings("bin", args.nextValue().?);
    try t.expectEqualStrings("a", args.next().?.value);
    try t.expectEqualStrings("b", args.next().?.value);
    try t.expectEqualStrings("c", args.next().?.value);
    try t.expect(args.next() == null);
}

test "Force analysis" {
    comptime {
        t.refAllDecls(@This());
    }
}

const std = @import("std");
