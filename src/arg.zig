// SPDX-License-Identifier: MIT

/// A single Arg. See the docs for each variant for details.
/// An `Arg` does not own any memory and can be `memcpy`d.
pub const Arg = union(enum) {
    /// The special value `"--"`. Encountering this value does not have any special effect.
    /// To escape any following arguments from the parser, use `nextValue` instead of `next`.
    escape,
    /// A long flag with an optional value (`"--long[=value]"`).
    long: Long,
    /// A collection for short flags ("-short").
    shorts: Shorts,
    /// A free-standing value, which could be a subcommand or a positional argument.
    value: [:0]const u8,

    pub const Long = struct {
        flag: []const u8,
        value: ?[:0]const u8 = null,

        pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = .{ fmt, options };
            return writer.print("Long{{ .flag = --{s}, .value = {?s} }}", .{ self.flag, self.value });
        }
    };

    /// An iterator over all short values in a combined flag or access to a short flag and its value.
    /// Calling `next` returns the next individual flag as long as it is valid UTF-8.
    /// Use `value` to read the rest of the flags as a value.
    pub const Shorts = struct {
        /// The remaining string for all short flags.
        /// Memory is borrowed and must not be deinitialized.
        flags: [:0]const u8,

        /// Return the next flag, if available.
        /// If the flag is not a valid UTF-8 codepoint, all remaining flags are returned as the `suffix`
        pub fn next(self: *Shorts) ?Short {
            if (self.flags.len == 0) return null;

            if (std.unicode.utf8ByteSequenceLength(self.flags[0])) |len| {
                defer self.flags = self.flags[len..];
                const slice = self.flags[0..len];

                if (@import("builtin").os.tag == .windows) {
                    return .{ .flag = std.unicode.wtf8Decode(slice) catch unreachable };
                } else {
                    return .{ .flag = std.unicode.utf8Decode(slice) catch unreachable };
                }
            } else |_| {
                defer self.flags = "";
                return .{ .suffix = self.flags };
            }
        }

        /// Return the next flag if it is a valid unicode codepoint
        /// or null if it isn't.
        pub fn nextFlag(self: *Shorts) ?u21 {
            switch (self.next() orelse return null) {
                .flag => |flag| return flag,
                .suffix => return null,
            }
        }

        /// Returns the remaining shortflags as a single value
        /// without consuming those values
        pub fn peekValue(self: *const Shorts) [:0]const u8 {
            return if (std.mem.startsWith(u8, self.flags, "=")) self.flags[1..] else self.flags;
        }

        /// Returns all remaining short flags as a single value
        pub fn value(self: *Shorts) [:0]const u8 {
            defer self.flags = "";
            return self.peekValue();
        }

        /// Check if the remaining flags look like a number (integer or float).
        /// If this returns true, calling `std.fmt.parseFloat` on `value()` will succeed.
        /// This can be used parse values like `"-42"` as negative number values instead of short flags.
        pub fn looks_like_number(self: Shorts) bool {
            return is_number(self.flags);
        }

        pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            return writer.print("Short{{ -{s} }}", .{self.flags});
        }
    };

    /// A single short flag
    pub const Short = union(enum) {
        /// A single char flag of this short arg
        flag: u21,
        /// A non UTF-8 suffix of the remaining short arg
        suffix: [:0]const u8,

        pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = .{ fmt, options };
            switch (self) {
                .flag => |flag| return writer.print("Short{{ -{u} }}", .{flag}),
                .suffix => |suffix| return writer.print("Short{{ -{s} }}", .{suffix}),
            }
        }
    };

    test Shorts {
        const t = std.testing;

        var shorts = Shorts{ .flags = "abc" };
        try t.expectEqual('a', shorts.next().?.flag);
        try t.expectEqual('b', shorts.next().?.flag);
        try t.expectEqual('c', shorts.next().?.flag);
        try t.expect(shorts.next() == null);

        var repeated = Shorts{ .flags = "vvv" };
        try t.expectEqual('v', repeated.next().?.flag);
        try t.expectEqual('v', repeated.next().?.flag);
        try t.expectEqual('v', repeated.next().?.flag);
        try t.expect(repeated.next() == null);

        var unicode = Shorts{ .flags = "クヌート" };
        try t.expectEqual('ク', unicode.next().?.flag);
        try t.expectEqual('ヌ', unicode.next().?.flag);
        try t.expectEqual('ー', unicode.next().?.flag);
        try t.expectEqual('ト', unicode.next().?.flag);
        try t.expect(unicode.next() == null);

        var emoji = Shorts{ .flags = "😀🦀😅" };
        try t.expectEqual('😀', emoji.next().?.flag);
        try t.expectEqual('🦀', emoji.next().?.flag);
        try t.expectEqual('😅', emoji.next().?.flag);
        try t.expect(emoji.next() == null);

        var invalid_utf8 = Shorts{ .flags = "abc\xFFdef" };
        try t.expectEqual('a', invalid_utf8.next().?.flag);
        try t.expectEqual('b', invalid_utf8.next().?.flag);
        try t.expectEqual('c', invalid_utf8.next().?.flag);
        try t.expectEqualStrings("\xFFdef", invalid_utf8.next().?.suffix);
        try t.expect(invalid_utf8.next() == null);

        var with_value = Shorts{ .flags = "j12" };
        try t.expectEqual('j', with_value.next().?.flag);
        try t.expectEqualStrings("12", with_value.peekValue());
        try t.expectEqualStrings("12", with_value.value());
        try t.expect(with_value.next() == null);

        var value_with_equals = Shorts{ .flags = "j=12" };
        try t.expectEqual('j', value_with_equals.next().?.flag);
        try t.expectEqualStrings("12", value_with_equals.peekValue());
        try t.expectEqualStrings("12", value_with_equals.value());
        try t.expect(value_with_equals.next() == null);

        var invalid_utf8_flag = Shorts{ .flags = "abc\xFFdef" };
        try t.expectEqual('a', invalid_utf8_flag.nextFlag().?);
        try t.expectEqual('b', invalid_utf8_flag.nextFlag().?);
        try t.expectEqual('c', invalid_utf8_flag.nextFlag().?);
        try t.expectEqual(null, invalid_utf8_flag.nextFlag());
        try t.expect(invalid_utf8_flag.next() == null);
    }
};

fn is_number(arg: []const u8) bool {
    const State = enum { start, integer, dot, fraction, e, exponent };
    var state: State = .start;

    for (arg) |c| {
        switch (state) {
            .start => switch (c) {
                '0'...'9' => state = .integer,
                else => return false,
            },
            .integer => switch (c) {
                '0'...'9' => {},
                '.' => state = .dot,
                'e' => state = .e,
                else => return false,
            },
            .dot => switch (c) {
                '0'...'9' => state = .fraction,
                'e' => state = .e,
                else => return false,
            },
            .fraction => switch (c) {
                '0'...'9' => {},
                'e' => state = .e,
                else => return false,
            },
            .e => switch (c) {
                '0'...'9' => state = .exponent,
                else => return false,
            },
            .exponent => switch (c) {
                '0'...'9' => {},
                else => return false,
            },
        }
    }

    return state != .start and state != .e;
}

test is_number {
    const t = std.testing;

    for ([_][]const u8{ "1", "100", "1.", "100.", "42.0", "13.37", "13e37", "4.2e0", "42.e1337" }) |num| {
        try t.expect(is_number(num));
    }

    for ([_][]const u8{ "", "-", "-42", "+", "+42", ".1", "1.33.7", "1.3e3.7", "e", "42e", "e42" }) |num| {
        try t.expectEqual(false, is_number(num));
    }
}

test "Force analysis" {
    comptime {
        std.testing.refAllDecls(@This());
    }
}

const std = @import("std");
