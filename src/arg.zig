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

    /// Tests if this arg represents a boolean flag without a value.
    ///
    /// The provided `flags` is a tuple of either short flag chars or
    /// long flag strings, both *without* their leading `-`
    /// ,e.g. `.{ "help", 'h' }`
    pub fn hasFlag(self: *const Arg, flags: anytype) bool {
        return self.flagOf(flags) catch .long != null;
    }

    pub const Kind = enum {
        long,
        short,
    };

    pub const FlagError = error{UnexpectedValueForFlag};

    /// Tests if this arg represents a boolean flag.
    /// Returns the kind of flag that has matched (long or short).
    /// If a value is provided to a long flag, an error is returned.
    ///
    /// The provided `flags` is a tuple of either short flag chars or
    /// long flag strings, both *without* their leading `-`
    /// ,e.g. `.{ "help", 'h' }`
    pub fn flagOf(self: *const Arg, flags: anytype) FlagError!?Kind {
        validateFlags(flags);

        switch (self.*) {
            .shorts => |shorts| {
                var sh = shorts;
                while (sh.nextFlag()) |s| {
                    inline for (flags) |f| if (@typeInfo(@TypeOf(f)) != .Pointer) {
                        if (s == f) return .short;
                    };
                }
            },
            .long => |long| {
                inline for (flags) |f| if (@typeInfo(@TypeOf(f)) == .Pointer) {
                    if (std.mem.eql(u8, long.flag, f)) {
                        if (long.value != null) return FlagError.UnexpectedValueForFlag;
                        return .long;
                    }
                };
            },
            else => {},
        }

        return null;
    }

    pub const ArgValue = union(Kind) {
        long: ?[:0]const u8,
        short: ?[:0]const u8,

        pub fn value(self: ArgValue) ?[:0]const u8 {
            return switch (self) {
                .long, .short => |v| v,
            };
        }
    };

    /// Returns the value represented by the given `flags`, or null if it doesn't match those.
    ///
    /// The provided `flags` is a tuple of either short flag chars or
    /// long flag strings, both *without* their leading `-`
    /// ,e.g. `.{ "help", 'h' }`
    pub fn valueOf(self: *const Arg, flags: anytype) ?ArgValue {
        validateFlags(flags);

        switch (self.*) {
            .shorts => |shorts| {
                var sh = shorts;
                while (sh.nextFlag()) |s| {
                    inline for (flags) |f| if (@typeInfo(@TypeOf(f)) != .Pointer) {
                        if (s == f) {
                            const val = sh.value();
                            const value = if (val.len > 0) val else null;
                            return .{ .short = value };
                        }
                    };
                }
            },
            .long => |long| {
                inline for (flags) |f| if (@typeInfo(@TypeOf(f)) == .Pointer) {
                    if (std.mem.eql(u8, long.flag, f)) return .{ .long = long.value };
                };
            },
            else => {},
        }

        return null;
    }

    fn validateFlags(flags: anytype) void {
        const flags_ti = @typeInfo(@TypeOf(flags));
        if (flags_ti != .Struct or flags_ti.Struct.is_tuple == false) {
            @compileError(std.fmt.comptimePrint(
                "Expected `flags` to be a tuple, but it is `{s}`.",
                .{@typeName(@TypeOf(flags))},
            ));
        }

        inline for (flags, 0..) |f, i| {
            switch (@typeInfo(@TypeOf(f))) {
                .Pointer => |p| {
                    if (p.size == .Slice and p.child == u8) continue;
                    if (p.size == .One) {
                        const pointee = @typeInfo(p.child);
                        if (pointee == .Array and pointee.Array.child == u8) continue;
                    }
                    @compileError(std.fmt.comptimePrint(
                        "Expected field `{}` to be a `str`/`[]const u8`, but it is `{s}`.",
                        .{ i, @typeName(@TypeOf(f)) },
                    ));
                },
                .ComptimeInt => {},
                .Int => |int| {
                    if (int.signedness != .unsigned or int.bits > 21) {
                        @compileError(std.fmt.comptimePrint(
                            "Expected field `{}` to be a `u21` coercible int, but it is `{s}`.",
                            .{ i, @typeName(@TypeOf(f)) },
                        ));
                    }
                },
                else => {
                    @compileError(std.fmt.comptimePrint(
                        "Expected field `{}` to be a `char`/`u21` or a `[]const u8`, but it is `{s}`.",
                        .{ i, @typeName(@TypeOf(f)) },
                    ));
                },
            }
        }
    }

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

    test hasFlag {
        const expect = std.testing.expectEqual;

        var short_arg = Arg{ .shorts = .{ .flags = "help" } };
        try expect(true, short_arg.hasFlag(.{ 'h', "help" }));
        try expect(false, short_arg.hasFlag(.{ 'v', "value" }));

        var long_arg = Arg{ .long = .{ .flag = "help", .value = null } };
        try expect(true, long_arg.hasFlag(.{ 'h', "help" }));
        try expect(false, long_arg.hasFlag(.{ 'v', "value" }));

        var long_arg_with_value = Arg{ .long = .{ .flag = "help", .value = "foo" } };
        try expect(false, long_arg_with_value.hasFlag(.{ 'v', "value" }));

        // runtime values
        var short_flag: u8 = 'h';
        var long_flag: []const u8 = "help";
        _ = .{ &short_flag, &long_flag };
        try expect(true, short_arg.hasFlag(.{ short_flag, long_flag }));
        try expect(true, long_arg.hasFlag(.{ short_flag, long_flag }));
    }

    test flagOf {
        const expect = std.testing.expectEqual;

        var short_arg = Arg{ .shorts = .{ .flags = "help" } };
        try expect(.short, try short_arg.flagOf(.{ 'h', "help" }));
        try expect(null, try short_arg.flagOf(.{ 'v', "value" }));

        var long_arg = Arg{ .long = .{ .flag = "help", .value = null } };
        try expect(.long, try long_arg.flagOf(.{ 'h', "help" }));
        try expect(null, try long_arg.flagOf(.{ 'v', "value" }));

        var long_arg_with_value = Arg{ .long = .{ .flag = "help", .value = "foo" } };
        try std.testing.expectError(error.UnexpectedValueForFlag, long_arg_with_value.flagOf(.{ 'h', "help" }));

        // runtime values
        var short_flag: u8 = 'h';
        var long_flag: []const u8 = "help";
        _ = .{ &short_flag, &long_flag };
        try expect(.short, try short_arg.flagOf(.{ short_flag, long_flag }));
        try expect(.long, try long_arg.flagOf(.{ short_flag, long_flag }));
    }

    test valueOf {
        const expect = std.testing.expectEqual;
        const expectStr = std.testing.expectEqualStrings;

        var short_arg = Arg{ .shorts = .{ .flags = "v=foo" } };
        try expectStr("foo", short_arg.valueOf(.{ 'v', "value" }).?.short.?);
        try expectStr("foo", short_arg.valueOf(.{ 'v', "value" }).?.value().?);
        try expect(null, short_arg.valueOf(.{ 'h', "help" }));

        var short_arg_no_equal = Arg{ .shorts = .{ .flags = "vfoo" } };
        try expectStr("foo", short_arg_no_equal.valueOf(.{ 'v', "value" }).?.short.?);

        var short_arg_no_value = Arg{ .shorts = .{ .flags = "v" } };
        try expect(null, short_arg_no_value.valueOf(.{ 'v', "value" }).?.short);

        var long_arg = Arg{ .long = .{ .flag = "value", .value = "foo" } };
        try expect("foo", long_arg.valueOf(.{ 'v', "value" }).?.long.?);
        try expect("foo", long_arg.valueOf(.{ 'v', "value" }).?.value().?);
        try expect(null, long_arg.valueOf(.{ 'h', "help" }));

        var long_arg_no_value = Arg{ .long = .{ .flag = "value", .value = null } };
        try expect(null, long_arg_no_value.valueOf(.{ 'v', "value" }).?.long);

        // runtime values
        var short_flag: u8 = 'v';
        var long_flag: []const u8 = "value";
        _ = .{ &short_flag, &long_flag };
        try expectStr("foo", short_arg.valueOf(.{ short_flag, long_flag }).?.short.?);
        try expectStr("foo", long_arg.valueOf(.{ short_flag, long_flag }).?.long.?);
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
