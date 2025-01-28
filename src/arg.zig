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
            _ = .{ fmt, options };
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
    /// Note that repeated flags (such as `-vv`) cannot be counted with this method.
    /// To do that, use `parse` instead.
    ///
    /// The provided `flags` is a tuple of either short flag chars or
    /// long flag strings, both *without* their leading `-`
    /// ,e.g. `.{ "help", 'h' }`
    pub fn isFlag(self: *const Arg, flags: anytype) bool {
        return self.parse(bool, flags, null) orelse false catch false;
    }

    pub const Kind = enum {
        long,
        short,
    };

    pub const FlagError = error{
        /// A value was provided for the flag but it is not allowed
        UnexpectedValueForFlag,
    };

    /// Tests if this arg represents a boolean flag.
    /// Returns the kind of flag that has matched (long or short),
    /// or null of it doesn't match.
    /// If a value is provided to a long flag, an error is returned.
    ///
    /// The provided `flags` is a tuple of either short flag chars or
    /// long flag strings, both *without* their leading `-`
    /// ,e.g. `.{ "help", 'h' }`
    pub fn flagOf(self: *const Arg, flags: anytype) ?(FlagError!Kind) {
        validateFlags(flags);

        switch (self.*) {
            .shorts => |shorts| {
                var sh = shorts;
                while (sh.nextFlag()) |s| {
                    inline for (flags) |f| if (@typeInfo(@TypeOf(f)) != .pointer) {
                        if (s == f) return .short;
                    };
                }
            },
            .long => |long| {
                inline for (flags) |f| if (@typeInfo(@TypeOf(f)) == .pointer) {
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

    /// Returns the value represented by the given `flags`,
    /// or null if it doesn't match.
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
                    inline for (flags) |f| if (@typeInfo(@TypeOf(f)) != .pointer) {
                        if (s == f) {
                            const val = sh.value();
                            const value = if (val.len > 0) val else null;
                            return .{ .short = value };
                        }
                    };
                }
            },
            .long => |long| {
                inline for (flags) |f| if (@typeInfo(@TypeOf(f)) == .pointer) {
                    if (std.mem.eql(u8, long.flag, f)) return .{ .long = long.value };
                };
            },
            else => {},
        }

        return null;
    }

    pub const ParseError = error{
        /// A flag requires a value but none where provided
        MissingRequiredValue,
        /// A value cannot be parsed into an enum tag
        InvalidEnumTag,
        /// A value cannot be parsed into the target type
        InvalidValue,
    } || FlagError || std.fmt.ParseIntError || std.fmt.ParseFloatError;

    /// Parsed the value represented by the given `flags` into the return type,
    /// or return null if it doesn't match.
    /// The type determines how the flag is parsed.
    ///
    /// The following values are supported, together with how they influence the parsing:
    ///
    /// |type          | value       | parsing                             |
    /// |--------------|-------------|-------------------------------------|
    /// |`bool`        | not allowed | checks for existence                |
    /// |`usize`       | not allowed | count occurences                    |
    /// |`u*` int      | required    | parse value as unsigned number      |
    /// |`i*` int      | required    | parse value as signed number        |
    /// |`f*` float    | required    | parse value as float                |
    /// |`[]const u8`  | required    | take value                          |
    /// |`[:0]const u8`| required    | take value                          |
    /// |`enum`        | required    | parse value as enum tag name        |
    /// |`?*`          | optional    | don't require value for inner type  |
    ///
    /// Since this method is only parsing a single value, there is no option
    /// to parse multiple values. To allow multiple values separated by some
    /// delimiter, parse as `[]const u8` and do the parsing on your side.
    ///
    /// If the argument itself does not provide a value, but the type requires one,
    /// `args.nextValue()` is called on the provided `args`, expecting it to return
    /// a `[:0]const u8`, which should be the outer args iterator.
    /// This value can also be `{}` (void) or `null` if
    ///
    /// Any type that requires a value can be wrapped in an optional (`?`) to
    /// not require the value. This can be used to differentiate between a
    /// flag that is missing and a flag that is missing a value.
    ///
    /// The provided `flags` is a tuple of either short flag chars or
    /// long flag strings, both *without* their leading `-`
    /// ,e.g. `.{ "help", 'h' }`
    pub fn parse(self: *const Arg, R: type, flags: anytype, args: anytype) ?(ParseError!R) {
        validateFlags(flags);

        switch (self.*) {
            .shorts => |shorts| {
                var sh = shorts;
                while (sh.nextFlag()) |s| {
                    inline for (flags) |f| if (@typeInfo(@TypeOf(f)) != .pointer) {
                        if (s == f) {
                            return try parseShortArg(R, &sh, f, args);
                        }
                    };
                }
            },
            .long => |long| {
                inline for (flags) |f| if (@typeInfo(@TypeOf(f)) == .pointer) {
                    if (std.mem.eql(u8, long.flag, f)) {
                        return try parseLongArg(R, long, args);
                    }
                };
            },
            else => return null,
        }

        return switch (R) {
            bool => false,
            usize => 0,
            else => null,
        };
    }

    fn parseShortArg(R: type, sh: *Shorts, flag: u21, args: anytype) ParseError!R {
        switch (R) {
            bool, ?bool => return true,
            usize, ?usize => {
                var result: usize = 1;
                while (sh.nextFlag()) |s| result += @intFromBool(s == flag);
                return result;
            },
            []const u8, [:0]const u8 => {
                return shortArgValue(sh, args) orelse ParseError.MissingRequiredValue;
            },
            else => switch (@typeInfo(R)) {
                .int => {
                    const value = shortArgValue(sh, args) orelse return ParseError.MissingRequiredValue;
                    return std.fmt.parseInt(R, value, 0);
                },
                .float => {
                    const value = shortArgValue(sh, args) orelse return ParseError.MissingRequiredValue;
                    return std.fmt.parseFloat(R, value);
                },
                .optional => |o| {
                    return parseShortArg(o.child, sh, flag, args) catch |err| switch (err) {
                        ParseError.MissingRequiredValue => null,
                        else => err,
                    };
                },
                .@"enum" => {
                    const value = shortArgValue(sh, args) orelse return ParseError.MissingRequiredValue;
                    return std.meta.stringToEnum(R, value) orelse ParseError.InvalidEnumTag;
                },
                else => {
                    const value = shortArgValue(sh, args) orelse return ParseError.MissingRequiredValue;
                    return parseCustom(R, value);
                },
            },
        }
    }

    fn shortArgValue(sh: *Shorts, args: anytype) ?[:0]const u8 {
        const val = sh.value();
        return if (val.len > 0) val else nextValue(args);
    }

    fn parseLongArg(R: type, long: Long, args: anytype) ParseError!R {
        switch (R) {
            bool, ?bool => {
                return if (long.value) |_| ParseError.UnexpectedValueForFlag else true;
            },
            usize, ?usize => {
                return if (long.value) |_| ParseError.UnexpectedValueForFlag else 1;
            },
            []const u8, [:0]const u8 => {
                return (long.value orelse nextValue(args)) orelse ParseError.MissingRequiredValue;
            },
            else => switch (@typeInfo(R)) {
                .int => {
                    const value = long.value orelse return ParseError.MissingRequiredValue;
                    return std.fmt.parseInt(R, value, 0);
                },
                .float => {
                    const value = long.value orelse return ParseError.MissingRequiredValue;
                    return std.fmt.parseFloat(R, value);
                },
                .optional => |o| {
                    return parseLongArg(o.child, long, args) catch |err| switch (err) {
                        ParseError.MissingRequiredValue => null,
                        else => err,
                    };
                },
                .@"enum" => {
                    const value = long.value orelse return ParseError.MissingRequiredValue;
                    return std.meta.stringToEnum(R, value) orelse ParseError.InvalidEnumTag;
                },
                else => {
                    const value = long.value orelse return ParseError.MissingRequiredValue;
                    return parseCustom(R, value);
                },
            },
        }
    }

    fn nextValue(args: anytype) ?[:0]const u8 {
        switch (@typeInfo(@TypeOf(args))) {
            .void, .null => return null,
            else => return args.nextValue(),
        }
    }

    fn parseCustom(R: type, value: [:0]const u8) ParseError!R {
        if (@hasDecl(R, "parse")) {
            switch (@TypeOf(R.parse)) {
                fn ([]const u8) ParseError!R,
                fn ([:0]const u8) ParseError!R,
                fn ([]const u8) R,
                fn ([:0]const u8) R,
                => {
                    return R.parse(value);
                },
                else => {
                    @compileError("Unsupported parse function for type: " ++ @typeName(R));
                },
            }
        } else {
            @compileError("Unsupported result type of parse: " ++ @typeName(R));
        }
    }

    fn validateFlags(flags: anytype) void {
        const flags_ti = @typeInfo(@TypeOf(flags));
        if (flags_ti != .@"struct" or flags_ti.@"struct".is_tuple == false) {
            @compileError(std.fmt.comptimePrint(
                "Expected `flags` to be a tuple, but it is `{s}`.",
                .{@typeName(@TypeOf(flags))},
            ));
        }

        inline for (flags, 0..) |f, i| {
            switch (@typeInfo(@TypeOf(f))) {
                .pointer => |p| {
                    if (p.size == .slice and p.child == u8) continue;
                    if (p.size == .one) {
                        const pointee = @typeInfo(p.child);
                        if (pointee == .array and pointee.array.child == u8) continue;
                    }
                    @compileError(std.fmt.comptimePrint(
                        "Expected field `{}` to be a `str`/`[]const u8`, but it is `{s}`.",
                        .{ i, @typeName(@TypeOf(f)) },
                    ));
                },
                .comptime_int => {},
                .int => |int| {
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

    const t = std.testing;
    const expect = t.expectEqual;
    const expectStr = t.expectEqualStrings;

    test Shorts {
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

    test isFlag {
        const short_arg = Arg{ .shorts = .{ .flags = "help" } };
        try expect(true, short_arg.isFlag(.{ 'h', "help" }));
        try expect(false, short_arg.isFlag(.{ 'v', "value" }));

        const long_arg = Arg{ .long = .{ .flag = "help", .value = null } };
        try expect(true, long_arg.isFlag(.{ 'h', "help" }));
        try expect(false, long_arg.isFlag(.{ 'v', "value" }));

        const long_arg_with_value = Arg{ .long = .{ .flag = "help", .value = "foo" } };
        try expect(false, long_arg_with_value.isFlag(.{ 'h', "help" }));
        try expect(false, long_arg_with_value.isFlag(.{ 'v', "value" }));

        // runtime values
        var short_flag: u8 = 'h';
        var long_flag: []const u8 = "help";
        _ = .{ &short_flag, &long_flag };
        try expect(true, short_arg.isFlag(.{ short_flag, long_flag }));
        try expect(true, long_arg.isFlag(.{ short_flag, long_flag }));
    }

    test flagOf {
        const short_arg = Arg{ .shorts = .{ .flags = "help" } };
        try expect(.short, short_arg.flagOf(.{ 'h', "help" }));
        try expect(null, short_arg.flagOf(.{ 'v', "value" }));

        const long_arg = Arg{ .long = .{ .flag = "help", .value = null } };
        try expect(.long, long_arg.flagOf(.{ 'h', "help" }));
        try expect(null, long_arg.flagOf(.{ 'v', "value" }));

        const long_arg_with_value = Arg{ .long = .{ .flag = "help", .value = "foo" } };
        try std.testing.expectError(error.UnexpectedValueForFlag, long_arg_with_value.flagOf(.{ 'h', "help" }).?);

        // runtime values
        var short_flag: u8 = 'h';
        var long_flag: []const u8 = "help";
        _ = .{ &short_flag, &long_flag };
        try expect(.short, short_arg.flagOf(.{ short_flag, long_flag }));
        try expect(.long, long_arg.flagOf(.{ short_flag, long_flag }));
    }

    test valueOf {
        const short_arg = Arg{ .shorts = .{ .flags = "v=foo" } };
        try expectStr("foo", short_arg.valueOf(.{ 'v', "value" }).?.short.?);
        try expectStr("foo", short_arg.valueOf(.{ 'v', "value" }).?.value().?);
        try expect(null, short_arg.valueOf(.{ 'h', "help" }));

        const short_arg_no_equal = Arg{ .shorts = .{ .flags = "vfoo" } };
        try expectStr("foo", short_arg_no_equal.valueOf(.{ 'v', "value" }).?.short.?);

        const short_arg_no_value = Arg{ .shorts = .{ .flags = "v" } };
        try expect(null, short_arg_no_value.valueOf(.{ 'v', "value" }).?.short);

        const long_arg = Arg{ .long = .{ .flag = "value", .value = "foo" } };
        try expect("foo", long_arg.valueOf(.{ 'v', "value" }).?.long.?);
        try expect("foo", long_arg.valueOf(.{ 'v', "value" }).?.value().?);
        try expect(null, long_arg.valueOf(.{ 'h', "help" }));

        const long_arg_no_value = Arg{ .long = .{ .flag = "value", .value = null } };
        try expect(null, long_arg_no_value.valueOf(.{ 'v', "value" }).?.long);

        // runtime values
        var short_flag: u8 = 'v';
        var long_flag: []const u8 = "value";
        _ = .{ &short_flag, &long_flag };
        try expectStr("foo", short_arg.valueOf(.{ short_flag, long_flag }).?.short.?);
        try expectStr("foo", long_arg.valueOf(.{ short_flag, long_flag }).?.long.?);
    }

    test parse {
        const color = enum { auto, always, never };

        // parse a color flag with the following rules
        //  ``  (no flag provided) : color=auto
        //  `--color`              : color=always
        //  `--color=<value>`      : color=<value>
        const parse_color = struct {
            fn f(arg: Arg) color {
                const color_flag = arg.parse(?color, .{ 'c', "color" }, null);
                return ((color_flag orelse .auto) catch unreachable) orelse .always;
            }
        }.f;

        try std.testing.expectEqual(
            .auto,
            parse_color(Arg{ .long = .{ .flag = "not-color", .value = null } }),
        );

        try std.testing.expectEqual(
            .always,
            parse_color(Arg{ .long = .{ .flag = "color", .value = null } }),
        );

        try std.testing.expectEqual(
            .never,
            parse_color(Arg{ .long = .{ .flag = "color", .value = "never" } }),
        );

        try std.testing.expectEqual(
            .auto,
            parse_color(Arg{ .long = .{ .flag = "color", .value = "auto" } }),
        );

        try std.testing.expectEqual(
            .always,
            parse_color(Arg{ .long = .{ .flag = "color", .value = "always" } }),
        );
    }

    test "parse into bool" {
        const short_arg = Arg{ .shorts = .{ .flags = "abbbc=de" } };

        try expect(true, short_arg.parse(bool, .{'a'}, null));
        try expect(false, short_arg.parse(bool, .{'x'}, null));

        const long_arg = Arg{ .long = .{ .flag = "a", .value = null } };

        try expect(true, long_arg.parse(bool, .{"a"}, null));
        try expect(false, long_arg.parse(bool, .{"x"}, null));
    }

    test "parse into usize" {
        const short_arg = Arg{ .shorts = .{ .flags = "abbbc=de" } };

        try expect(1, short_arg.parse(usize, .{'a'}, null));
        try expect(3, short_arg.parse(usize, .{'b'}, null));
        try expect(0, short_arg.parse(usize, .{'x'}, null));

        const long_arg = Arg{ .long = .{ .flag = "a", .value = null } };

        try expect(1, long_arg.parse(usize, .{"a"}, null));
        try expect(0, long_arg.parse(usize, .{"x"}, null));
    }

    test "parse into enum" {
        const short_arg = Arg{ .shorts = .{ .flags = "abbbc=de" } };
        try expect(.de, short_arg.parse(enum { de, fg }, .{'c'}, null));
        try t.expectError(ParseError.InvalidEnumTag, short_arg.parse(enum { a }, .{'c'}, null).?);

        const long_arg = Arg{ .long = .{ .flag = "b", .value = "foo" } };
        try expect(.foo, long_arg.parse(enum { foo, bar }, .{"b"}, null));
        try t.expectError(ParseError.InvalidEnumTag, long_arg.parse(enum { a }, .{"b"}, null).?);
    }

    test "parse into str" {
        const short_arg = Arg{ .shorts = .{ .flags = "abbbc=de" } };

        try expectStr("de", try short_arg.parse([]const u8, .{'c'}, null).?);
        try expectStr("de", try short_arg.parse([:0]const u8, .{'c'}, null).?);

        try t.expect(short_arg.parse([]const u8, .{'x'}, null) == null);
        try t.expect(short_arg.parse(?[]const u8, .{'x'}, null) == null);
        try expect(@as(?[]const u8, null), try short_arg.parse(?[]const u8, .{'e'}, null).?);

        const long_a_arg = Arg{ .long = .{ .flag = "a", .value = null } };
        const long_b_arg = Arg{ .long = .{ .flag = "b", .value = "foo" } };

        try expectStr("foo", try long_b_arg.parse([]const u8, .{"b"}, null).?);
        try expectStr("foo", try long_b_arg.parse([:0]const u8, .{"b"}, null).?);

        try t.expect(long_a_arg.parse([]const u8, .{"x"}, null) == null);
        try t.expect(long_a_arg.parse(?[]const u8, .{"x"}, null) == null);
        try expect(@as(?[]const u8, null), try long_a_arg.parse(?[]const u8, .{"a"}, null).?);
    }

    test "parse into uint" {
        const short_arg = Arg{ .shorts = .{ .flags = "a42" } };
        try expect(42, short_arg.parse(u8, .{'a'}, null));
        try expect(42, short_arg.parse(u16, .{'a'}, null));
        try expect(42, short_arg.parse(u32, .{'a'}, null));
        try expect(42, short_arg.parse(u64, .{'a'}, null));

        const invalid_number_short_arg = Arg{ .shorts = .{ .flags = "a42x" } };
        try t.expectError(ParseError.InvalidCharacter, invalid_number_short_arg.parse(u8, .{'a'}, null).?);

        const overflow_short_arg = Arg{ .shorts = .{ .flags = "a1337" } };
        try t.expectError(ParseError.Overflow, overflow_short_arg.parse(u8, .{'a'}, null).?);

        const long_arg = Arg{ .long = .{ .flag = "a", .value = "42" } };
        try expect(42, long_arg.parse(u8, .{"a"}, null));
        try expect(42, long_arg.parse(u16, .{"a"}, null));
        try expect(42, long_arg.parse(u32, .{"a"}, null));
        try expect(42, long_arg.parse(u64, .{"a"}, null));

        const invalid_number_long_arg = Arg{ .long = .{ .flag = "a", .value = "42x" } };
        try t.expectError(ParseError.InvalidCharacter, invalid_number_long_arg.parse(u8, .{"a"}, null).?);

        const overflow_long_arg = Arg{ .long = .{ .flag = "a", .value = "1337" } };
        try t.expectError(ParseError.Overflow, overflow_long_arg.parse(u8, .{"a"}, null).?);
    }

    test "parse into int" {
        const short_arg = Arg{ .shorts = .{ .flags = "a42" } };
        try expect(42, short_arg.parse(i8, .{'a'}, null));
        try expect(42, short_arg.parse(i16, .{'a'}, null));
        try expect(42, short_arg.parse(i32, .{'a'}, null));
        try expect(42, short_arg.parse(i64, .{'a'}, null));
        try expect(42, short_arg.parse(isize, .{'a'}, null));

        const negative_short_arg = Arg{ .shorts = .{ .flags = "a-42" } };
        try expect(-42, negative_short_arg.parse(i8, .{'a'}, null));
        try expect(-42, negative_short_arg.parse(i16, .{'a'}, null));
        try expect(-42, negative_short_arg.parse(i32, .{'a'}, null));
        try expect(-42, negative_short_arg.parse(i64, .{'a'}, null));
        try expect(-42, negative_short_arg.parse(isize, .{'a'}, null));

        const invalid_number_short_arg = Arg{ .shorts = .{ .flags = "a42x" } };
        try t.expectError(ParseError.InvalidCharacter, invalid_number_short_arg.parse(i8, .{'a'}, null).?);

        const overflow_short_arg = Arg{ .shorts = .{ .flags = "a1337" } };
        try t.expectError(ParseError.Overflow, overflow_short_arg.parse(i8, .{'a'}, null).?);

        const long_arg = Arg{ .long = .{ .flag = "a", .value = "42" } };
        try expect(42, long_arg.parse(i8, .{"a"}, null));
        try expect(42, long_arg.parse(i16, .{"a"}, null));
        try expect(42, long_arg.parse(i32, .{"a"}, null));
        try expect(42, long_arg.parse(i64, .{"a"}, null));
        try expect(42, long_arg.parse(isize, .{"a"}, null));

        const negative_long_arg = Arg{ .long = .{ .flag = "a", .value = "-42" } };
        try expect(-42, negative_long_arg.parse(i8, .{"a"}, null));
        try expect(-42, negative_long_arg.parse(i16, .{"a"}, null));
        try expect(-42, negative_long_arg.parse(i32, .{"a"}, null));
        try expect(-42, negative_long_arg.parse(i64, .{"a"}, null));
        try expect(-42, negative_long_arg.parse(isize, .{"a"}, null));

        const invalid_number_long_arg = Arg{ .long = .{ .flag = "a", .value = "42x" } };
        try t.expectError(ParseError.InvalidCharacter, invalid_number_long_arg.parse(i8, .{"a"}, null).?);

        const overflow_long_arg = Arg{ .long = .{ .flag = "a", .value = "1337" } };
        try t.expectError(ParseError.Overflow, overflow_long_arg.parse(i8, .{"a"}, null).?);
    }

    test "parse into float" {
        const short_arg = Arg{ .shorts = .{ .flags = "a42.42" } };
        try expect(42.42, short_arg.parse(f16, .{'a'}, null));
        try expect(42.42, short_arg.parse(f32, .{'a'}, null));
        try expect(42.42, short_arg.parse(f64, .{'a'}, null));
        try expect(42.42, short_arg.parse(f128, .{'a'}, null));

        const negative_short_arg = Arg{ .shorts = .{ .flags = "a-42.42" } };
        try expect(-42.42, negative_short_arg.parse(f16, .{'a'}, null));
        try expect(-42.42, negative_short_arg.parse(f32, .{'a'}, null));
        try expect(-42.42, negative_short_arg.parse(f64, .{'a'}, null));
        try expect(-42.42, negative_short_arg.parse(f128, .{'a'}, null));

        const invalid_number_short_arg = Arg{ .shorts = .{ .flags = "a42.42x" } };
        try t.expectError(ParseError.InvalidCharacter, invalid_number_short_arg.parse(f16, .{'a'}, null).?);

        const long_arg = Arg{ .long = .{ .flag = "a", .value = "42.42" } };
        try expect(42.42, long_arg.parse(f16, .{"a"}, null));
        try expect(42.42, long_arg.parse(f32, .{"a"}, null));
        try expect(42.42, long_arg.parse(f64, .{"a"}, null));
        try expect(42.42, long_arg.parse(f128, .{"a"}, null));

        const negative_long_arg = Arg{ .long = .{ .flag = "a", .value = "-42.42" } };
        try expect(-42.42, negative_long_arg.parse(f16, .{"a"}, null));
        try expect(-42.42, negative_long_arg.parse(f32, .{"a"}, null));
        try expect(-42.42, negative_long_arg.parse(f64, .{"a"}, null));
        try expect(-42.42, negative_long_arg.parse(f128, .{"a"}, null));

        const invalid_number_long_arg = Arg{ .long = .{ .flag = "a", .value = "42.42x" } };
        try t.expectError(ParseError.InvalidCharacter, invalid_number_long_arg.parse(f16, .{"a"}, null).?);
    }

    test "parse into custom type" {
        const Custom = struct {
            a: i32,
            b: B,

            const B = enum { x, y };

            pub fn parse(value: []const u8) ParseError!@This() {
                var splits = std.mem.splitScalar(u8, value, ',');
                const a_value = splits.next() orelse return ParseError.InvalidValue;
                const a = try std.fmt.parseInt(i32, a_value, 10);
                const b_value = splits.next() orelse return ParseError.InvalidValue;
                const b = std.meta.stringToEnum(B, b_value) orelse return ParseError.InvalidEnumTag;
                return .{ .a = a, .b = b };
            }
        };

        const short_arg = Arg{ .shorts = .{ .flags = "a42,x" } };
        try t.expectEqualDeep(
            Custom{ .a = 42, .b = .x },
            try short_arg.parse(Custom, .{'a'}, null).?,
        );

        const long_arg = Arg{ .long = .{ .flag = "a", .value = "42,x" } };
        try t.expectEqualDeep(
            Custom{ .a = 42, .b = .x },
            try long_arg.parse(Custom, .{"a"}, null).?,
        );
    }

    test "parse non flags" {
        const value_arg = Arg{ .value = "42" };

        try expect(null, value_arg.parse(bool, .{'a'}, null));
        try expect(null, value_arg.parse(usize, .{'a'}, null));
        try expect(null, value_arg.parse(i32, .{'a'}, null));
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
