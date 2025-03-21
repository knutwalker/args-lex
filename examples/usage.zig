// SPDX-License-Identifier: MIT

const std = @import("std");

const args_lex = @import("args_lex");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    // The args should be provided by build.zig as:
    // `"-a -42 -bcdef --flag --long=value --also yes -- remaining --args"`
    // The lexer does not map automatically to types, we can do this on our own.
    const MyOptions = struct {
        a: i32 = -1,
        b: bool = false,
        c: ?[:0]const u8 = null,
        flag: bool = false,
        long: [:0]const u8 = "default",
        also: enum { auto, yes, no } = .auto,
        remainder: ?[]const [:0]const u8 = null,
    };

    // Args can be read lazily (and on POSIX systems without allocations)
    // by using `Args`, which uses `std.process.args` internally.
    var process_args = try args_lex.Args.init(alloc);

    // This should be a `defer` instead of the `errdefer`.
    // In this example, we explicitly ``deinit` the args lexer to test for
    // dangling pointers and use-after-free.
    errdefer process_args.deinit();

    // Process args can be read from an existing allocation with `SliceArgs`.
    // This can be used for args from `argsAlloc`
    const args_alloc = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args_alloc);
    var slice_args = args_lex.SliceArgs.init(args_alloc);
    errdefer slice_args.deinit();

    // One benefit of SliceArgs is that they can be reset with `reset`.
    // One might, for example, do a first pass to check for `--help`,
    // then `reset` and do a second pass for the actual parsing.

    // We can also use `std.os.argv` if it is supported by the OS.
    var args_impls = if (comptime args_lex.OsArgs.isSupported()) impls: {
        const os_args = args_lex.OsArgs.init();
        break :impls .{ process_args, slice_args, os_args };
    } else .{ process_args, slice_args };

    inline for (&args_impls) |*args| {

        // Skip the binary name
        _ = args.nextAsValue().?;

        // Initialize the result type with all defaults.
        // If some args are required, they need to be wrapped in an option
        // and validated after the loop.
        var options: MyOptions = .{};

        // The main lexing loop. `next` will return an `*Arg`.
        // Don't store to pointers without making a copy,
        // every call to `next` will invalidate those pointers.
        // Calling `next` (or any of its variants) will never fail or panic.
        // A `null` indicated the end of the arguments list.
        while (args.next()) |arg| {
            switch (arg) {
                // `arg` is the `--` escape. Usually that means that the remaining
                // args are no longer supposed to be parsed.
                // That decision is up to the user, and we are doing just that.
                .escape => {
                    var remainder = std.ArrayList([:0]const u8).init(alloc);
                    defer remainder.deinit();

                    // `nextValue` will return the next args without any parsing
                    while (args.nextAsValue()) |arg_value| {
                        // The slices returned by any "value" like access are
                        // invalidated when `deinit` is called.
                        // It is recommended to `dupe` any slice that should
                        // remain valid after the parsing is done.
                        const value = try alloc.dupeZ(u8, arg_value);
                        try remainder.append(value);
                    }

                    options.remainder = try remainder.toOwnedSlice();
                },
                // `arg` is one or more short flags, or a negative number.
                // That is, it starts with `-`, but not with `--`.
                .shorts => |shorts_arg| {
                    // Command line parsers often do not support negative numbers,
                    // because it is ambiguous. We can choose to look for negative
                    // numbers and treat is as a `value` instead. In this example,
                    // we imply that any negative number must be for the `a` flag.
                    if (shorts_arg.looks_like_number()) {
                        // `value` will return everything *after* the leading `-`,
                        // so we need to negate the number on our own.
                        options.a = -(try std.fmt.parseInt(i32, shorts_arg.peekValue(), 10));
                        continue;
                    }
                    // Iterate over any short flag. There is no special handling
                    // of `=` done in `next`.
                    var shorts = shorts_arg;
                    while (shorts.next()) |short| switch (short) {
                        // The typical case, where the short flag is a valid
                        // Unicode codepoint (`flag` here is a `u21`).
                        .flag => |flag| switch (flag) {
                            // allow, but ignore a flag
                            'a' => {},
                            // flag is given, here we set the option to `true`
                            'b' => options.b = true,
                            // short flags can also take a value, optionally separated
                            // with a `=`. When we see the `c` flag, we take the
                            // remainder as its value.
                            'c' => options.c = try alloc.dupeZ(u8, shorts.value()),
                            else => return error.UnknownFlag,
                        },
                        // In case we encounter an invalid Unicode sequence as
                        // short flags, the parser bails and returns the remainder
                        // as an unparsed suffix. It is up to the user to decide
                        // what to do in this case.
                        .suffix => return error.UnknownFlag,
                    };
                },
                // `arg` is a long flag.
                // That is it starts with `--` and is not just `--` (`.escape`).
                .long => |long| {
                    // we can check for the actual arg using `flag` and `value`.
                    if (std.mem.eql(u8, long.flag, "flag")) {
                        // Some flags are not allowed to take values,
                        // we can check for that.
                        if (long.value) |_| {
                            return error.UnexpectedValue;
                        }
                        options.flag = true;
                    } else if (std.mem.eql(u8, long.flag, "long")) {
                        // the provided `value` is only set if it was given using the
                        // `--flag=value` syntax, but often `--flag value` should
                        // also be allowed. We can use `nextValue` to take the next
                        // argument as value, no matter how it looks.
                        // Here, the flag also requires a value.
                        const value = long.value orelse (args.nextAsValue() orelse return error.MissingValue);
                        options.long = try alloc.dupeZ(u8, value);
                    } else if (std.mem.eql(u8, long.flag, "also")) {
                        // Here, the flag has an optional value.
                        const value = long.value orelse args.nextAsValue();
                        // If there is a value, we can validate it.
                        // Here, we only allow values that are enum tags.
                        // A missing value will default to the `.auto` option.
                        options.also = if (value) |v|
                            (std.meta.stringToEnum(@TypeOf(options.also), v) orelse return error.UnknownValueForEnumFlag)
                        else
                            .auto;
                    } else {
                        return error.UnknownFlag;
                    }
                },
                // Any plain value that is not a `-` short flag, `--` long flag
                // or the special value `--` (`.escape`).
                // It is up to the user to decide what to do.
                // Those might be a subcommand, a freestanding/positional args,
                // or - depending on how the parsing loop is written - values for
                // a previous short or long option.
                // Here, we don't allow those args.
                .value => return error.UnexpectedValueBeforeEscape,
            }
        }

        // print the result so that we can see something in the example
        std.debug.print(
            \\Parsed options using {s}:
            \\  --a: {}
            \\  --b: {}
            \\  --c: {?s}
            \\  --flag: {}
            \\  --long: {s}
            \\  --also: {s}
            \\  --remainder: {?s}
            \\
        , .{
            @typeName(@TypeOf(args.*)),
            options.a,
            options.b,
            options.c,
            options.flag,
            options.long,
            @tagName(options.also),
            options.remainder,
        });

        // explicit call to test that we don't have values pointing to the args buffer.
        // Usually, this would be done by `defer args.deinit()`.
        args.deinit();

        // validate that we parsed into the same result for any impl
        const expected = MyOptions{
            .a = -42,
            .b = true,
            .c = "def",
            .flag = true,
            .long = "value",
            .also = .yes,
            .remainder = &[_][:0]const u8{ "remaining", "--args" },
        };
        defer {
            if (options.c) |c| alloc.free(c);
            alloc.free(options.long);
            if (options.remainder) |remainder| {
                for (remainder) |value| alloc.free(value);
                alloc.free(remainder);
            }
        }

        try std.testing.expectEqualDeep(expected, options);
    }
}
