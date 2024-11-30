<!-- Autogenerated: Edit `README.md.tpl` and run `zig build readme` -->

# args-lex [![CI Status][ci-badge]][ci-url] ![License: MIT][license-badge]

[ci-badge]: https://github.com/knutwalker/args-lex/actions/workflows/checks.yml/badge.svg
[ci-url]: https://github.com/knutwalker/args-lex
[license-badge]: https://img.shields.io/badge/license-MIT-blue.svg?style=shield

A lexer to parse command line arguments

> [!NOTE]
> This is not a full CLI parser.
> A lot of functionality is intentionally missing.

## Features / Parsing capabilities*

- Long arguments: `--flag`
    - values separated by space or `=` (`--flag=value`, `--flag value`)
    - multiple occurrences: `--flag one --flag two`
- Short arguments: `-a`
    - multiple flags grouped together: `-abc`
    - values separated by space or `=` (`-a=value`, `-a value`, `-avalue`)
    - repeated flags: `-vvvv`
- Positional arguments: `arg`
    - escaping remaining arguments after `--`

_*_: Some of those features are not directly implemented by this library.
However, parsers written using the lexer can choose to support those features
and are not locked out of anything.

## Non-Features

- help generation
- arguments to type mapping
- builder API

## Installation

Update to latest version:

```sh
zig fetch --save git+https://github.com/knutwalker/args-lex.git
```

Add to `build.zig`:

```zig
exe.root_module.addImport("args-lex", b.dependency("args-lex", .{}).module("args-lex"));
```

> [!IMPORTANT]
> `args-lex` tracks Zig `0.13.0`

## Examples

### Demo

A simple demo, showing the basics of the API:

```zig
const std = @import("std");

const args_lex = @import("args_lex");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var args = try args_lex.Args.init(arena.allocator());
    defer args.deinit();

    while (args.next()) |arg| {
        switch (arg.*) {
            .escape => while (args.nextValue()) |value| {
                std.debug.print("POSITIONAL: {s}\n", .{value});
            },
            .shorts => |*shorts| while (shorts.next()) |short| switch (short) {
                .flag => |flag| std.debug.print("-{u}\n", .{flag}),
                .suffix => |s| std.debug.print("non utf8 flag: {any}\n", .{s}),
            },
            .long => |long| {
                std.debug.print("--{s}", .{long.flag});
                if (long.value) |v| std.debug.print("={s}", .{v});
                std.debug.print("\n", .{});
            },
            .value => |v| std.debug.print("POSITIONAL: {s}\n", .{v}),
        }
    }
}

```

### Usage

A more detailed example showing more usage patterns:

```zig
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
        _ = args.nextValue().?;

        // Intialize the result type with all defaults.
        // If some args are required, they need to be wrapped in an option
        // and validated after the loop.
        var options: MyOptions = .{};

        // The main lexing loop. `next` will return an `*Arg`.
        // Don't store to pointers without making a copy,
        // every call to `next` will invalidate those pointers.
        // Calling `next` (or any of its variants) will never fail or panic.
        // A `null` inidicated the end of the arguments list.
        while (args.next()) |arg| {
            switch (arg.*) {
                // `arg` is the `--` escape. Usually that means that the remaining
                // args are no longer supposed to be parsed.
                // That decision is up to the user, and we are doing just that.
                .escape => {
                    var remainder = std.ArrayList([:0]const u8).init(alloc);
                    defer remainder.deinit();

                    // `nextValue` will return the next args without any parsing
                    while (args.nextValue()) |arg_value| {
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
                .shorts => |*shorts| {
                    // Command line parsers often do not support negative numbers,
                    // because it is ambiguous. We can choose to look for negative
                    // numbers and treat is as a `value` instead. In this example,
                    // we imply that any negative number must be for the `a` flag.
                    if (shorts.looks_like_number()) {
                        // `value` will return everything *afer* the leading `-`,
                        // so we need to negate the number on our own.
                        options.a = -(try std.fmt.parseInt(i32, shorts.value(), 10));
                        continue;
                    }
                    // Iterate over any short flag. There is no special handling
                    // of `=` done in `next`.
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
                        const value = long.value orelse (args.nextValue() orelse return error.MissingValue);
                        options.long = try alloc.dupeZ(u8, value);
                    } else if (std.mem.eql(u8, long.flag, "also")) {
                        // Here, the flag has an optional value.
                        const value = long.value orelse args.nextValue();
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

```

## License

args-lex is licensed under the [MIT License](http://opensource.org/licenses/MIT)

---

<!-- vim: set ft=markdown: -->
