// SPDX-License-Identifier: MIT

const std = @import("std");

const args_lex = @import("args_lex");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var args = try args_lex.Args.init(arena.allocator());
    defer args.deinit();

    while (args.next()) |arg| {
        switch (arg.*) {
            .escape => while (args.nextAsValue()) |value| {
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
