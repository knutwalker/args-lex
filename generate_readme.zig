const std = @import("std");
const build_vars = @import("build_vars");

pub fn main() !void {
    var stdout_buf: [4069]u8 = undefined;
    var stdout = std.fs.File.stdout();
    var stdout_writer = stdout.writer(&stdout_buf);

    const readme_tpl = @embedFile("README.md.template");

    try stdout_writer.interface.print(readme_tpl, .{
        .name = build_vars.lib_name,
        .repo = build_vars.repo,
        .demo = read("examples/demo.zig"),
        .usage = read("examples/usage.zig"),
    });

    try stdout_writer.interface.flush();
}

fn read(comptime file: []const u8) []const u8 {
    var content = @as([]const u8, @embedFile(file));
    if (std.mem.startsWith(u8, content, "// SPDX-License-Identifier")) {
        const line_end = std.mem.indexOfScalar(u8, content, '\n').?;
        content = content[line_end + 2 ..];
    }
    return content;
}
