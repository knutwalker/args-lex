// SPDX-License-Identifier: MIT

const std = @import("std");

pub fn build(b: *std.Build) void {
    const check_step = b.step("check", "Check if the project compiles");
    const test_step = b.step("test", "Run unit tests");
    const docs_step = b.step("docs", "Generate docs");
    const example_step = b.step("example", "Run an example");
    var readme_step = b.step("readme", "Generate the README");

    const all_step = b.step("all", "Build everything");
    all_step.dependOn(test_step);
    all_step.dependOn(example_step);
    all_step.dependOn(readme_step);
    b.default_step.dependOn(all_step);

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("args-lex", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    // check {{{
    const host_target = b.resolveTargetQuery(.{});
    const check = b.addStaticLibrary(.{
        .name = "args-lex-check",
        .root_source_file = b.path("src/root.zig"),
        .target = host_target,
        .optimize = .Debug,
    });

    const check_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = host_target,
        .optimize = .Debug,
    });

    check_step.dependOn(&check.step);
    check_step.dependOn(&check_tests.step);
    // }}}

    // tests {{{
    const tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_tests = b.addRunArtifact(tests);
    test_step.dependOn(&run_tests.step);
    // }}}

    // docs {{{
    const install_docs = b.addInstallDirectory(.{
        .source_dir = tests.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });
    docs_step.dependOn(&install_docs.step);
    // }}}

    // examples {{{
    const Examples = enum { demo, usage };
    const example_option = b.option(Examples, "example", "The example to run");

    inline for (comptime std.meta.tags(Examples)) |example_tag| {
        const example_name = @tagName(example_tag);
        const example = b.addExecutable(.{
            .name = example_name,
            .root_source_file = b.path("examples/" ++ example_name ++ ".zig"),
            .target = target,
            .optimize = optimize,
            .single_threaded = true,
        });
        example.root_module.addImport("args-lex", mod);

        const install_example = b.addInstallArtifact(example, .{});

        const run_example = b.addRunArtifact(example);
        switch (example_tag) {
            .usage => {
                run_example.addArgs(&.{
                    "-a",     "-42", "-bcdef", "--flag",    "--long=value",
                    "--also", "yes", "--",     "remaining", "--args",
                });
            },
            .demo => {
                if (b.args) |args| {
                    run_example.addArgs(args);
                }
            },
        }

        example_step.dependOn(&example.step);
        example_step.dependOn(&install_example.step);
        if (example_option) |selected| {
            if (selected == example_tag) {
                example_step.dependOn(&run_example.step);
            }
        }
    }
    // }}}

    // readme {{{
    readme_step.id = .custom;
    readme_step.makeFn = struct {
        fn read(comptime file: []const u8) []const u8 {
            var content = @as([]const u8, @embedFile(file));
            if (std.mem.startsWith(u8, content, "// SPDX-License-Identifier")) {
                const line_end = std.mem.indexOfScalar(u8, content, '\n').?;
                content = content[line_end + 2 ..];
            }
            return content;
        }

        fn make(_: *std.Build.Step, _: std.Build.Step.MakeOptions) anyerror!void {
            var readme_file = try std.fs.cwd().createFile("README.md", .{});
            try readme_file.writer().print(@embedFile("README.md.template"), .{
                .name = "args-lex",
                .repo = "https://github.com/knutwalker/args-lex",
                .demo = read("examples/demo.zig"),
                .usage = read("examples/usage.zig"),
            });
        }
    }.make;
    readme_step.dependOn(example_step);
    // }}}
}
