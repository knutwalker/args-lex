// SPDX-License-Identifier: MIT

const std = @import("std");

const Manifest = struct {
    name: @Type(.enum_literal),
    version: []const u8,
    minimum_zig_version: []const u8,
    paths: []const []const u8,
    fingerprint: u64,

    const it: Manifest = @import("build.zig.zon");
    const lib_name = std.mem.trimLeft(u8, @tagName(it.name), ".");
    const repo_name = repo_name: {
        var dashed_name_buf: [lib_name.len]u8 = undefined;
        @memcpy(&dashed_name_buf, lib_name);
        std.mem.replaceScalar(u8, &dashed_name_buf, '_', '-');
        const dashed_name = dashed_name_buf;
        break :repo_name &dashed_name;
    };
};

pub fn build(b: *std.Build) void {
    errdefer |err| switch (err) {
        error.OutOfMemory => std.process.fatal("oom", .{}),
    };

    const check_step = b.step("check", "Check if the project compiles");
    const test_step = b.step("test", "Run unit tests");
    const docs_step = b.step("docs", "Generate docs");
    const readme_step = b.step("readme", "Generate the README");
    const example_step = b.step("example", "Run an example");
    const fmt_step = b.step("fmt", "Run formatting checks");
    const clean_step = b.step("clean", "Clean up");

    const all_step = b.step("all", "Build everything");
    all_step.dependOn(test_step);
    all_step.dependOn(docs_step);
    all_step.dependOn(readme_step);
    all_step.dependOn(example_step);

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule(Manifest.lib_name, .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    // check {{{
    const check = b.addLibrary(.{ .name = "check", .root_module = mod });
    const check_tests = b.addTest(.{ .root_module = mod });

    check_step.dependOn(&check.step);
    check_step.dependOn(&check_tests.step);
    // }}}

    // tests {{{
    const tests = b.addTest(.{ .root_module = mod });
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

    // readme {{{
    const gen_readme_step = readme: {
        const root_path = std.fs.path.relative(b.allocator, b.install_path, b.build_root.path.?) catch
            break :readme @as(?*std.Build.Step.Run, null);

        // build vars {{{
        const build_vars = b.addOptions();
        build_vars.addOption([]const u8, "lib_name", Manifest.lib_name);
        build_vars.addOption([]const u8, "repo", b.fmt("https://github.com/knutwalker/{s}", .{Manifest.repo_name}));
        // }}}

        var gen_readme = b.addExecutable(.{
            .name = "gen_readme",
            .root_module = b.createModule(.{
                .root_source_file = b.path("generate_readme.zig"),
                .optimize = .Debug,
                .target = b.resolveTargetQuery(.{}),
            }),
        });
        gen_readme.root_module.addOptions("build_vars", build_vars);

        const run_gen_readme = b.addRunArtifact(gen_readme);
        run_gen_readme.addFileInput(b.path("README.md.template"));
        const readme_file = run_gen_readme.captureStdOut();

        readme_step.dependOn(&run_gen_readme.step);

        const install_readme_file = b.addInstallFileWithDir(readme_file, .{ .custom = root_path }, "README.md");
        readme_step.dependOn(&install_readme_file.step);

        break :readme run_gen_readme;
    };
    // }}}

    // examples {{{
    const Examples = enum { demo, usage };
    const example_option = b.option([]const Examples, "example", "The example to run") orelse &.{};

    inline for (comptime std.meta.tags(Examples)) |example_tag| {
        const example_name = @tagName(example_tag);
        const example_mod = b.createModule(.{
            .root_source_file = b.path("examples/" ++ example_name ++ ".zig"),
            .target = target,
            .optimize = optimize,
        });
        example_mod.addImport(Manifest.lib_name, mod);

        const example = b.addExecutable(.{
            .name = example_name,
            .root_module = example_mod,
            .single_threaded = true,
        });

        all_step.dependOn(&example.step);
        if (gen_readme_step) |readme| {
            readme.step.dependOn(&example.step);
        }

        if (std.mem.indexOfScalar(Examples, example_option, example_tag) != null) {
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
            example_step.dependOn(&install_example.step);
            example_step.dependOn(&run_example.step);
        }
    }
    // }}}

    // fmt {{{
    const fmt = b.addFmt(.{
        .paths = &.{ "src", "examples", "build.zig", "build.zig.zon" },
        .check = false,
    });
    fmt_step.dependOn(&fmt.step);
    // }}}

    // clean {{{
    const install_path = std.Build.LazyPath{ .cwd_relative = b.getInstallPath(.prefix, "") };
    clean_step.dependOn(&b.addRemoveDirTree(install_path).step);
    if (@import("builtin").os.tag != .windows) {
        clean_step.dependOn(&b.addRemoveDirTree(b.path(".zig-cache")).step);
    }
    // }}}
}

// zig version check {{{
comptime {
    const required_zig = Manifest.it.minimum_zig_version;
    const min_zig = std.SemanticVersion.parse(required_zig) catch unreachable;
    const current_zig = @import("builtin").zig_version;
    if (current_zig.order(min_zig) == .lt) {
        const error_message =
            \\Your zig version is too old :/
            \\
            \\{[name]} requires zig {[zig]s}
            \\Please download a suitable version from
            \\
            \\https://ziglang.org/download/
            \\
        ;

        @compileError(std.fmt.comptimePrint(error_message, .{
            .name = Manifest.lib_name,
            .zig = required_zig,
        }));
    }
}
// }}}
