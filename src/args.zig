// SPDX-License-Identifier: MIT

pub const GenericArgs = @import("generic.zig").GenericArgs;

/// An args lexer over `argv` of the process.
/// Allocations only happen on Windows and WASI.
/// On POSIX systems, `initPosix` can be used without an `Allocator`.
pub const Args = struct {
    pub const Iter = GenericArgs(std.process.ArgIterator);

    /// Create a new Args lexer using `std.process.argsWithAllocator`.
    /// No other internal allocations are done.
    /// Must deinitialize the iterator buffer using `deinit` when done.
    pub fn init(alloc: std.mem.Allocator) Iter.Inner.InitError!Iter {
        return .{ .iter = try std.process.argsWithAllocator(alloc) };
    }

    /// Create a new Args lexer using `std.process.args`.
    /// The args iterator is not cross-platform and does not work on Windows or WASI.
    /// This makes the lexer completely free of any allocations.
    pub fn initPosix() Iter {
        const native_os = @import("builtin").os.tag;
        if (native_os == .wasi or native_os == .windows) {
            @compileError("Use `init` instead.");
        }

        return .{ .inner = .{ .iter = std.process.args() } };
    }
};

/// An args lexer over a provided string, including basic parsing and quoting.
pub const StringArgs = struct {
    pub const Iter = GenericArgs(std.process.ArgIteratorGeneral(.{}));

    /// Create a new lexer using `std.process.ArgIteratorGeneral.init`,
    /// using the default configuration.
    /// Does not take ownership over `args`.
    pub fn init(alloc: std.mem.Allocator, args: []const u8) Iter.Inner.InitError!Iter {
        return .{ .iter = try Iter.Inner.init(alloc, args) };
    }
};

/// An args lexer over a provided string, including basic parsing and quoting.
/// In addition to `StringArgs`, the parser can be configured to recognize
/// comments or single quoted arguments (').
/// This could be use to parse args from a file.
pub fn GeneralArgs(comptime options: std.process.ArgIteratorGeneralOptions) type {
    return struct {
        pub const Iter = GenericArgs(std.process.ArgIteratorGeneral(options));

        /// Create a new lexer using `std.process.ArgIteratorGeneral.init`,
        /// using the provided configuration.
        /// Does not take ownership over `args`.
        pub fn init(alloc: std.mem.Allocator, args: []const u8) Iter.Inner.InitError!Iter {
            return .{ .iter = try Iter.Inner.init(alloc, args) };
        }
    };
}

/// An args lexer over the provided slice of args.
/// The slice represents the already parsed/quoted arguments.
/// This type can be used with the slice returned by `std.process.argsAlloc`.
/// This lexer can be reset for multi-pass parsing.
pub const SliceArgs = struct {
    pub const SliceIter = struct {
        args: []const [:0]const u8,
        idx: usize = 0,

        pub fn next(self: *SliceIter) ?[:0]const u8 {
            if (self.idx >= self.args.len) return null;
            defer self.idx += 1;
            return self.args[self.idx];
        }

        pub fn reset(self: *SliceIter) void {
            self.idx = 0;
        }
    };

    pub const Iter = GenericArgs(SliceIter);

    /// Create a new lexer using the given args.
    /// Does not take ownership over args.
    pub fn init(args: []const [:0]const u8) Iter {
        return .{ .iter = .{ .args = args } };
    }
};

/// An args lexer over `std.os.argv`.
/// Can only be used on supported OS.
/// Can be reset for multi-pass parsing.
pub const OsArgs = struct {
    const OsIter = struct {
        args: []const [*:0]const u8,
        idx: usize = 0,

        pub fn next(self: *@This()) ?[:0]const u8 {
            if (self.idx >= self.args.len) return null;
            defer self.idx += 1;
            return std.mem.span(self.args[self.idx]);
        }

        pub fn reset(self: *@This()) void {
            self.idx = 0;
        }
    };

    pub const Iter = GenericArgs(OsIter);

    pub fn isSupported() bool {
        const builtin = @import("builtin");
        return builtin.link_libc or switch (builtin.os.tag) {
            .windows, .wasi => false,
            else => true,
        };
    }

    /// Create a new lexer using `std.os.argv`.
    pub fn init() Iter {
        if (comptime isSupported() == false) {
            @compileError("Using `std.os.argv` is not supported on this system.");
        }
        return initFrom(std.os.argv);
    }

    pub fn initFrom(argv: []const [*:0]const u8) Iter {
        return .{ .iter = .{ .args = argv } };
    }
};

test "Force analysis" {
    comptime {
        std.testing.refAllDecls(@This());
    }
}

const std = @import("std");
