// SPDX-License-Identifier: MIT

/// An args lexer over `argv` of the process.
/// Allocations only happen on Windows and WASI.
/// On POSIX systems, `initPosix` can be used without an `Allocator`.
///
/// See `GenericArgs` for the implementation type and methods.
pub const Args = struct {
    pub const Iter = GenericArgs(std.process.Args.Iterator, true);

    /// Create a new Args lexer using `Args.iterateAllocator`.
    /// No other internal allocations are done.
    /// Must deinitialize the iterator buffer using `deinit` when done.
    pub fn init(args: std.process.Args, gpa: std.mem.Allocator) Iter.Inner.InitError!Iter {
        return .{ .iter = try args.iterateAllocator(gpa) };
    }

    /// Create a new Args lexer using `Args.iterator`.
    /// The args iterator is not cross-platform and does not work on Windows or WASI.
    /// This makes the lexer completely free of any allocations.
    pub fn initPosix(args: std.process.Args) Iter {
        const native_os = @import("builtin").os.tag;
        if (native_os == .wasi or native_os == .windows) {
            @compileError("Use `init` instead.");
        }

        return .{ .iter = args.iterator() };
    }
};

/// An args lexer over a provided string, including basic parsing and quoting.
///
/// See `GenericArgs` for the implementation type and methods.
pub const StringArgs = struct {
    pub const Iter = GenericArgs(std.process.Args.IteratorGeneral(.{}), true);

    /// Create a new lexer using `std.process.Args.IteratorGeneral.init`,
    /// using the default configuration.
    /// Does not take ownership over `args`.
    pub fn init(gpa: std.mem.Allocator, args: []const u8) Iter.Inner.InitError!Iter {
        return .{ .iter = try Iter.Inner.init(gpa, args) };
    }
};

/// An args lexer over a provided string, including basic parsing and quoting.
/// In addition to `StringArgs`, the parser can be configured to recognize
/// comments or single quoted arguments (').
/// This could be use to parse args from a file.
///
/// See `GenericArgs` for the implementation type and methods.
pub fn GeneralArgs(comptime options: std.process.Args.IteratorGeneralOptions) type {
    return struct {
        pub const Iter = GenericArgs(std.process.Args.IteratorGeneral(options), true);

        /// Create a new lexer using `std.process.Args.IteratorGeneral.init`,
        /// using the provided configuration.
        /// Does not take ownership over `args`.
        pub fn init(gpa: std.mem.Allocator, args: []const u8) Iter.Inner.InitError!Iter {
            return .{ .iter = try Iter.Inner.init(gpa, args) };
        }
    };
}

/// An args lexer over the provided slice of args.
/// The slice represents the already parsed/quoted arguments.
/// This type can be used with the slice returned by `std.process.argsAlloc`.
/// This lexer can be reset for multi-pass parsing.
///
/// See `GenericArgs` for the implementation type and methods.
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

    pub const Iter = GenericArgs(SliceIter, true);

    /// Create a new lexer using the given args.
    /// Does not take ownership over args.
    pub fn init(args: []const [:0]const u8) Iter {
        return .{ .iter = .{ .args = args } };
    }
};

test "Force analysis" {
    comptime {
        std.testing.refAllDecls(@This());
    }
}

const std = @import("std");

pub const GenericArgs = @import("generic.zig").GenericArgs;
