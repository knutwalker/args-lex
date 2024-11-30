// SPDX-License-Identifier: MIT

pub const Args = @import("args.zig").Args;
pub const SliceArgs = @import("args.zig").SliceArgs;
pub const OsArgs = @import("args.zig").OsArgs;
pub const StringArgs = @import("args.zig").StringArgs;
pub const GeneralArgs = @import("args.zig").GeneralArgs;
pub const GenericArgs = @import("args.zig").GenericArgs;

pub const Arg = @import("arg.zig").Arg;

test "force analysis" {
    comptime {
        @import("std").testing.refAllDecls(@This());
    }
}
