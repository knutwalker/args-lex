// SPDX-License-Identifier: MIT

pub const Arg = @import("arg.zig").Arg;
pub const ParseError = Arg.ParseError;
pub const Args = @import("args.zig").Args;
pub const GeneralArgs = @import("args.zig").GeneralArgs;
pub const GenericArgs = @import("args.zig").GenericArgs;
pub const SliceArgs = @import("args.zig").SliceArgs;
pub const StringArgs = @import("args.zig").StringArgs;

test "force analysis" {
    comptime {
        @import("std").testing.refAllDecls(@This());
    }
}
