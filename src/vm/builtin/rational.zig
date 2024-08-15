const std = @import("std");
const VM = @import("../vm.zig");

pub const SelfTag = "Rational";

pub fn create(allocator: std.mem.Allocator) !*VM.Object.Table {
    return try VM.Object.Table.create(allocator, &.{
        .{
            .k = "init",
            .v = (try VM.Object.NativeFunction.create(
                allocator,
                SelfTag ++ ".init",
                &rational_init,
            )).object.asValue(),
        },
    });
}

pub fn rational_init(vm: *VM, args: []const VM.Value) VM.Value {
    if (args.len != 1)
        unreachable;

    const str_float = args[0].asNumber();

    const rational = VM.Object.Rational.create(vm.allocator) catch unreachable;
    rational.setFromFloat(f64, str_float) catch unreachable;

    return rational.object.asValue();
}
