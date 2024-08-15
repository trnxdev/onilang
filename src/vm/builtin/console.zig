const std = @import("std");
const VM = @import("../vm.zig");

pub const SelfTag = "console";

pub fn create(allocator: std.mem.Allocator) !*VM.Object.Table {
    return try VM.Object.Table.create(allocator, &.{
        .{
            .k = "println",
            .v = (try VM.Object.NativeFunction.create(
                allocator,
                SelfTag ++ ".println",
                &console_println,
            )).object.asValue(),
        },
    });
}

pub fn console_println(vm: *VM, args: []const VM.Value) VM.Value {
    _ = vm;

    for (args) |arg| {
        raw_print(arg);
        std.io.getStdOut().writeAll(" ") catch unreachable;
    }

    std.io.getStdOut().writeAll("\n") catch unreachable;

    return VM.Value.initNil();
}

fn raw_print(v: VM.Value) void {
    const stdout = std.io.getStdOut().writer();

    if (v.isNumber())
        return stdout.print("{d}", .{v.asNumber()}) catch unreachable;

    if (v.isBool())
        return stdout.print("{s}", .{if (v.asBool()) "true" else "false"}) catch unreachable;

    if (v.isNil())
        return stdout.writeAll("nil") catch unreachable;

    if (v.isObjectOfType(.String))
        return stdout.print("{s}", .{v.asObjectOfType(.String).value}) catch unreachable;

    if (v.isObjectOfType(.Function))
        return stdout.print("<function {s}>", .{if (v.asObjectOfType(.Function).name) |name| name.value else "anonymous"}) catch unreachable;

    if (v.isObjectOfType(.NativeFunction))
        return stdout.print("<function {s}:{d}>", .{ v.asObjectOfType(.NativeFunction).name.value, v.asObjectOfType(.NativeFunction).ptr }) catch unreachable;

    if (v.isObjectOfType(.Rational))
        // TODO: Fix 0.1 - 2 panicing (unexpected bits in result)
        return stdout.print("{d}", .{v.asObjectOfType(.Rational).value.toFloat(f64) catch unreachable}) catch unreachable;

    if (v.isObjectOfType(.Array)) {
        stdout.writeAll("[") catch unreachable;
        const values = v.asObjectOfType(.Array).values;
        for (values, 0..) |value, idx| {
            raw_print(value);

            if (idx != values.len -| 1) {
                stdout.writeAll(", ") catch unreachable;
            }
        }
        stdout.writeAll("]") catch unreachable;

        return;
    }

    unreachable;
}
