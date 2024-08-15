const std = @import("std");
const Instruction = @import("instruction.zig");
const Value = @import("value.zig");
const Object = @import("object.zig");
const VM = @import("vm.zig");

pub fn execute(allocator: std.mem.Allocator, root_func: *Object.Function) !void {
    var vm: VM = .{
        .frames = undefined,
        .frame_idx = 0,
        .stack = undefined,
        .stack_i = 0,
        .globals = undefined,
        .allocator = allocator,
    };

    vm.allocFrame().* = .{
        .function = root_func,
        .ip = 0,
        .locals = undefined,
        .needs_return = false,
    };

    for (root_func.chunk, 0..) |instr, idx| {
        std.debug.print("{d}: {}\n", .{ idx, instr });
    }

    //  defer for (vm.currentFrame().locals) |local| {
    //      if (local.isObject()) {
    //          local.asObject().destroy(allocator);
    //      }
    //  };

    o: while (true) {
        if (vm.currentFrame().ip >= vm.currentFrame().function.chunk.len)
            unreachable; // Program should exit with .Exit instruction

        const instr = vm.currentFrame().function.chunk[vm.currentFrame().ip];

        switch (instr.kind) {
            .LoadConst => try vm.push(vm.currentFrame().function.constants[instr.A]),
            .UnaryMinus => {
                const to_min = vm.pop();

                if (to_min.isObjectOfType(.Rational)) {
                    const rat = to_min.asObjectOfType(.Rational);
                    rat.value.negate();
                    try vm.push(to_min);
                } else {
                    try vm.push(Value.initNumber(-to_min.asNumber()));
                }
            },
            .Add => {
                const b = vm.pop();
                const a = vm.pop();

                if (a.isObjectOfType(.Rational) and b.isObjectOfType(.Rational)) {
                    const dest = try Object.Rational.create(allocator);
                    try dest.value.add(a.asObjectOfType(.Rational).value, b.asObjectOfType(.Rational).value);
                    try vm.push(dest.object.asValue());
                } else {
                    try vm.push(Value.initNumber(a.asNumberCast() + b.asNumberCast()));
                }
            },
            .Sub => {
                const b = vm.pop();
                const a = vm.pop();

                if (a.isObjectOfType(.Rational) and b.isObjectOfType(.Rational)) {
                    const dest = try Object.Rational.create(allocator);
                    try dest.value.sub(a.asObjectOfType(.Rational).value, b.asObjectOfType(.Rational).value);
                    try vm.push(dest.object.asValue());
                } else {
                    try vm.push(Value.initNumber(a.asNumberCast() - b.asNumberCast()));
                }
            },
            .Mul => {
                const b = vm.pop();
                const a = vm.pop();

                if (a.isObjectOfType(.Rational)) {
                    const dest = try Object.Rational.create(allocator);
                    try dest.value.mul(a.asObjectOfType(.Rational).value, b.asObjectOfType(.Rational).value);
                    try vm.push(dest.object.asValue());
                } else {
                    try vm.push(Value.initNumber(a.asNumberCast() * b.asNumberCast()));
                }
            },
            .Div => {
                const b = vm.pop();
                const a = vm.pop();

                if (a.isObjectOfType(.Rational)) {
                    const dest = try Object.Rational.create(allocator);
                    try dest.value.div(a.asObjectOfType(.Rational).value, b.asObjectOfType(.Rational).value);
                    try vm.push(dest.object.asValue());
                } else {
                    try vm.push(Value.initNumber(a.asNumberCast() / b.asNumberCast()));
                }
            },
            .Lte => {
                const b = vm.pop();
                const a = vm.pop();
                try vm.push(Value.initBool(a.asNumberCast() <= b.asNumberCast()));
            },
            .Gte => {
                const b = vm.pop();
                const a = vm.pop();
                try vm.push(Value.initBool(a.asNumberCast() >= b.asNumberCast()));
            },
            .Lt => {
                const b = vm.pop();
                const a = vm.pop();
                try vm.push(Value.initBool(a.asNumberCast() < b.asNumberCast()));
            },
            .Gt => {
                const b = vm.pop();
                const a = vm.pop();
                try vm.push(Value.initBool(a.asNumberCast() > b.asNumberCast()));
            },
            .Neql => {
                const b = vm.pop();
                const a = vm.pop();
                try vm.push(Value.initBool(a.asNumberCast() != b.asNumberCast()));
            },
            .SetLocal => vm.currentFrame().locals[instr.A] = vm.pop(),
            .GetLocal => try vm.push(vm.currentFrame().locals[instr.A]),
            .SetGlobal => vm.globals[instr.A] = vm.pop(),
            .GetGlobal => try vm.push(vm.globals[instr.A]),
            .Return => {
                if (vm.frame_idx == 0)
                    @panic("Use Exit!");

                if (vm.currentFrame().needs_return) {
                    if (instr.A == 0) {
                        try vm.push(Value.initNil());
                    }
                }

                vm.deallocLastFrame();
            },
            .Jump => {
                vm.currentFrame().ip = instr.A;
                continue :o;
            },
            .Jump_True => {
                if (vm.pop().asBool()) {
                    vm.currentFrame().ip = instr.A;
                    continue :o;
                }
            },
            .Jump_False => {
                if (!vm.pop().asBool()) {
                    vm.currentFrame().ip = instr.A;
                    continue :o;
                }
            },
            .Negate => {
                vm.stack[vm.lastIndex()] = Value.initBool(!vm.getLast().asBool());
            },
            .Call => {
                const popd = vm.pop();

                if (popd.isObjectOfType(.Function)) {
                    const function = popd.asObjectOfType(.Function);

                    vm.allocFrame().* = .{
                        .function = function,
                        .ip = 0,
                        .locals = undefined,
                        .needs_return = instr.B == 1,
                    };

                    for (0..function.arity) |idx| {
                        vm.currentFrame().locals[idx] = vm.pop();
                    }
                    continue :o;
                } else if (popd.isObjectOfType(.NativeFunction)) {
                    const native_function = popd.asObjectOfType(.NativeFunction);
                    const zignvfn: Object.NativeFunction.ZigNativeFunc = @ptrFromInt(native_function.ptr);

                    var args = std.ArrayList(Value).init(allocator);
                    defer args.deinit();

                    for (0..instr.A) |_| {
                        try args.append(vm.pop());
                    }

                    std.mem.reverse(Value, args.items);

                    const call = zignvfn(&vm, try args.toOwnedSlice());

                    if (instr.B == 1) {
                        try vm.push(call);
                    }
                } else {
                    return error.NotAFunction;
                }
            },
            .Access => {
                const field = vm.pop();
                const prefix = vm.pop();

                if (prefix.isObjectOfType(.Array)) {
                    const value = prefix.asObjectOfType(.Array).at(field.asNumber());
                    try vm.push(value);
                } else {
                    const table = prefix.asObjectOfType(.Table);
                    try vm.push(table.values.get(field.asObjectOfType(.String).value) orelse Value.initNil());
                }
            },
            .MakeArray => {
                var values = std.ArrayList(Value).init(allocator);
                defer values.deinit();

                for (0..instr.A) |_| {
                    try values.append(vm.pop());
                }

                std.mem.reverse(Value, values.items);

                // Owns the memory now
                const array = try Object.Array.create(allocator, try values.toOwnedSlice());
                try vm.push(array.object.asValue());
            },
            .Exit => break :o,
        }

        vm.currentFrame().ip += 1;
        continue :o;
    }

    std.debug.print("== Stack\n", .{});

    for (vm.stack[0..1], 0..) |stack_item, idx| {
        std.debug.print("  {d}: {}\n", .{ idx, stack_item });
    }
}
