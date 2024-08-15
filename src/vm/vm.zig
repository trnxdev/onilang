const std = @import("std");

pub const Instruction = @import("instruction.zig");
pub const Value = @import("value.zig");
pub const Object = @import("object.zig");

frames: [64]CallFrame,
frame_idx: usize,
stack: [std.mem.page_size]Value,
stack_i: usize,
globals: [10000]Value,
allocator: std.mem.Allocator,

pub inline fn currentFrame(self: *@This()) *CallFrame {
    return &self.frames[self.frame_idx - 1];
}

pub fn push(self: *@This(), val: Value) !void {
    self.stack[self.stack_i] = val;
    self.stack_i += 1;
    // try self.stack.append(val);
}

pub fn pop(self: *@This()) Value {
    self.stack_i -= 1;
    return self.stack[self.stack_i];
    // return self.stack.pop();
}

pub inline fn allocFrame(self: *@This()) *CallFrame {
    defer self.frame_idx += 1;
    return &self.frames[self.frame_idx];
}

pub fn deallocLastFrame(self: *@This()) void {
    if (self.frame_idx == 0)
        unreachable;

    self.frame_idx -= 1;
}

pub inline fn lastIndex(self: @This()) usize {
    return self.stack_i - 1;
}

pub inline fn getLast(self: @This()) Value {
    return self.at(self.lastIndex());
}

pub inline fn at(self: @This(), index: usize) Value {
    return self.stack[index];
}

pub const Chunk = []const Instruction;
pub const CallFrame = struct {
    function: *Object.Function,
    locals: [1028]Value,
    ip: usize,
    needs_return: bool,
};
