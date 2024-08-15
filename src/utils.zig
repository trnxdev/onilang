const std = @import("std");

pub fn defineClone(comptime clone_type: type) fn (
    self: clone_type,
    allocator: std.mem.Allocator,
) anyerror!*clone_type {
    return struct {
        pub fn definedClone(self: clone_type, allocator: std.mem.Allocator) !*clone_type {
            const clonee = try allocator.create(clone_type);
            clonee.* = self;
            return clonee;
        }
    }.definedClone;
}
