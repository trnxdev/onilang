const std = @import("std");
const Value = @import("value.zig");
const Instruction = @import("instruction.zig");
const VM = @import("vm.zig");

type: Type,

const Object = @This();

pub const Type = enum {
    String,
    Rational,
    Function,
    NativeFunction,
    Array,
    Table,
};

pub fn as(self: *@This(), comptime object_type: Type) *ObjectTypeToType(object_type) {
    return @alignCast(@fieldParentPtr("object", self));
}

pub fn asValue(self: *Object) Value {
    return Value.initObject(self);
}

pub fn ObjectTypeToType(comptime object_type: Object.Type) type {
    comptime return @field(Object, @tagName(object_type));
}

pub fn allocate(allocator: std.mem.Allocator, comptime objType: Type) !*Object {
    const act_type: type = comptime ObjectTypeToType(objType);
    const ptr = try allocator.create(act_type);

    ptr.object = Object{
        .type = objType,
    };

    return &ptr.object;
}

pub fn destroy(self: *@This(), allocator: std.mem.Allocator) void {
    return switch (self.type) {
        inline else => |@"type"| self.as(@"type").free(allocator),
    };
}

pub const String = struct {
    object: Object,
    value: []u8,

    /// Dupes the string
    pub fn create(allocator: std.mem.Allocator, bytes: []const u8) !*String {
        const obj = try Object.allocate(allocator, .String);
        const out = obj.as(.String);

        out.* = String{
            .object = obj.*,
            .value = try allocator.dupe(u8, bytes),
        };

        return out;
    }

    pub fn free(self: *@This(), allocator: std.mem.Allocator) void {
        allocator.free(self.value);
        allocator.destroy(self);
    }
};

pub const Rational = struct {
    object: Object,
    value: std.math.big.Rational,

    pub fn create(allocator: std.mem.Allocator) !*Rational {
        const obj = try Object.allocate(allocator, .Rational);
        const out = obj.as(.Rational);

        out.* = Rational{
            .object = obj.*,
            .value = try std.math.big.Rational.init(allocator),
        };

        return out;
    }

    pub fn setFromInt(self: *@This(), comptime int_type: type, int: int_type) void {
        comptime {
            const type_info = @typeInfo(int_type);

            if (type_info != .Int or type_info != .ComptimeInt)
                @compileError("Specified type is not a an integer type");
        }

        return self.value.setInt(int);
    }

    pub fn setFromFloat(self: *@This(), comptime float_type: type, float: float_type) !void {
        comptime {
            const type_info = @typeInfo(float_type);

            if (type_info != .Float)
                @compileError("Specified type is not a float type");
        }

        const str_flt = try std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{float});
        defer std.heap.page_allocator.free(str_flt);

        // Otherwise it's not precise
        return try self.value.setFloatString(str_flt);
    }

    pub fn free(self: *@This(), allocator: std.mem.Allocator) void {
        self.value.deinit();
        allocator.destroy(self);
    }
};

pub const Function = struct {
    object: Object,
    is_root: bool,
    name: ?*Object.String,
    arity: usize,
    chunk: []const Instruction,
    constants: []Value,

    pub fn create(allocator: std.mem.Allocator, maybe_func_name: ?[]const u8, arity: usize, chunk: []const Instruction, constants: []Value, is_root: bool) !*Function {
        const obj = try Object.allocate(allocator, .Function);
        const out = obj.as(.Function);

        const func_name: ?*String = if (maybe_func_name) |given_func_name|
            try String.create(allocator, given_func_name)
        else
            null;

        out.* = Function{
            .name = func_name,
            .object = obj.*,
            .arity = arity,
            .is_root = is_root,
            .chunk = chunk,
            .constants = constants,
        };

        return out;
    }

    pub fn setFromInt(self: *@This(), comptime int_type: type, int: int_type) void {
        comptime {
            const type_info = @typeInfo(int_type);

            if (type_info != .Int or type_info != .ComptimeInt)
                @compileError("Specified type is not a an integer type");
        }

        return self.value.setInt(int);
    }

    pub fn setFromFloat(self: *@This(), comptime float_type: type, float: float_type) void {
        comptime {
            const type_info = @typeInfo(float_type);

            if (type_info != .Float)
                @compileError("Specified type is not a float type");
        }

        return self.value.setFloat(float_type, float);
    }

    pub fn free(self: *@This(), allocator: std.mem.Allocator) void {
        self.value.deinit();
        allocator.destroy(self);
    }
};

pub const NativeFunction = struct {
    object: Object,
    name: *Object.String,
    ptr: usize,

    pub const ZigNativeFunc = *const fn (vm: *VM, args: []const Value) Value;

    pub fn create(allocator: std.mem.Allocator, name: []const u8, zig_native_func: ZigNativeFunc) !*NativeFunction {
        const obj = try Object.allocate(allocator, .NativeFunction);
        const out = obj.as(.NativeFunction);

        const func_name: *String = try String.create(allocator, name);

        out.* = NativeFunction{
            .name = func_name,
            .object = obj.*,
            .ptr = @intFromPtr(zig_native_func),
        };

        return out;
    }

    pub fn setFromInt(self: *@This(), comptime int_type: type, int: int_type) void {
        comptime {
            const type_info = @typeInfo(int_type);

            if (type_info != .Int or type_info != .ComptimeInt)
                @compileError("Specified type is not a an integer type");
        }

        return self.value.setInt(int);
    }

    pub fn setFromFloat(self: *@This(), comptime float_type: type, float: float_type) void {
        comptime {
            const type_info = @typeInfo(float_type);

            if (type_info != .Float)
                @compileError("Specified type is not a float type");
        }

        return self.value.setFloat(float_type, float);
    }

    pub fn free(self: *@This(), allocator: std.mem.Allocator) void {
        self.value.deinit();
        allocator.destroy(self);
    }
};

pub const Array = struct {
    object: Object,
    values: []Value,

    pub fn create(allocator: std.mem.Allocator, values: []Value) !*Array {
        const obj = try Object.allocate(allocator, .Array);
        const out = obj.as(.Array);

        out.* = Array{
            .object = obj.*,
            .values = values,
        };

        return out;
    }

    pub fn at(self: *@This(), index: f64) Value {
        std.debug.assert(index >= 0);
        const usize_index: usize = @intFromFloat(index);
        std.debug.assert(usize_index < self.values.len);
        return self.values[usize_index];
    }

    pub fn setFromInt(self: *@This(), comptime int_type: type, int: int_type) void {
        comptime {
            const type_info = @typeInfo(int_type);

            if (type_info != .Int or type_info != .ComptimeInt)
                @compileError("Specified type is not a an integer type");
        }

        return self.value.setInt(int);
    }

    pub fn setFromFloat(self: *@This(), comptime float_type: type, float: float_type) void {
        comptime {
            const type_info = @typeInfo(float_type);

            if (type_info != .Float)
                @compileError("Specified type is not a float type");
        }

        return self.value.setFloat(float_type, float);
    }

    pub fn free(self: *@This(), allocator: std.mem.Allocator) void {
        self.value.deinit();
        allocator.destroy(self);
    }
};

pub const Table = struct {
    object: Object,
    values: std.StringHashMap(Value),

    const KeyValue = struct {
        k: []const u8,
        v: Value,
    };

    pub fn create(allocator: std.mem.Allocator, values: []const KeyValue) !*Table {
        const obj = try Object.allocate(allocator, .Table);
        const out = obj.as(.Table);

        var map = std.StringHashMap(Value).init(allocator);
        errdefer map.deinit();

        for (values) |kv| {
            try map.put(kv.k, kv.v);
        }

        out.* = Table{
            .object = obj.*,
            .values = map,
        };

        return out;
    }

    pub fn free(self: *@This(), allocator: std.mem.Allocator) void {
        self.values.deinit();
        allocator.destroy(self);
    }
};
