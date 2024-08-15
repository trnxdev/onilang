/// Nan-Boxed
/// https://github.com/jwmerrill/zig-lox/blob/main/src/value.zig
const std = @import("std");
const Object = @import("object.zig");

data: u64,

const Value = @This();

const SIGN_BIT: u64 = 0x8000000000000000;
const QNAN: u64 = 0x7ffc000000000000;

const TAG_NIL = 1; // 01.
const TAG_FALSE = 2; // 10.
const TAG_TRUE = 3; // 11.

const NIL_VAL = Value{ .data = QNAN | TAG_NIL };
const TRUE_VAL = Value{ .data = QNAN | TAG_TRUE };
const FALSE_VAL = Value{ .data = QNAN | TAG_FALSE };

pub fn isBool(self: Value) bool {
    return (self.data & FALSE_VAL.data) == FALSE_VAL.data;
}

pub fn isNil(self: Value) bool {
    return self.data == NIL_VAL.data;
}

pub fn isNumber(self: Value) bool {
    return (self.data & QNAN) != QNAN;
}

pub fn isObject(self: Value) bool {
    return (self.data & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
}

pub fn asNumber(self: Value) f64 {
    std.debug.assert(self.isNumber());
    return @bitCast(self.data);
}

pub fn asNumberCast(self: Value) f64 {
    if (self.isNumber())
        return self.asNumber();

    if (self.isObjectOfType(.Rational))
        return self.asObjectOfType(.Rational).value.toFloat(f64) catch unreachable;

    unreachable;
}

pub fn asBool(self: Value) bool {
    std.debug.assert(self.isBool());
    return self.data == TRUE_VAL.data;
}

pub fn asObject(self: Value) *Object {
    std.debug.assert(self.isObject());
    return @ptrFromInt(self.data & ~(SIGN_BIT | QNAN));
}

pub fn initNumber(x: f64) Value {
    return Value{ .data = @bitCast(x) };
}

pub fn initBool(x: bool) Value {
    return if (x) TRUE_VAL else FALSE_VAL;
}

pub fn initObject(x: *Object) Value {
    return Value{ .data = SIGN_BIT | QNAN | @intFromPtr(x) };
}

pub fn initNil() Value {
    return NIL_VAL;
}

pub fn isObjectOfType(self: @This(), object_type: Object.Type) bool {
    return self.isObject() and self.asObject().type == object_type;
}

pub fn asObjectOfType(self: @This(), comptime object_type: Object.Type) *Object.ObjectTypeToType(object_type) {
    std.debug.assert(self.isObjectOfType(object_type));
    const object = self.asObject();
    return object.as(object_type);
}
