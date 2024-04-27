const std = @import("std");

const Obj = @import("obj.zig");

// Testing
const _test = @import("test.zig");
const assertTrue = std.testing.expect;
const assertEqual = std.testing.expectEqual;
const printPass = _test.printPass;
const printStatus = _test.printStatus;


pub const Type = enum { Bool, Nil, Number, Obj };

/// Value
/// 
pub const Value = union(Type) {
    Nil: void,
    Bool: bool,
    Number: f64,
    Obj: *Obj,

    /// Prints the value.
    /// 
    pub fn print(self: *const Value, writer: anytype) void {
        switch (self.*) {
            .Obj => self.Obj.print(writer),
            .Nil    => writer.print("nil", .{}),
            .Bool   =>  writer.print("{s}", .{if (self.Bool) "true" else "false"}),
            .Number => writer.print("{d}", .{self.Number}),
        }
    }

    /// Do two values equal?
    /// 
    pub fn eq(a: Value, b: Value) bool {
        if (@as(Type, a) != @as(Type, b)) return false;

        return switch (@as(Type, a)) {
            .Nil => true,
            .Bool => a.Bool == b.Bool,
            .Number => a.Number == b.Number,
            .Obj => {
                const astr = a.Obj.narrow(Obj.String);
                const bstr = b.Obj.narrow(Obj.String);
                return Obj.String.eq(astr, bstr);
            },
        };
    }

    /// Is value falsey?  
    /// 
    pub inline fn is_falsey(self: Value) bool {
        return self.as_nil() != null or (self.as_boolean().? == false);
    }

    /// Create boolean Value.
    /// 
    pub inline fn boolean(val: bool) Value {
        return Value{ .Bool = val };
    }

    /// Create nil value.
    /// 
    pub inline fn nil() Value {
        return Value.Nil;
    }

    /// Create number value.
    /// 
    pub inline fn number(val: f64) Value {
        return Value{ .Number = val };
    }

    /// Return value as a boolean.
    /// 
    pub fn as_boolean(self: Value) ?bool {
        switch (self) {
            .Bool => |val| return val,
            else => return null,
        }
    }

    /// Return value as a nil.
    /// 
    pub inline fn as_nil(self: Value) ?void {
        switch (self) {
            .Nil => return,
            else => return null,
        }
    }

    /// Return value as a number.
    pub fn as_number(self: Value) ?f64 {
        switch (self) {
            .Number => |val| return val,
            else => return null,
        }
    }

    pub inline fn obj(o: *Obj) Value {
        return Value{ .Obj = o };
    }

    pub inline fn as_obj(self: Value) ?*Obj {
        switch (self) {
            .Obj => |val| return val,
            else => return null,
        }
    }

    pub inline fn as_obj_narrowed(self: Value, comptime ParentType: type) ?*ParentType {
        if (self.as_obj()) |object| {
            return object.safe_narrow(ParentType);
        } 

        return null;
    }
};

test "Test number values - positive" {
    const value = Value.number(3.14);

    const expected: f64 = 3.14;
    try assertEqual(expected, value.as_number().?);

    printPass("Test number values - positive");
}

test "Test number values - negative" {
    const value = Value.number(-3.14);

    const expected: f64 = -3.14;
    try assertEqual(expected, value.as_number().?);

    printPass("Test number values - negative");
}

test "Test numbers equal" {
    const value = Value.number(316);

    try assertEqual(Value.number(316), value);

    printPass("Test numbers equal");
}

test "Test boolean values - true" {
    const value = Value.boolean(true);

    try assertEqual(true, value.as_boolean().?);

    printPass("Test boolean values - true");
}

test "Test boolean values - false" {
    const value = Value.boolean(false);

    try assertEqual(false, value.as_boolean().?);

    printPass("Test boolean values - false");
}

test "Test booleans equal" {
    const value = Value.boolean(false);

    try assertEqual(Value.boolean(false), value);

    printPass("Test booleans equal");
}

test "Test nil equal" {
    const value = Value.nil;

    try assertEqual(Value.nil, value);

    printPass("Test nil equal");
}

test "Test nil is falsey" {
    try assertTrue(Value.nil().is_falsey());

    printPass("Test nil is falsey");
}
