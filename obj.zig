const std = @import("std");
const mem = std.mem;
const debug = std.debug;

const Allocator = mem.Allocator;
const Chunk = @import("chunk.zig").Chunk;
const GC = @import("gc.zig");
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;

// Testing
const Test = @import("test.zig");

const _test = @import("test.zig");
const assertTrue = std.testing.expect;
const assertEqual = std.testing.expectEqual;
const printPass = _test.printPass;
const printStatus = _test.printStatus;
const test_allocator = std.testing.allocator;

pub const Obj = @This();

/// Type for Punning.
/// 
pub const Type = enum (u64) {
    Function,
    NativeFunction,
    Class,
    Instance,
    BoundMethod,
    Closure,
    Upvalue,
    String,

    pub fn obj_struct(comptime self: Type) type {
        return switch (self) {
            Type.String => String,
            Type.Function => Function,
            Type.NativeFunction => NativeFunction,
            Type.Class => Class,
            Type.Instance => Instance,
            Type.BoundMethod => BoundMethod,
            Type.Closure => Closure,
            Type.Upvalue => Upvalue,
        };
    }

    pub fn from_obj(comptime ObjType: type) Type {
        return Type.from_obj_safe(ObjType) orelse @panic("invalid obj type");
    }

    pub fn from_obj_safe(comptime ObjType: type) ?Type {
        return switch (ObjType) {
            String => Type.String,
            Function => Type.Function,
            NativeFunction => Type.NativeFunction,
            Class => Type.Class,
            Instance => Type.Instance,
            BoundMethod => Type.BoundMethod,
            Closure => Type.Closure,
            Upvalue => Type.Upvalue,
            else => @panic("Error casting from Obj"),
        };
    }
};

type: Type,
is_marked: bool,
next: ?*Obj = null,

const SAFE_OBJ_CAST = true;

/// Downcasts to Type.
/// 
pub inline fn narrow(self: *Obj, comptime ParentType: type) *ParentType {
    if (comptime SAFE_OBJ_CAST) {
         return self.safe_narrow(ParentType) orelse @panic("invalid cast");
    }
    return @fieldParentPtr(ParentType, "obj", self);
}

/// Safe downcast to type.
/// 
pub inline fn safe_narrow(self: *Obj, comptime ParentType: type) ?*ParentType {
    if (self.type != Type.from_obj(ParentType)) return null;
    return @fieldParentPtr(ParentType, "obj", self);
}

/// Is a type?
/// 
pub fn is(self: *Obj, comptime ParentType: type) bool {
    return self.type == Type.from_obj(ParentType);
}

/// Prints Obj.
/// 
pub fn print(self: *Obj, writer: anytype) void {
    switch (self.type) {
         inline else => |ty| {
             self.narrow(Type.obj_struct(ty)).print(writer);
         },
    }
}

/// String Object
/// 
pub const String = struct {
    obj: Obj,
    len: u32,
    hash: u32,
    chars: [*]const u8,

    /// Converts to []const u8.  What else can I say?
    /// 
    pub fn as_string(self: *String) []const u8 {
        return self.chars[0..self.len];
    }

    /// Do two strings equal?
    /// 
    pub fn eq(a: *String, b: *String) bool {
        if (a.len != b.len) return false;
        return mem.eql(u8, a.chars[0..a.len], b.chars[0..b.len]);
    }

    /// Upcast to Obj.
    /// 
    pub inline fn widen(self: *String) *Obj {
        return @ptrCast(self);
    }

    /// Print!
    /// 
    pub fn print(self: *String, writer: anytype) void {   
        writer.print("{s}", .{self.chars[0..self.len]});
    }
};

/// Function object.
/// 
pub const Function = struct {
    obj: Obj,
    arity: u8,
    upvalue_count: u32,
    name: ?*String,
    chunk: Chunk,

    /// Initializes a function.
    /// 
    pub fn init(self: *Function, allocator: Allocator) !void {
        self.arity = 0;
        self.name = null;
        self.upvalue_count = 0;
        self.chunk = try Chunk.init(allocator);
    }

    /// Upcast to Obj.
    /// 
    pub inline fn widen(self: *Function) *Obj {
        return @as(*Obj, @ptrCast(self));
    }

    /// Print!
    /// 
    pub fn print(self: *Function, writer: anytype) void {
        const name = if (self.name) |name| name.chars[0..name.len] else return writer.print("<script>", .{});
        writer.print("<fn {s}>", .{name});
    }

    /// Returns the name.
    /// 
    pub fn name_str(self: *Function) []const u8 {
        return if (self.name) |name| name.chars[0..name.len] else "script";
    }
};

/// Native Function.
/// 
pub const NativeFunction = struct {
    obj: Obj,
    function: NativeFn,

    /// A function pointer
    pub const NativeFn = *const fn (u8, []Value) Value;

    /// Initializes a native function.
    /// 
    pub fn init(self: *NativeFunction, function: NativeFn) void {
        self.function = function;
    }

    /// Print!
    /// 
    pub fn print(self: *NativeFunction, writer: anytype) void {
        _ = self;
        writer.print("<native fn>", .{});
    }

    /// Upcast to Obj.
    /// 
    pub fn widen(self: *NativeFunction) *Obj {
        return @as(*Obj, @ptrCast(self));
    }
};

// Closure Object.
//
pub const Closure = struct {
    obj: Obj,
    function: *Function,
    upvalues: [*]*Upvalue,
    upvalues_len: u32,

    /// Initializes a closure.
    /// 
    pub fn init(gc: *GC, function: *Function) !*Closure {
        const upvalues = try gc.as_allocator().alloc(?*Upvalue, function.upvalue_count);
        for (upvalues) |*upvalue| {
             upvalue.* = std.mem.zeroes(?*Upvalue);
        }

        var ptr = try gc.alloc_obj(Obj.Closure);
        ptr.function = function;

        ptr.upvalues = @as([*]*Upvalue, @ptrCast(upvalues));
        ptr.upvalues_len = function.upvalue_count;

        return ptr;
    }

    /// Print!
    /// 
    pub fn print(self: *Closure, writer: anytype) void {
        const name = self.function.name_str();
        writer.print("<closure> {s}", .{name});
    }
    
    /// Upcasts to Obj.
    /// 
    pub fn widen(self: *Closure) *Obj {
        return @as(*Obj, @ptrCast(self));
    }
};

/// Upvalue.  Captures closure values from stack and moves them onto the heap.
///
pub const Upvalue = struct {
    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*Upvalue = null,

    /// Initializes an upvalue.
    /// 
    pub fn init(self: *Upvalue, value: *Value) void {
        self.location = value;
        self.closed = Value.nil();
        self.next = null;
    }

    /// Print!!
    /// 
    pub fn print(self: *Upvalue, writer: anytype) void {
        _ = self;
        writer.print("upvalue", .{});
    }

    /// Upcast to Obj.
    /// 
    pub fn widen(self: *Upvalue) *Obj {
        return @as(*Obj, @ptrCast(self));
    }
};

/// Class Object.
pub const Class = struct {
    obj: Obj,
    name: *String,
    methods: Table,

    /// Initialize a Class.
    /// 
    pub fn init(gc: *GC, name: *String) !*Class {
        var ptr = try gc.alloc_obj(Obj.Class);
        ptr.name = name;
        ptr.methods = Table.init();

        return ptr;
    }

    /// Print!
    /// 
    pub fn print(self: *Class, writer: anytype) void {
        writer.print("class {s}", .{self.name.as_string()});
    }

    /// Upcast to Obj.
    /// 
    pub fn widen(self: *Class) *Obj {
        return @as(*Obj, @ptrCast(self));
    }
};

/// Instance of Class!
/// 
pub const Instance = struct {
    obj: Obj,
    class: *Class,
    fields: Table,

    /// Initializes an Instance.
    /// 
    pub fn init(gc: *GC, class: *Class) !*Instance {
        var ptr = try gc.alloc_obj(Obj.Instance);
        ptr.class = class;
        ptr.fields = Table.init();

        return ptr;
    }

    /// Print!
    /// 
    pub fn print(self: *Instance, writer: anytype) void {
        writer.print("{s} instance", .{self.class.name.as_string()});
    }

    /// Upcast to Obj.
    /// 
    pub fn widen(self: *Instance) *Obj {
        return @as(*Obj, @ptrCast(self));
    }
};


pub const BoundMethod = struct {
    obj: Obj,
    receiver: Value,
    method: *Closure,

    pub fn init(gc: *GC, receiver: Value, method: *Closure) !*BoundMethod {
        var ptr = try gc.alloc_obj(Obj.BoundMethod);
        ptr.receiver = receiver;
        ptr.method = method;

        return ptr;
    }

    pub fn print(self: *BoundMethod, writer: anytype) void {
        self.method.print(writer);
    }

    pub fn widen(self: *BoundMethod) *Obj {
        return @as(*Obj, @ptrCast(self));
    }
};

test "Test Print String Object" {
    debug.print("\n", .{});

    const s: *Obj.String = try Test.alloc_string("ABC", 3);
  
    const obj = s.widen();
    obj.print(debug);
    
    debug.print("\n", .{});
    printStatus("Test Print String Object");
}

test "Test String Object is String" {
    debug.print("\n", .{});

    const s: *Obj.String = try Test.alloc_string("ABC", 3);
    
    const obj = s.widen();
    try assertTrue(obj.is(String));
    
    printStatus("Test String Object is String");
}

test "Test As String" {
    debug.print("\n", .{});

    const s: *Obj.String = try Test.alloc_string("ABC", 3);
    
    debug.print("{s}\n", .{s.as_string()});
    
    printStatus("Test As String");
}

test "Test Strings Equal" {
    debug.print("\n", .{});

    const s: *Obj.String = try Test.alloc_string("ABC", 3);
    const s2: *Obj.String = try Test.alloc_string("ABC", 3);

    try assertTrue(s.eq(s2));
    
    printStatus("Test Strings Equal");
}

test "Test String Upcast" {
    debug.print("\n", .{});

    const s: *Obj.String = try Test.alloc_string("ABC", 3);

    const obj = s.widen();

    try assertEqual(obj.type, Type.String);
    
    printStatus("Test String Upcast");
}
