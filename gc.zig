const std = @import("std");
const debug = std.debug;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayListUnmanaged;
const CallFrameStack = @import("vm.zig").CallFrameStack;
const Conf = @import("conf.zig");
const Obj = @import("obj.zig");
const Table = @import("table.zig");
const Value = @import("value.zig").Value;
const ValueStack = @import("vm.zig").ValueStack;

// Testing
const assert = std.debug.assert;

// Garbage Collector!!!!
//
const GC = @This();

/// The allocator used by the GC, this should only be used to implement the std.mem.Allocator interface.
inner_allocator: Allocator,

/// The GC represented as an allocator. This is so the GC can be used as an allocator for std library for example ArrayList. This
/// should mostly be used.
this_allocator: Allocator,

init_string: *Obj.String,
obj_list: ?*Obj,
open_upvalues: ?*Obj.Upvalue = null,
interned_strings: Table,
globals: Table,
gray_stack: ArrayList(*Obj),
call_frames: ?*CallFrameStack = null,
stack: ?*ValueStack = null,
bytes_allocated: usize = 0,
next_gc: usize = 1024 * 1024,
enabled: bool = false,

/// Creates a new Garbage Collector.
/// 
pub fn init(gc: *GC, allocator: Allocator) !void {
    gc.inner_allocator = allocator;
    gc.obj_list = null;
    gc.open_upvalues = null;
    gc.call_frames = null;
    gc.stack = null;
    gc.interned_strings = Table.init();
    gc.globals = Table.init();
    gc.gray_stack = try ArrayList(*Obj).initCapacity(allocator, 64);

    gc.this_allocator = gc.map_vtable();
    gc.init_string = try gc.copy_string(@ptrCast("init"), "init".len);
}

/// Prints a list of objects owned by the garbage collector.
/// 
pub fn print_object_list(self: *GC, msg: []const u8) void {
    debug.print("=== Object list {s} ===\n", .{msg});
    var obj = self.obj_list;
    while (obj) |o| {
        o.print(debug);
        debug.print("\n", .{});
        obj = o.next;
    }
    debug.print("=== end Object list ===\n", .{});
}

/// Downcasts to Allocator.
/// 
pub inline fn as_allocator(self: *GC) Allocator {
    return self.this_allocator;
}

/// Maps the Allocator vtable to the Garbage Collector.
/// 
pub fn map_vtable(self: *GC) Allocator {
    return .{ .ptr = self, .vtable = &.{ .alloc = alloc, .resize = resize, .free = free,} };
}

// pub fn patch_allocator(self: *GC) void {
//     self.this_allocator = Allocator.init(self, alloc, resize, free);
// }

// Allocator alloc method.  Triggers collection, maybe.
//
fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
    const self: *GC = @ptrCast(@alignCast(ctx));

    const bytes = self.inner_allocator.rawAlloc(len, ptr_align, ret_addr).?;
    self.bytes_allocated += len;
    self.maybe_collect();
    return bytes;
}

// Allocator resize method.  Triggers collection, maybe.
//
fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_size: usize, ret_addr: usize) bool {
    const self: *GC = @ptrCast(@alignCast(ctx));

    _ = self.inner_allocator.rawResize(buf, buf_align, new_size, ret_addr);
    self.bytes_allocated += new_size - buf.len;
    if (new_size > buf.len) {
        self.maybe_collect(); 
    }
    return true;
}

// Allocator free method.
//
fn free(ctx: *anyopaque,buf: []u8,buf_align: u8,ret_addr: usize,) void {
    const self: *GC = @ptrCast(@alignCast(ctx));

    self.inner_allocator.rawFree(buf, buf_align, ret_addr);
    self.bytes_allocated -= buf.len;
}

// Collects, maybe :)  Checks to see if enabled, and if not enough memory.
//
fn maybe_collect(self: *GC) void {
    if (self.enabled and self.bytes_allocated > self.next_gc) {
        self.collect() catch @panic("GC failed to collect");
    }
}

/// Runs collection.
/// 
pub fn collect(self: *GC) !void {
    if (comptime Conf.DEBUG_LOG_GC) {
        debug.print("-- gc begin\n", .{});
    }

    try self.mark_roots();
    try self.trace_references();
    self.interned_strings.remove_white();
    try self.sweep();

    if (comptime Conf.DEBUG_LOG_GC) {
        debug.print("-- gc end\n", .{});
        var obj = self.obj_list;
        while (obj) |o| {
            debug.assert(!o.is_marked);
            obj = o.next;
        }
    }

    const factor: usize = 2;

    self.next_gc = self.bytes_allocated *| factor;
}

/// Marks the roots.
/// 
pub fn mark_roots(self: *GC) !void {
    {
        var i: usize = 0;
        
        if (self.globals.entries_slice()) |entries| {
            while (i < self.globals.cap) : (i += 1) {
                if (entries[i].key != null) {
                    if (entries[i].val.as_obj()) |obj| {
                        if (obj.is_marked) {
                            debug.print("That's unexpected!", .{});
                            entries[i].val.print(debug);
                            debug.print("\n", .{});
                        }
                    }
                }
            }
        }
    }

    if (self.stack) |values| {
        var slot = @as([*]Value, @ptrCast(&values.stack[0]));
        while (@intFromPtr(slot) < @intFromPtr(values.top)) : (slot += 1) {
            try self.mark_value(slot[0]);
        }
    }
    if (self.call_frames) |call_frames| {
        var i: usize = 0;
        while (i < call_frames.count) : (i += 1) {
            try self.mark_obj(call_frames.stack[i].closure.widen());
        }
    }

    var upvalue: ?*Obj.Upvalue = self.open_upvalues;
    while (upvalue) |upval| : (upvalue = upval.next) {
        try self.mark_obj(upval.widen());
    }

    try self.mark_table(&self.globals);

    try self.mark_obj(self.init_string.widen());
}

/// Traces all accessible references.
/// 
pub fn trace_references(self: *GC) !void {
    while (self.gray_stack.items.len > 0) {
        const obj = self.gray_stack.pop();
        try self.blacken_object(obj);
    }
}

/// Sweeps all owned nodes, freeing up all that are not marked as used.
/// 
pub fn sweep(self: *GC) !void {
    var previous: ?*Obj = null;
    var object: ?*Obj = self.obj_list;

    while (object) |obj| {
        if (obj.is_marked) {
            obj.is_marked = false;
            previous = obj;
            object = obj.next;
        } 
        else {
            const unreached = obj;
            object = obj.next;
            if (previous) |prev| {
               prev.next = object;
            } 
            else {
               self.obj_list = object;
            }

           try self.free_object(unreached);
        }
    }
}

/// Marks Hash Table.
/// 
pub fn mark_table(self: *GC, table: *Table) !void {
    var i: usize = 0;
    while (i < table.cap) : (i += 1) {
        var entry: *Table.Entry = &table.entries.?[i];
        if (entry.key) |key| {
            try self.mark_obj(key.widen());
        }
        try self.mark_value(entry.val);
    }
}

/// Marks a value.
/// 
pub fn mark_value(self: *GC, value: Value) !void {
    switch (value) {
        .Obj => |obj| try self.mark_obj(obj),
        else => {},
    }
}

/// Marks object.
/// 
pub fn mark_obj(self: *GC, maybe_obj: ?*Obj) !void {
    const obj = maybe_obj orelse return;
    if (obj.is_marked) return;

    if (comptime Conf.DEBUG_LOG_GC) {
        std.debug.print("{x} mark ", .{@intFromPtr(self)});
        Value.obj(obj).print(std.debug);
        std.debug.print("\n", .{});
    }

    obj.is_marked = true;
    try self.gray_stack.append(self.as_allocator(), obj);
}

/// Marks an array.
/// 
pub fn mark_array(self: *GC, array: []Value) !void {
    for (array) |value| {
        try self.mark_value(value);
    }
}

/// Sets an object to "black" to mark progress.
/// 
pub fn blacken_object(self: *GC, obj: *Obj) !void {
    if (comptime Conf.DEBUG_LOG_GC) {
        debug.print("{x} blacken ({s}) ", .{ @intFromPtr(self), @tagName(obj.type) });
        Value.obj(obj).print(debug);
        debug.print("\n", .{});
    }

    switch (obj.type) {
        .NativeFunction, .String => {},
        .Upvalue => {
            const upvalue: *Obj.Upvalue = obj.narrow(Obj.Upvalue);
            try self.mark_value(upvalue.closed);
        },
        .Function => {
            const function: *Obj.Function = obj.narrow(Obj.Function);
            if (function.name) |name| {
                try self.mark_obj(name.widen());
            }

            try self.mark_array(function.chunk.constants.items.ptr[0..function.chunk.constants.items.len]);
        },
        
        .Class => {
            const class: *Obj.Class = obj.narrow(Obj.Class);
            try self.mark_table(&class.methods);
            try self.mark_obj(class.name.widen());
        },

        .Instance => {
            const instance: *Obj.Instance = obj.narrow(Obj.Instance);
            try self.mark_obj(instance.class.widen());
            try self.mark_table(&instance.fields);
        },

        .Closure => {
            const closure: *Obj.Closure = obj.narrow(Obj.Closure);
            try self.mark_obj(closure.function.widen());
            var i: usize = 0;
            while (i < closure.upvalues_len) : (i += 1) {
                try self.mark_obj(closure.upvalues[i].widen());
            }
        },

        .BoundMethod => {
            const method: *Obj.BoundMethod = obj.narrow(Obj.BoundMethod);
            try self.mark_value(method.receiver);
            try self.mark_obj(method.method.widen());
        },
    }
}

/// Frees all owned objects.
/// 
pub fn free_objects(self: *GC) !void {
    var obj = self.obj_list;
    while (obj) |next| {
        obj = next.next;
        self.free_object(next) catch @panic("Error freeing objects");
    }
    self.obj_list = null;

    self.interned_strings.free(self.as_allocator());
    self.globals.free(self.as_allocator());
    self.gray_stack.clearAndFree(self.as_allocator());
}

/// Frees an Obj.
/// 
pub fn free_object(self: *GC, obj: *Obj) !void {
    if (comptime Conf.DEBUG_LOG_GC) {
        debug.print("{x} free type {s}: ", .{ @intFromPtr(obj), @tagName(obj.type) });
        obj.print(debug);
        debug.print("\n", .{});
    }

    switch (obj.type) {
        .BoundMethod => {
            self.as_allocator().destroy(obj.narrow(Obj.BoundMethod));
        },

        .String => {
            const string = obj.narrow(Obj.String);
            //self.as_allocator().destroy(string.chars);
            self.as_allocator().destroy(string);
        },

        .Function => {
            const function = obj.narrow(Obj.Function);
            function.chunk.free(self.as_allocator());
            self.as_allocator().destroy(function);
        },

        .NativeFunction => {
            self.as_allocator().destroy(obj.narrow(Obj.NativeFunction));
        },
        
        .Class => {
            // TODO free methods.
            self.as_allocator().destroy(obj.narrow(Obj.Class));
        },

        .Instance => {
            const instance = obj.narrow(Obj.Instance);
            //instance.fields.free(self.as_allocator());
            self.as_allocator().destroy(instance);
        },

        .Closure => {
            const closure = obj.narrow(Obj.Closure);
            self.as_allocator().free(closure.upvalues[0..closure.upvalues_len]);
            self.as_allocator().destroy(closure);
        },

        .Upvalue => {
            self.as_allocator().destroy(obj.narrow(Obj.Upvalue));
        },
    }
}

/// Validates an Obj's "Pun" type, for casting.
/// 
fn validate_obj_pun_type(comptime ParentType: type) void {
    const ty_info = @typeInfo(ParentType).Struct;
    const obj_type = ty_info.fields[0];
    if (!std.mem.eql(u8, obj_type.name, "obj")) {
        @compileError("expected first field to be named 'obj'");
    }
    if (obj_type.type != Obj) {
        @compileError("expected first field to be of type 'Obj'");
    }
    var found_widen = false;
    for (ty_info.decls) |decl| {
        if (std.mem.eql(u8, decl.name, "widen")) {
            found_widen = true;
        }
    }
    if (!found_widen) {
        @compileError("expected punnable obj type to have a 'widen' method");
    }
}

/// Allocates an Obj.
/// 
pub fn alloc_obj(self: *GC, comptime ParentType: type) !*ParentType {
    comptime validate_obj_pun_type(ParentType);
    const ptr = try self.this_allocator.create(ParentType);

    ptr.widen().type = Obj.Type.from_obj(ParentType);
    ptr.widen().is_marked = false;
    ptr.widen().next = self.obj_list;
    self.obj_list = ptr.widen();

    if (comptime Conf.DEBUG_LOG_GC) {
        debug.print("{d} allocate {d} for {s}\n", .{ @intFromPtr(ptr), @sizeOf(ParentType), @tagName(Obj.Type.from_obj(ParentType)) });
    }

    return ptr;
}

/// Allocates a String object.
/// 
pub fn alloc_string(self: *GC, chars: [*]const u8, len: u32, hash: u32) !*Obj.String {
    var ptr = try self.alloc_obj(Obj.String);
    ptr.len = len;
    ptr.chars = chars;
    ptr.hash = hash;

    // Adding to interned strings might trigger GC and free newly allocated string.
    ptr.obj.is_marked = true;
    _ = try self.interned_strings.insert(self.as_allocator(), ptr, Value.nil());
    ptr.obj.is_marked = false;
    return ptr;
}

/// Transfers ownership of string.
/// 
pub fn take_string(self: *GC, chars: [*]const u8, len: u32) !*Obj.String {
    const hash = Table.hash_string(chars, len);
    if (self.interned_strings.find_string(chars, len, hash)) |interned| {
        self.as_allocator().free(chars[0..len]);
        return interned;
    }
    return self.alloc_string(chars, len, hash);
}

/// Creates a copy of a string.
/// 
pub fn copy_string(self: *GC, chars: [*]const u8, len: u32) !*Obj.String {
    const hash = Table.hash_string(chars, len);
    if (self.interned_strings.find_string(chars, len, hash)) |interned| {
        return interned;
    }
    const alloced_chars = try self.as_allocator().alloc(u8, len);
    std.mem.copy(u8, alloced_chars, chars[0..len]);
    return self.alloc_string(@as([*]const u8, @ptrCast(alloced_chars)), len, hash);
}
