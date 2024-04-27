
const std = @import("std");
const debug = std.debug;
const mem = std.mem;

const _chunk = @import("chunk.zig");

const native_fns = @import("native_fns.zig");

const Allocator = std.mem.Allocator;
const Chunk = _chunk.Chunk;
const Compiler = @import("compiler.zig");
const FunctionType = @import("compiler.zig").FunctionType;
const GC = @import("gc.zig");
const Obj = @import("obj.zig").Obj;
const Opcode = _chunk.Opcode;
const Scanner = @import("scanner.zig").Scanner;
const String = @import("obj.zig").String;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;

// Testing
const Test = @import("test.zig");
const assertTrue = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const printPass = @import("test.zig").printPass;
const printStatus = @import("test.zig").printStatus;

const DEBUG_TRACE_EXECUTION = @import("conf.zig").DEBUG_TRACE_EXECUTION;

const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * std.math.maxInt(u8);

/// Call Frame!
/// 
const CallFrame = struct {
    closure: *Obj.Closure,
    ip: [*]u8,
    slots: [*]Value,

    /// Reads a single byte from bytecode.
    /// 
    pub inline fn read_byte(self: *CallFrame) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    /// Reads a byte and looks up constant.
    /// 
    pub inline fn read_constant(self: *CallFrame) Value {
        const byte = self.read_byte();
        return self.closure.function.chunk.constants.items[byte];
    }

    /// Reads a constant and downcasts to String.
    /// 
    pub inline fn read_string(self: *CallFrame) *Obj.String {
        return self.read_constant().as_obj().?.narrow(Obj.String);
    }

    /// Reads a short.
    /// 
    pub inline fn read_u16(self: *CallFrame) u16 {
        const byte1 = self.read_byte();
        const byte2 = self.read_byte();
        return (@as(u16, @intCast(byte1)) << 8) | @as(u16, @intCast(byte2));
    }
};

/// Call Frame Stack!
/// 
pub const CallFrameStack = struct {
    stack: [FRAMES_MAX]CallFrame,
    count: u32 = 0,
};

/// Stack of Values!
/// 
pub const ValueStack = struct {
    stack: [STACK_MAX]Value,
    top: [*]Value,

    /// Push.
    /// 
    pub inline fn push(self: *ValueStack, value: Value) void {
        self.top[0] = value;
        self.top += 1;
    }

    /// Pop.
    /// 
    pub inline fn pop(self: *ValueStack) Value {
        self.top -= 1;
        return (self.top)[0];
    }
};

/// VM singleton.
/// 
const Self = @This();
pub var VM: Self = .{
    .call_frames = undefined,
    .values = undefined,
    .gc = undefined,
};

/// Gets singleton VM.
/// 
pub fn get_vm() *Self {
    return &VM;
}

/// Result of interpret.
/// 
pub const InterpretResult = enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
};

// Fields
//
gc: *GC,
call_frames: CallFrameStack,
values: ValueStack,

/// Initializes the Compiler.
/// 
pub fn init(self: *Self, gc: *GC, closure: *Obj.Closure) !void {
    self.values.top = self.values.stack[0..];
    self.gc = gc;
    try self.define_native("clock", native_fns.clock);

    self.values.stack[0] = Value.obj(closure.widen());
    self.call_frames.stack[0] = .{
        .closure = closure,
        .ip = closure.function.chunk.code.items.ptr,
        .slots = self.values.stack[0..],
    };
    self.values.top = self.values.stack[1..];
    self.call_frames.count = 1;
    self.gc.call_frames = &self.call_frames;
    self.gc.stack = &self.values;
}

/// Frees memory from VM.
/// 
pub fn free(self: *Self) !void {
    try self.gc.free_objects();
}

/// Reset the VM's stack.
/// 
pub fn reset_stack(self: *Self) void {
    self.values.top = self.values.stack[0..];
    self.call_frames.count = 0;
    self.gc.open_upvalues = null;
}

/// Peeks into stack at given distance.
/// 
pub inline fn peek(self: *Self, distance: usize) Value {
    return (self.values.top - 1 - distance)[0];
}

/// Calls a function.
/// 
pub fn call(self: *Self, closure: *Obj.Closure, arg_count: u8) bool {
    if (arg_count != closure.function.arity) {
        // TODO: stack trace!
        self.runtime_error_fmt("Expected {d} arguments but got {d}\n", .{ closure.function.arity, arg_count });
        return false;
    }

    if (self.call_frames.count == FRAMES_MAX) {
        self.runtime_error("Stack overflow.");
        return false;
    }

    var frame = &self.call_frames.stack[self.call_frames.count];
    self.call_frames.count += 1;
    frame.closure = closure;
    frame.ip = closure.function.chunk.code.items.ptr;
    frame.slots = self.values.top - arg_count - 1;
    return true;
}

/// Calls a value (function, native function).
/// 
pub fn call_value(self: *Self, callee: Value, arg_count: u8) !bool {
    if (callee.as_obj()) |obj| {
        switch (obj.type) {     
            .BoundMethod => {
                const bound: *Obj.BoundMethod = obj.narrow(Obj.BoundMethod);
                (self.values.top - arg_count - 1)[0] = bound.receiver;
                return self.call(bound.method, arg_count);
            },

            .NativeFunction => {
                const native = obj.narrow(Obj.NativeFunction);
                const result = native.function(arg_count, (self.values.top - arg_count)[0..arg_count]);
                self.values.top -= arg_count + 1;
                self.push(result);
                return true;
            },

            .Class => {
                const class: *Obj.Class = obj.narrow(Obj.Class);
                const instance = Obj.Instance.init(self.gc, class) catch @panic("Error creating Instance!");
                (self.values.top - arg_count - 1)[0] = Value.obj(instance.widen());

                if (class.methods.get(self.gc.init_string)) |initializer| {
                    const closure = initializer.as_obj_narrowed(Obj.Closure).?;
                    return self.call(closure, arg_count);
                } 
                else if (arg_count != 0) {
                    self.runtime_error_fmt("Expected 0 arguments but got {d}.", .{arg_count});
                    return false;
                }

                return true;
            },

            .Closure => {
                const closure = obj.narrow(Obj.Closure);
                return self.call(closure, arg_count);
            },

            else => {},
        }
    }
    self.runtime_error("Can only call functions and classes.");
    return false;
}

/// Invokes from a class.
/// 
/// # Errors
/// 
/// Reports a runtime error if invoking an undefined property.
/// 
pub fn invoke_from_class(self: *Self, class: *Obj.Class, name: *Obj.String, arg_count: u8) bool {
    const method = class.methods.get(name) orelse {
        self.runtime_error_fmt("Undefined property '{s}'.\n", .{name.chars[0..name.len]});
        return false;
    };

    return self.call(method.as_obj_narrowed(Obj.Closure).?, arg_count);
}

/// Invokes a method on a class.
/// 
/// # Errors
/// 
/// Reports a runtime error if attempting to call a method on an obj that is not an Instance.
/// 
pub fn invoke(self: *Self, name: *Obj.String, arg_count: u8) !bool {
    const receiver = self.peek(arg_count);

    const instance: *Obj.Instance = receiver.as_obj_narrowed(Obj.Instance) orelse {
        self.runtime_error("Only instances have methods.");
        return false;
    };

    if (instance.fields.get(name)) |value| {
        (self.values.top - arg_count - 1)[0] = value;
        return self.call_value(value, arg_count);
    }
    return self.invoke_from_class(instance.class, name, arg_count);
}

/// Binds a method to a variable.
/// 
fn bind_method(self: *Self, class: *Obj.Class, name: *Obj.String) !bool {
    if (class.methods.get(name)) |method| {
        const bound: *Obj.BoundMethod = try Obj.BoundMethod.init(self.gc, self.peek(0), method.as_obj_narrowed(Obj.Closure).?);

        _ = self.pop();
        self.push(Value.obj(bound.widen()));

        return true;
    }
    self.runtime_error_fmt("Undefined property '{s}'.\n", .{name.chars[0..name.len]});
    return false;
}

/// Captures an upvalue.
/// 
pub fn capture_upvalue(self: *Self, local: *Value) !*Obj.Upvalue {
    var prev_upvalue: ?*Obj.Upvalue = null;
    var upvalue = self.gc.open_upvalues;

    while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
        prev_upvalue = upvalue;
        upvalue = upvalue.?.next;
    }

    if (upvalue != null and @intFromPtr(upvalue.?.location) == @intFromPtr(local)) {
        return upvalue.?;
    }

    const created_upvalue = try self.gc.alloc_obj(Obj.Upvalue);
    created_upvalue.init(local);
    created_upvalue.next = upvalue;

    if (prev_upvalue == null) {
        self.gc.open_upvalues = created_upvalue;
    } 
    else {
        prev_upvalue.?.next = created_upvalue;
    }

    return created_upvalue;
}

/// Closes upvalues.
/// 
pub fn close_upvalues(self: *Self, last: [*]Value) void {
    while (self.gc.open_upvalues) |open_upvalues| {
        if (!(@intFromPtr(open_upvalues) >= @intFromPtr(last))) {
            break;
        }

        var upvalue = open_upvalues;
        upvalue.closed = upvalue.location.*;
        upvalue.location = &upvalue.closed;
        self.gc.open_upvalues = upvalue.next;
    }
}

// Defines a method.
//
fn define_method(self: *Self, name: *Obj.String) !void {
    const method = self.peek(0);
    const class = self.peek(1).as_obj().?.narrow(Obj.Class);
    _ = try class.methods.insert(self.gc.as_allocator(), name, method);
    _ = self.pop();
}

/// Pushes value on stack.
/// 
pub inline fn push(self: *Self, value: Value) void {
    self.values.push(value);
}

/// Pops value from stack.
/// 
pub inline fn pop(self: *Self) Value {
    return self.values.pop();
}

/// Prints a stack trace for runtime error.  Or at least it should :)
/// 
pub fn runtime_error(self: *Self, message: []const u8) void {
    {
        const frame = &self.call_frames.stack[self.call_frames.count - 1];
        const instr = @intFromPtr(frame.ip) - @intFromPtr(frame.closure.function.chunk.code.items.ptr) - 1;
        debug.print("[line {d}] Runtime error: {s}\n", .{ frame.closure.function.chunk.lines.items[instr], message });
    }

    // FIXME
    // var i = self.call_frames.count - 1;
    // while (i >= 0) : (i -= 1) {
    //     const frame = &self.call_frames.stack[i];
    //     const function = frame.function;
    //     const instr = @intFromPtr(frame.ip) - @intFromPtr(frame.function.chunk.code.items.ptr) - 1;
    //     debug.print("[line {d}] in ", .{function.chunk.lines.items[instr]});
    //     if (function.name) |name| {
    //         debug.print("{s}()\n", .{name.as_string()});
    //     } 
    //     else {
    //         debug.print("script\n", .{});
    //     }
    // }

    self.reset_stack();
}

/// It is important that these are defined first before anything is put on the stack
pub fn define_native(self: *Self, name: []const u8, function: Obj.NativeFunction.NativeFn) !void {
    const name_str = (try self.gc.copy_string(@as([*]const u8, @ptrCast(name)), @as(u32, @intCast(name.len)))).widen();
    self.push(Value.obj(name_str));

    var native = try self.gc.alloc_obj(Obj.NativeFunction);
    native.init(function);
    self.push(Value.obj(native.widen()));

    _ = try self.gc.globals.insert(self.gc.as_allocator(), self.values.stack[0].as_obj().?.narrow(Obj.String), self.values.stack[1]);
    _ = self.pop();
    _ = self.pop();
}

/// Pops top two values from stack, concatenates them, and pushes the result.
/// 
pub fn concatenate(self: *Self) !void {
    const b = self.peek(0).Obj.narrow(Obj.String);
    const a = self.peek(1).Obj.narrow(Obj.String);

    const new_len = a.len + b.len;
    const new_chars = try self.gc.as_allocator().alloc(u8, new_len);

    mem.copy(u8, new_chars, a.chars[0..a.len]);
    mem.copy(u8, new_chars[a.len..], b.chars[0..b.len]);

    const str = try self.gc.take_string(@as([*]const u8, @ptrCast(new_chars)), new_len);

    _ = self.pop();
    _ = self.pop();

    self.push(Value.obj(str.widen()));
}

/// Handles binary operation.  TODO: should be inline?
/// 
pub fn binary_op(self: *Self, comptime op: Opcode) InterpretResult {
    if (self.peek(0).as_number() == null or self.peek(1).as_number() == null) {
        self.runtime_error("Operands must be numbers.");
        return InterpretResult.INTERPRET_RUNTIME_ERROR;
    }

    const b = self.pop().Number;
    const a = self.pop().Number;

    var val: Value =
        switch (op) {
        .OP_ADD => Value.number(a + b),
        .OP_SUBTRACT => Value.number(a - b),
        .OP_MULTIPLY => Value.number(a * b),
        .OP_DIVIDE => Value.number(a / b),

        .OP_LESS => Value.boolean(a < b),
        .OP_GREATER => Value.boolean(a > b),
        else => unreachable,
    };

    self.push(val);
    return InterpretResult.INTERPRET_OK;
}

/// Runs the bytecode.
/// 
pub fn run(self: *Self) InterpretResult {
    if (DEBUG_TRACE_EXECUTION) {
       debug.print("=== run ===\n", .{});
    }
    var frame: *CallFrame = &self.call_frames.stack[self.call_frames.count - 1];
    
    while (true) {
        const instruction = @as(Opcode, @enumFromInt(frame.read_byte()));

        // Print stack trace
        const top: usize = @intFromPtr(self.values.top);
        const stack: usize = @intFromPtr(self.values.stack[0..]);

        if (DEBUG_TRACE_EXECUTION) {
            if (top == stack) {
                debug.print("        STACK: []\n", .{});
            } 
            else {
                _ = @subWithOverflow(top, stack);
                const value_size: usize = @sizeOf(Value);
                const idx: usize = (top - stack) / value_size;
                debug.print("        STACK: [", .{});
                for (self.values.stack[0..idx]) |value| {
                    value.print(debug);
                    debug.print(" ", .{});
                }
                debug.print("]\n", .{});
            }

            const offset = @intFromPtr(frame.ip) - @intFromPtr(frame.closure.function.chunk.code.items.ptr);
            if (offset >= frame.closure.function.chunk.code.items.len) {
                //break :blk;
            }
            _ = frame.closure.function.chunk.disassemble_instruction(offset - 1);

        }
        self.gc.enabled = true;

        // blk: {
            switch (instruction) {
                .OP_CONSTANT => {
                    const constant = frame.read_constant();
                    self.values.push(constant);
                },
                
                .OP_NIL => self.push(Value.nil()),
                .OP_TRUE => self.push(Value.boolean(true)),
                .OP_FALSE => self.push(Value.boolean(false)),

                .OP_POP => {
                    _ = self.pop();
                },

                .OP_JUMP => {
                    const offset = frame.read_u16();
                    frame.ip += offset;
                },

                .OP_JUMP_IF_FALSE => {
                    const offset = frame.read_u16();
                    if (self.peek(0).is_falsey()) {
                        frame.ip += offset;
                    }
                },

                .OP_LOOP => {
                    const offset = frame.read_u16();
                    frame.ip -= offset;
                },

                .OP_SET_LOCAL => {
                    const slot = frame.read_byte();
                    frame.slots[slot] = self.peek(0);
                },

                .OP_GET_LOCAL => {
                    const slot = frame.read_byte();
                    self.push(frame.slots[slot]);
                },

                .OP_SET_GLOBAL => {
                    const name = frame.read_string();
                    const val = self.peek(0);
                    const result = self.gc.globals.insert(self.gc.as_allocator(), name, val) catch @panic("SET GLOBAL ERROR!");

                    if (result) {
                        _ = self.gc.globals.delete(name);
                        self.runtime_error_fmt("redefinition of global variable '{}'", .{name});
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                },

                .OP_GET_GLOBAL => {
                    const name = frame.read_string();
                    if (self.gc.globals.get(name)) |value| {
                        
                        self.push(value);
                    } 
                    else {
                        @panic("Runtime Error!!!!");
                        //return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                },

                .OP_DEFINE_GLOBAL => {
                    const name = frame.read_string();
                    const value = self.peek(0);
                    _ = self.gc.globals.insert(self.gc.as_allocator(), name, value) catch @panic("BAH!");
                    _ = self.pop();
                },

                .OP_EQUAL => {
                    const b = self.pop();
                    const a = self.pop();

                    self.push(Value.boolean(Value.eq(a, b)));
                },

                .OP_GREATER => {
                    _ = self.binary_op(.OP_GREATER);
                },

                .OP_LESS => {
                    _ = self.binary_op(.OP_LESS);
                },

                .OP_ADD => {
                    if (self.peek(0).as_obj()) |a| {
                        if (self.peek(1).as_obj()) |b| {
                            if (a.is(Obj.String) and b.is(Obj.String)) {
                                self.concatenate() catch {
                                    debug.print("BAH!!!", .{});
                                };
                                continue;
                            }
                        }
                    }
                    const result = self.binary_op(.OP_ADD);
                    if (result == InterpretResult.INTERPRET_RUNTIME_ERROR) {
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                },

                .OP_SUBTRACT => {
                    const result = self.binary_op(.OP_SUBTRACT);
                    if (result == InterpretResult.INTERPRET_RUNTIME_ERROR) {
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                },

                .OP_MULTIPLY => { 
                    const result = self.binary_op(.OP_MULTIPLY);
                    if (result == InterpretResult.INTERPRET_RUNTIME_ERROR) {
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                },

                .OP_DIVIDE => {
                    const result = self.binary_op(.OP_DIVIDE);
                    if (result == InterpretResult.INTERPRET_RUNTIME_ERROR) {
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                },

                .OP_NOT => {
                    self.push(Value.boolean(self.pop().is_falsey()));
                },

                .OP_NEGATE => {
                    const number = self.peek(0).as_number() orelse {
                        self.runtime_error("Operand must be a number.");
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    };
                    self.push(Value.number(-number));
                },

                .OP_PRINT => {
                    self.pop().print(debug);
                    debug.print("\n", .{});
                },

                .OP_CALL => {
                    const arg_count = frame.read_byte();
                    if (!(try self.call_value(self.peek(arg_count), arg_count))) {
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                    frame = &self.call_frames.stack[self.call_frames.count - 1];
                },

                .OP_GET_SUPER => {
                    const name = frame.read_string();
                    const superclass = self.pop().as_obj_narrowed(Obj.Class).?;

                    if (!(self.bind_method(superclass, name) catch @panic("BAH"))) {
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                },

                .OP_INHERIT => {
                    const superclass: *Obj.Class = self.peek(1).as_obj_narrowed(Obj.Class) orelse {
                        self.runtime_error("Superclass must be a class.");
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    };
                    const subclass: *Obj.Class = self.peek(0).as_obj_narrowed(Obj.Class).?;

                    superclass.methods.add_all(self.gc.as_allocator(), &subclass.methods) catch {
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    };
                    // pop the subclass
                    _ = self.pop();
                },

                .OP_METHOD => {
                    self.define_method(frame.read_string()) catch @panic("Error defining method.");
                },

                .OP_INVOKE => {
                    const method = frame.read_string();
                    const arg_count = frame.read_byte();
                    if (!(try self.invoke(method, arg_count))) {
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                    frame = &self.call_frames.stack[self.call_frames.count - 1];
                },

                .OP_INVOKE_SUPER => {
                    const method = frame.read_string();
                    const arg_count = frame.read_byte();
                    const superclass = self.pop().as_obj_narrowed(Obj.Class).?;
                    if (!self.invoke_from_class(superclass, method, arg_count)) {
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                    frame = &self.call_frames.stack[self.call_frames.count - 1];
                },

                .OP_GET_PROPERTY => {
                    const instance: *Obj.Instance = self.peek(0).as_obj_narrowed(Obj.Instance) orelse {
                        self.runtime_error("Only instances have properties.");
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    };
                    const name = frame.read_string();

                    if (instance.fields.get(name)) |value| {
                        _ = self.pop();
                        self.push(value);
                        continue;
                    } 

                    if (!(self.bind_method(instance.class, name) catch @panic("BAH!!!"))) {
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    }
                },

                .OP_SET_PROPERTY => {
                    const instance: *Obj.Instance = self.peek(1).as_obj_narrowed(Obj.Instance) orelse {
                        self.runtime_error("Only instances have properties.");
                          return InterpretResult.INTERPRET_RUNTIME_ERROR;
                    };

                    _ = instance.fields.insert(self.gc.as_allocator(), frame.read_string(), self.peek(0)) catch @panic("Error inserting field!");

                    const val = self.pop();
                    _ = self.pop();
                    self.push(val);
                },

                .OP_CLASS => {
                    const class_name = frame.read_string();
                    const class = Obj.Class.init(self.gc, class_name) catch @panic("Error init Class!");
                    self.push(Value.obj(class.widen()));
                },

                .OP_CLOSE_UPVALUE => {
                    self.close_upvalues(self.values.top - 1);
                    _ = self.pop();
                },

                .OP_GET_UPVALUE => {
                    const slot = frame.read_byte();
                    const val: Value = frame.closure.upvalues[slot].location.*;
                    self.push(val);
                },

                .OP_SET_UPVALUE => {
                    const slot = frame.read_byte();
                    const val = self.peek(0);
                    frame.closure.upvalues[slot].location.* = val;
                },

                .OP_CLOSURE => {
                    var function = frame.read_constant().as_obj().?.narrow(Obj.Function);
                    var closure = Obj.Closure.init(self.gc, function) catch {
                        // BAH
                        return InterpretResult.INTERPRET_RUNTIME_ERROR;              
                    };
                    self.push(Value.obj(closure.widen()));
                    var i: usize = 0;
                    while (i < closure.upvalues_len) : (i += 1) {
                        const is_local = frame.read_byte() == 1;
                        const index = frame.read_byte();
                        if (is_local) {
                            closure.upvalues[i] = self.capture_upvalue(&frame.slots[index]) catch {
                                // BAH
                                return InterpretResult.INTERPRET_RUNTIME_ERROR;                          
                            };
                        } 
                        else {
                            closure.upvalues[i] = frame.closure.upvalues[index];
                        }
                    }
                },

                .OP_RETURN => {
                    // // TODO: WHY?
                    // // if (self.call_frames.count == 1) {
                    // //     _ = self.pop();
                    // //     return;
                    // // }

                    const result = self.pop();
                    self.close_upvalues(frame.slots);
                    self.call_frames.count -= 1;
                    if (self.call_frames.count == 0) {
                        _ = self.pop();
                        return InterpretResult.INTERPRET_OK;
                    }

                    self.values.top = frame.slots;
                    self.push(result);
                    frame = &self.call_frames.stack[self.call_frames.count - 1];
                },
                

                .OP_BREAK => {
                    return InterpretResult.INTERPRET_OK;
                },

            }
        //}
    }
    return InterpretResult.INTERPRET_OK;
}

/// Scans, parses, and interprets source.
/// 
pub fn interpret(allocator: Allocator, source: []const u8) !InterpretResult {
    var gc: *GC = try allocator.create(GC);
    try gc.init(allocator);

    var parser = Compiler.init_parser();
    var scanner = Scanner.init(source);
    var compiler = try Compiler.init(gc, null, &scanner, &parser, null, FunctionType.Script);

    const function = try compiler.compile() orelse return InterpretResult.INTERPRET_COMPILE_ERROR;
    const closure = try Obj.Closure.init(gc, function);
    try VM.init(gc, closure);
    return VM.run();
}

/// Uses for testing.  Emits OP_BREAK to allow stack to be introspected.  FIXME.
/// 
fn test_interpret(source: []const u8) !InterpretResult {
    var gc: *GC = try Test.alloc.create(GC);
    try gc.init(Test.alloc);

    var parser = Compiler.init_parser();
    var scanner = Scanner.init(source);
    var compiler = try Compiler.init(gc, null, &scanner, &parser, null, FunctionType.Script);
    compiler.test_mode = true;

    const function = try compiler.compile() orelse return InterpretResult.INTERPRET_COMPILE_ERROR;
    const closure = try Obj.Closure.init(gc, function);
    try VM.init(gc, closure);
    return VM.run();
}

/// Prints runtime error and resets stack.
/// 
pub fn runtime_error_fmt(self: *Self, comptime fmt: []const u8, args: anytype) void {
    debug.print(fmt, args);
    self.reset_stack();
}

// Interpreting a constant should push it on the stack.
//
test "Test Constant Operator" {
    var result = try test_interpret("1.2;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);
    //try expectEqual(VM.values.pop(), Value.number(1.2));

    printStatus("Test Constant Operator");
}

// Greater operator should pop the top two values, and push the result of the comparison.
//
test "Test Greater Operator" {
    var result = try test_interpret("2 > 1;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);
    //try expectEqual(VM.values.pop().as_boolean(), false);

    printStatus("Test Greater Operator");
}

// Less operator should pop the top two values, and push the result of the comparison.
//
test "Test Less Operator" {
    var result = try test_interpret("1 < 2;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);
    //try expectEqual(VM.values.pop().as_boolean(), true);

    printStatus("Test Less Operator");
}

// Equal operator should pop the top two values, and push the result of the comparison.
//
test "Test Equal Operator" {
    var result = try test_interpret("1 == 1;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);
    //try expectEqual(VM.values.pop().as_boolean(), true);

    printStatus("Test Equal Operator");
}


// Add operator should pop the top two values, and push the result of addition.
//
test "Test Add Operator" {
    var result = try test_interpret("1 + 1;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    //const expected: f64 = 2.4;
    //try expectEqual(expected, VM.values.pop().as_number().?);

    printStatus("Test Add Operator");
}

// Adding string should concatenate them.
//
test "Test Add Strings" {
    var result = try test_interpret("\"ABC\" + \"DEF\";");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);
    
   // const expected: *Obj.String = try copy_string("ABCDEF", 6);
    //try assertTrue(expected.eq(VM.values.pop().as_obj_narrowed(String).?));

    printStatus("Test Add Strings");
}

// Add operator should fail if left not a number.
//
test "Test Add Left Not Number" {
    var result = try test_interpret("true + 1;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_RUNTIME_ERROR, result);
    // Assert Operands must be numbers

    printStatus("Test Add Left Not Number");
}

// Add operator should fail if right not a number.
//
test "Test Add Right Not Number" {
    var result = try test_interpret("1.2 + true;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_RUNTIME_ERROR, result);
    // Assert Operands must be numbers

    printStatus("Test Add Right Not Number");
}

// Subtract operator should pop the top two values, and push the result of subtraction.
//
test "Test Subtract Operator" {
    var result = try test_interpret("5 - 2;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);
    //try expectEqual(VM.values.pop().as_number(), 2);

    printStatus("Test Subtract Operator");
}

// Subtract operator should fail if left not a number.
//
test "Test Subtract Left Not Number" {
    var result = try test_interpret("false - 2;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_RUNTIME_ERROR, result);
    // Assert Operands must be numbers

    printStatus("Test Subtract Left Not Number");
}

// Subtract operator should fail if right not a number.
//
test "Test Subtract Right Not Number" {
    var result = try test_interpret("5 - false;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_RUNTIME_ERROR, result);
    // Assert Operands must be numbers

    printStatus("Test Subtract Right Not Number");
}

// Multiply operator should pop the top two values, and push the result of multiplication.
//
test "Test Multiply Operator" {
    var result = try test_interpret("2 * 3;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);
    //try expectEqual(VM.values.pop().as_number(), 6);

    printStatus("Test Multiply Operator");
}

// Multiply operator should fail if left not a number.
//
test "Test Multiply Left Not Number" {
    var result = try test_interpret("true * 1;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_RUNTIME_ERROR, result);
    // Assert Operands must be numbers

    printStatus("Test Multiply Left Not Number");
}

// Multiply operator should fail if right not a number.
//
test "Test Multiply Right Not Number" {
    var result = try test_interpret("1 * false;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_RUNTIME_ERROR, result);
    // Assert Operands must be numbers

    printStatus("Test Multiply Right Not Number");
}

// Divide operator should pop the top two values, and push the result of division.
//
test "Test Divide Operator" {
    var result = try test_interpret("1/1;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);
    //try expectEqual(VM.values.pop().as_number(), 1);

    printStatus("Test Divide Operator");
}

// Divide operator should fail if left not a number.
//
test "Test Divide Left Not Number" {
    var result = try test_interpret("true/1;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_RUNTIME_ERROR, result);
    // Assert Operands must be numbers

    printStatus("Test Divide Left Not Number");
}

// Divide operator should fail if right not a number.
//
test "Test Divide Right Not Number" {
    var result = try test_interpret("1/true;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_RUNTIME_ERROR, result);
    // Assert Operands must be numbers

    printStatus("Test Divide Right Not Number");
}

// Negate operator should pop the top value, and push the negation.
//
test "Test Negate Operator" {
    var result = try test_interpret("-3.14;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);
    //try expectEqual(VM.values.pop().as_number(), -3.14);

    printStatus("Test Negate Operator");
}

// Negate operator should fail if not a number.
//
test "Test Negate Not A Number" {
    var result = try test_interpret("-true;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_RUNTIME_ERROR, result);
    // Assert Operand must be a number.
    
    printStatus("Test Negate Not A Number");
}

// Not operator should pop the top value, and push the negation.
//
test "Test Not Operator" {
    var result = try test_interpret("!true;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);
    // Assert Operand must be a number.

    printStatus("Test Not Operator");
}

// Tests defining a global variable.
//
test "Test Define Global" {
    var result = try test_interpret("var abc = 123;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test Define Global");
}

// Tests setting a global variable.
//
test "Test Set Global" {
    var result = try test_interpret("var abc = 123; abc = 2;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test Set Global");
}

// Tests getting a global variable.
//
test "Test Get Global" {
    var result = try test_interpret("var abc = 123; print abc;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test Get Global");
}

// Tests setting a local variable.
//
test "Test Set Local" {
    var result = try test_interpret("{ var abc = 123; abc = 2; }");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test Set Local");
}

// Tests getting a local variable.
//
test "Test Get Local" {
    var result = try test_interpret("{ var abc = 123; print abc; }");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test Get Local");
}

// Tests if statement.
//
test "Test If Statement" {
    var result = try test_interpret("if (true) { print \"Hello!\"; }");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test If Statement");
}

// Tests while statement.
//
test "Test While Statement" {
    var result = try test_interpret("var test = true; while (test) { test = false; } print 123;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test While Statement");
}

// Tests for statement.
//
test "Test For Statement" {
    var result = try test_interpret("for (var test = 1; test < 5; test = test + 1) { print 777; }");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test For Statement");
}

// Tests Class declaration.
//
test "Test Class Declaration" {
    var result = try test_interpret("class Brioche {} print Brioche;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test Class Declaration");
}

// Tests Get And Set Property.
//
test "Test Get And Set Property" {
    debug.print("\n", .{});

    var result = try test_interpret("class Brioche {} var test = Brioche(); test.field = 123; print test.field;");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test Get And Set Property");
}

// Tests Defining And Invoking a Method
//
test "Test Define And Invoke Method" {
    debug.print("\n", .{});

    var result = try test_interpret("class Test { WriteLn() { print \"ABC\"; } } var test = Test(); test.WriteLn();");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test Define And Invoke Method");
}

// Tests using the this operator.
//
test "Test This" {
    debug.print("\n", .{});

    var result = try test_interpret("class Test { WriteLn() { print this.Text ; } } var test = Test(); test.Text = \"HELLO!!!\"; test.WriteLn();");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test This");
}

// Tests using this on a property that doesn't exist.
//
test "Test This No Property" {
    debug.print("\n", .{});

    var result = try test_interpret("class Test { WriteLn() { print this.Text ; } } var test = Test(); test.WriteLn();");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_RUNTIME_ERROR, result);

    printStatus("Test This No Property");
}

// Tests initializer.
//
test "Test Initializer" {
    debug.print("\n", .{});

    var result = try test_interpret("class Test { init(Text) { this.Text = Text; } WriteLn() { print this.Text ; } } var test = Test(\"Hello, Wisconsin!!!\"); test.WriteLn();");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test Initializer");
}

// Tests invoking super.
//
test "Test Invoke Super" {
    debug.print("\n", .{});

    var result = try test_interpret("class Animal { talk() { print \"Says:\";}} class Dog < Animal { talk() { super.talk(); print \"Bow Wow\"; }} var dog = Dog(); dog.talk();");

    // Assert
    try expectEqual(InterpretResult.INTERPRET_OK, result);

    printStatus("Test Invoke Super");
}