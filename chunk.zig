const std = @import("std");
const mem = std.mem;
const debug = std.debug;

const Allocator = mem.Allocator;
const ArrayList = std.ArrayListUnmanaged;
const Value = @import("value.zig").Value;
const Obj = @import("obj.zig").Obj;

// Testing
const test_allocator = std.testing.allocator;


/// Opcodes!
/// 
pub const Opcode = enum(u8) { 
    OP_CONSTANT, 
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,
    OP_DEFINE_GLOBAL,
    OP_EQUAL,
    OP_GET_SUPER,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_INVOKE,
    OP_INVOKE_SUPER,
    OP_CLASS,
    OP_INHERIT,
    OP_METHOD,
    OP_CLOSURE,
    OP_CLOSE_UPVALUE,
    OP_RETURN,
    OP_BREAK,
};


/// A chunk of bytecode.
///
pub const Chunk = struct {
    const Self = @This();

    code: ArrayList(u8),
    constants: ArrayList(Value),
    lines: ArrayList(u16),

    /// Initializes a Chunk!
    ///
    pub fn init(allocator: Allocator) Allocator.Error!Self {
        return Chunk{
            .code = try ArrayList(u8).initCapacity(allocator, 64),
            .constants = try ArrayList(Value).initCapacity(allocator, 64),
            .lines = try ArrayList(u16).initCapacity(allocator, 64),
        };
    }

    // Writes an opcode to the chunk.
    pub fn write_op(self: *Chunk, allocator: Allocator, op: Opcode, line: u16) !void {
        try self.write_byte(allocator, @intFromEnum(op), line);
    }

    /// Writes a byte to the chunk.
    ///
    pub fn write_byte(self: *Chunk, allocator: Allocator, byte: u8, line: u16) !void {
        try self.code.append(allocator, byte);
        try self.lines.append(allocator, line);
    }

    /// Writes a "short" to the chunk.
    /// 
    pub fn write_u16(self: *Chunk, allocator: Allocator, val: u16, line: u16) !void {
        try self.write_byte(allocator, @as(u8, @intCast(val >> 8)), line);
        try self.write_byte(allocator, @as(u8, @truncate(val)), line);
    }

    /// Frees up the memory used by the chunk.
    ///
    pub fn free(self: *Chunk, allocator: Allocator) void {
        self.code.clearAndFree(allocator);
        self.constants.clearAndFree(allocator);
        self.lines.clearAndFree(allocator);
    }

    /// Adds a constant value to the chunk.
    ///
    pub fn add_constant(self: *Chunk, allocator: Allocator, value: Value) !u8 {
        const index = self.constants.items.len;
        try self.constants.append(allocator, value);
        return @as(u8, @truncate(index));
    }

    /// Prints a disassembled view of the chunk.
    ///
    pub fn disassemble(self: *const Chunk, name: []const u8) void {
        debug.print("== {s} ==\n", .{name});
        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassemble_instruction(offset);
        }
    }

    /// Disassembles an instruction.
    ///
    pub fn disassemble_instruction(self: *const Chunk, offset: usize) usize {
        debug.print("{:0>4} ", .{offset});
        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            debug.print("   | ", .{});
        } 
        else {
            debug.print("{d:04} ", .{self.lines.items[offset]});
        }

        const instruction: Opcode = @as(Opcode, @enumFromInt(self.code.items[offset]));
        switch (instruction) {
            .OP_GET_LOCAL => {
                return self.byte_instruction("OP_GET_LOCAL", offset);
            },

            .OP_SET_LOCAL => {
                return self.byte_instruction("OP_SET_LOCAL", offset);
            },

            .OP_JUMP => {
                return self.jump_instruction("OP_JUMP", 1, offset);
            },

            .OP_JUMP_IF_FALSE => {
                return self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset);
            },

            .OP_LOOP => {
                return self.jump_instruction("OP_LOOP", 1, offset);
            },

            .OP_GET_GLOBAL => {
                return self.constant_instruction("OP_GET_GLOBAL", offset);
            },

            .OP_SET_GLOBAL => {
                return self.constant_instruction("OP_SET_GLOBAL", offset);
            },

            .OP_DEFINE_GLOBAL => {
                return self.constant_instruction("OP_DEFINE_GLOBAL", offset);
            },

            .OP_GET_PROPERTY => {
                return self.constant_instruction("OP_GET_PROPERTY", offset);
            },

            .OP_SET_PROPERTY => {
                return self.constant_instruction("OP_SET_PROPERTY", offset);
            },

            .OP_GET_SUPER => {
                return self.constant_instruction("OP_GET_SUPER", offset);
            },

            .OP_EQUAL => {
                return simple_instruction("OP_EQUAL", offset);
            },
            .OP_GREATER => {
                return simple_instruction("OP_GREATER", offset);
            },
            .OP_LESS => {
                return simple_instruction("OP_LESS", offset);
            },

            .OP_CONSTANT => {
                return self.constant_instruction("OP_CONSTANT", offset);
            },

            .OP_NIL => {
                return simple_instruction("OP_NIL", offset);
            },

            .OP_TRUE => {
                return simple_instruction("OP_TRUE", offset);
            },

            .OP_NOT => {
                return simple_instruction("OP_NOT", offset);
            },

            .OP_FALSE => {
                return simple_instruction("OP_FALSE", offset);
            },

            .OP_NEGATE => {
                return simple_instruction("OP_NEGATE", offset);
            },

            .OP_ADD => {
                return simple_instruction("OP_ADD", offset);
            },

            .OP_SUBTRACT => {
                return simple_instruction("OP_SUBTRACT", offset);
            },

            .OP_MULTIPLY => {
                return simple_instruction("OP_MULTIPLY", offset);
            },

            .OP_DIVIDE => {
                return simple_instruction("OP_DIVIDE", offset);
            },

            .OP_PRINT => {
                return simple_instruction("OP_PRINT", offset);
            },

            .OP_POP => {
                return simple_instruction("OP_POP", offset);
            },

            .OP_CALL => {
                return self.byte_instruction("OP_CALL", offset);
            },

            .OP_CLOSURE => {
                var ret = offset + 1;
                const constant = self.code.items.ptr[ret];
                ret += 1;
                //self.constants.items[constant].print(debug);

                const function: *Obj.Function = self.constants.items[constant].as_obj().?.narrow(Obj.Function);
                debug.print("{s} {d} {d} \n", .{ "OP_CLOSURE", constant, function.upvalue_count });
                var j: usize = 0;
                while (j < function.upvalue_count) : (j += 1) {
                    const is_local = self.code.items[ret];
                    ret += 1;
                    const index = self.code.items[ret];
                    ret += 1;
                    debug.print("{d:04}    |   {s} {d}\n", .{ ret - 2, if (is_local == 1) "local" else "upvalue", index });
                }

                return ret;
            },

            .OP_CLOSE_UPVALUE => {
                return self.byte_instruction("OP_CLOSE_UPVALUE", offset);
            },

            .OP_GET_UPVALUE => {
                return self.byte_instruction("OP_GET_UPVALUE", offset);
            },

            .OP_SET_UPVALUE => {
                return self.byte_instruction("OP_SET_UPVALUE", offset);
            },

            .OP_RETURN => {
                return simple_instruction("OP_RETURN", offset);
            },

           .OP_CLASS => {
                return self.constant_instruction("OP_CLASS", offset);
            },

           .OP_METHOD => {
                return self.constant_instruction("OP_METHOD", offset);
            },

           .OP_INHERIT => {
                return simple_instruction("OP_INHERIT", offset);
            },

            .OP_INVOKE => {
                return self.invoke_instruction("OP_INVOKE", offset);
            },

            .OP_INVOKE_SUPER => {
                return self.invoke_instruction("OP_INVOKE_SUPER", offset);
            },

            .OP_BREAK => {
                return simple_instruction("OP_BREAK", offset);
            },
        }
    }

    /// Disassembles a simple instruction.
    ///
    fn simple_instruction(name: [:0]const u8, offset: usize) usize {
        debug.print("{s}\n", .{name});
        return offset + 1;
    }

    // Disassembles a byte instruction.
    fn byte_instruction(self: *const Chunk, name: [:0]const u8, offset: usize) usize {
        const slot = self.code.items[offset + 1];
        debug.print("{s} {d}\n", .{ name, slot });
        return offset + 2;
    }

    // Disassembles a constant instruction.
    //
    fn constant_instruction(self: *const Chunk, name: [:0]const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];

        debug.print("{s} {d} ", .{ name, constant });
        // FIXME
        // self.constants.items[constant].print(debug);
        debug.print("\n", .{});

        return offset + 2;
    }

    // Disassembles a jump instruction.
    //
    fn jump_instruction(self: *const Chunk, name: [:0]const u8, sign: i32, offset: usize) usize {
        const jump: u16 = (@as(u16, @intCast(self.code.items[offset + 1])) << 8) | self.code.items[offset + 2];
        debug.print("{s} {d} -> {d}\n", .{ name, offset, @as(i64, @intCast(offset)) + 3 + sign * jump });
        return offset + 3;
    }

    // Disassembles an invoke instruction.
    //
    fn invoke_instruction(self: *const Chunk, name: [:0]const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        const arg_count = self.code.items[offset + 2];
        debug.print("{s} ({d} args) {d} ", .{ name, arg_count, constant });
        // self.constants.items[constant].print(debug);
        debug.print("\n", .{});
        return offset + 3;
    }
};

test "Tests Chunk" {
    var chunk = try Chunk.init(test_allocator);
    defer chunk.free(test_allocator);
    
    debug.print("\n", .{});
    var constant = try chunk.add_constant(test_allocator, Value.number(1.2));
    try chunk.write_op(test_allocator, Opcode.OP_CONSTANT, 123);
    try chunk.write_byte(test_allocator, constant, 123);

    constant = try chunk.add_constant(test_allocator, Value.number(3.4));
    try chunk.write_op(test_allocator, Opcode.OP_CONSTANT, 123);
    try chunk.write_byte(test_allocator, constant, 123);

    try chunk.write_op(test_allocator, Opcode.OP_ADD, 123);

    constant = try chunk.add_constant(test_allocator, Value.number(5.6));
    try chunk.write_op(test_allocator, Opcode.OP_CONSTANT, 123);
    try chunk.write_byte(test_allocator, constant, 123);

    try chunk.write_op(test_allocator, Opcode.OP_DIVIDE, 123);
    try chunk.write_op(test_allocator, Opcode.OP_NEGATE, 123);

    try chunk.write_op(test_allocator, Opcode.OP_RETURN, 123);

    chunk.disassemble("test chunk");
}

test "Tests Globals Chunk" {
    var chunk = try Chunk.init(test_allocator);
    defer chunk.free(test_allocator);
    
    debug.print("\n", .{});

    // Add constant
    var constant = try chunk.add_constant(test_allocator, Value.number(1.2));
    try chunk.write_op(test_allocator, Opcode.OP_CONSTANT, 123);
    try chunk.write_byte(test_allocator, constant, 123);

    // Define the global.
    try chunk.write_op(test_allocator, Opcode.OP_DEFINE_GLOBAL, 123);
    try chunk.write_byte(test_allocator, 0, 123);   

    // Add another constant.
    constant = try chunk.add_constant(test_allocator, Value.number(2));
    try chunk.write_op(test_allocator, Opcode.OP_CONSTANT, 123);
    try chunk.write_byte(test_allocator, constant, 123);

    try chunk.write_op(test_allocator, Opcode.OP_SET_GLOBAL, 123);
    try chunk.write_byte(test_allocator, 1, 123); 

    try chunk.write_op(test_allocator, Opcode.OP_GET_GLOBAL, 123);
    try chunk.write_byte(test_allocator, 1, 123); 

    try chunk.write_op(test_allocator, Opcode.OP_PRINT, 123);
    try chunk.write_op(test_allocator, Opcode.OP_RETURN, 123);

    chunk.disassemble("globals chunk");
}

test "Tests Locals Chunk" {
    var chunk = try Chunk.init(test_allocator);
    defer chunk.free(test_allocator);
    
    debug.print("\n", .{});

    // Add constant
    var constant = try chunk.add_constant(test_allocator, Value.number(1.2));
    try chunk.write_op(test_allocator, Opcode.OP_CONSTANT, 123);
    try chunk.write_byte(test_allocator, constant, 123);

    // Define the global.
    try chunk.write_op(test_allocator, Opcode.OP_DEFINE_GLOBAL, 123);
    try chunk.write_byte(test_allocator, 0, 123);   

    // Add another constant.
    constant = try chunk.add_constant(test_allocator, Value.number(2));
    try chunk.write_op(test_allocator, Opcode.OP_CONSTANT, 123);
    try chunk.write_byte(test_allocator, constant, 123);

    try chunk.write_op(test_allocator, Opcode.OP_SET_LOCAL, 123);
    try chunk.write_byte(test_allocator, 1, 123); 

    try chunk.write_op(test_allocator, Opcode.OP_GET_LOCAL, 123);
    try chunk.write_byte(test_allocator, 1, 123); 

    try chunk.write_op(test_allocator, Opcode.OP_PRINT, 123);
    try chunk.write_op(test_allocator, Opcode.OP_RETURN, 123);

    chunk.disassemble("globals chunk");
}
