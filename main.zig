const std = @import("std");
const debug = std.debug;
const equals = std.mem.eql;
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const new_function = @import("gc.zig").new_function;

const _chunk = @import("chunk.zig");
const _compiler = @import("compiler.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const InterpretResult = VMType.InterpretResult;
const Chunk = _chunk.Chunk;
const Compiler = _compiler;
const FunctionType = _compiler.FunctionType;
const Opcode = _chunk.Opcode;
const Scanner = @import("scanner.zig").Scanner;
const VMType = @import("vm.zig");
const interpret = VMType.interpret;


var VM = VMType.get_vm();

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{
    .retain_metadata = true,
}){};

var alloc = general_purpose_allocator.allocator();

/// Main!!!
///
pub fn main() !void {
    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();

    // program name.
    _ = args.next();

    // file name
    if (args.next()) |arg| {
        // additional unneeded arguments.
        if (args.next()) |_| {  
            debug.print("Usage: zlox [path]\n", .{});
            return;
        }     
        // run file
        try run_file(alloc, arg);      
    }
    else {
        try repl(alloc);
    }

    try VM.free();
}

/// Run the REPL!
/// 
pub fn repl(allocator: Allocator, ) !void {
    var input = [_]u8{0} ** 1024;

    while (true) {
        try stdout.print("> ", .{});
        const slice = try stdin.readUntilDelimiter(&input, '\n');
        
        if (equals(u8, slice, "Quit") or equals(u8, slice, "quit")) {
           debug.print("Bye!\n", .{});
           return;
        }
        _ = try interpret(allocator, slice);
    }
}

/// Read a Lox file and run it.
/// 
pub fn run_file(allocator: Allocator, path: []const u8) !void {
    const source = std.fs.cwd().readFileAlloc(allocator, path, std.math.maxInt(usize)) catch {
        std.debug.print("Could not open file \"{s}\".\n", .{path});
        std.os.exit(74);
    };

    const result = try interpret(allocator, source);
    if (result == InterpretResult.INTERPRET_COMPILE_ERROR) std.os.exit(65);        
    if (result == InterpretResult.INTERPRET_RUNTIME_ERROR) std.os.exit(70);  

    allocator.free(source);
}
