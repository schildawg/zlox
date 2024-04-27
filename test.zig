const debug = @import("std").debug;

const std = @import("std");
const assertTrue = std.testing.expect;
const assertEqual = std.testing.expectEqual;
const equals = std.mem.eql;

const Obj = @import("obj.zig");

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{
    .retain_metadata = true,
}){};

pub var alloc = general_purpose_allocator.allocator();

/// Asserts two strings are equal.
/// 
pub fn assertEquals(s1 : []const u8, s2: []const u8) !void {
    if (!equals(u8, s1, s2)) {
        debug.print("\x1b[91;1mFAIL\x1b[0m\n", .{});    
        debug.print("\x1b[91;1mExpected '{s}' but got '{s}'.\x1b[0m\n", .{s1, s2});        
    }
    try assertTrue(equals(u8, s1, s2));
}

/// Prints "PASS" in green.  Takes in status message for alignment.
/// 
pub fn printPass(name: []const u8) void {
    var count: u8 = 0;
    while (count < 32 - name.len) {
        debug.print(" ", .{});    
        count += 1;
    }
    debug.print("\x1b[32;1m PASS\x1b[0m\n", .{});
}

/// Prints "PASS" in green.  Takes in length for alignment.
/// 
pub fn printSizedPass(size : u8, name: []const u8) void {
    var count: u8 = 0;
    while (count < size - name.len) {
        debug.print(" ", .{});    
        count += 1;
    }
    debug.print("\x1b[32;1m PASS\x1b[0m\n", .{});
}

/// Prints status message, and "PASS" in green.
/// 
pub fn printStatus(name: []const u8) void {
    debug.print("{s}...", .{name});
    printSizedPass(50, name);
}

/// Allocates an Obj of type.
/// 
fn alloc_obj(comptime ParentType: type) !*ParentType {
    const ptr = try alloc.create(ParentType);

    ptr.widen().type = Obj.Type.from_obj(ParentType);
    ptr.widen().is_marked = false;

    return ptr;
}

/// Creates a String object.
/// 
pub fn alloc_string(chars: [*]const u8, len: u32) !*Obj.String {
    var ptr = try alloc_obj(Obj.String);
    ptr.len = len;
    ptr.chars = chars;
    //ptr.hash = hash;

    return ptr;
}