const std = @import("std");
const debug = std.debug;

const Allocator = std.mem.Allocator;
const GC = @import("gc.zig");
const Obj = @import("obj.zig");
const Value = @import("value.zig").Value;

// Testing
const Test = @import("test.zig");
const _test = @import("test.zig");
const assertTrue = std.testing.expect;
const assertEqual = std.testing.expectEqual;
const printPass = _test.printPass;
const printStatus = _test.printStatus;
const test_allocator = std.testing.allocator;

/// Hash Table!!!
/// 
pub const Table = @This();

count: u32 = 0,
cap: u32 = 0,
entries: ?[*]Entry = null,

const MAX_LOAD = 0.75;

pub const Entry = struct { key: ?*Obj.String, val: Value };

/// Creates a new Table.
/// 
pub fn init() Table {
    return .{};
}

/// Frees the table.
/// 
pub fn free(self: *Table, allocator: Allocator) void {
    if (self.entries) |entries| {
        allocator.free(entries[0..self.cap]);
    }
    self.count = 0;
    self.cap = 0;
    self.entries = null;
}

/// Returns a hash for a string
/// 
pub fn hash_string(key: [*]const u8, len: u32) u32 {
    var hash: u32 = 2166136261;
    for (key[0..len]) |c| {
        hash ^= c;

        _ = @mulWithOverflow(16777619, hash);
    }
    return hash;
}

/// Gets an entry.
/// 
pub fn get(self: *Table, key: *Obj.String) ?Value {
    if (self.count == 0) return null;

    const entry = self.find_entry(key) orelse return null;
    if (entry.key == null) return null;

    return entry.val;
}

/// Inserts entry.  Returns true if new.
/// 
pub fn insert(self: *Table, allocator: Allocator, key: *Obj.String, val: Value) !bool {
    if (@as(f64, @floatFromInt(self.count + 1)) > @as(f64, @floatFromInt(self.cap)) * MAX_LOAD) {
        const new_cap = if (self.cap < 8) 8 else self.cap * 2;
        try self.adjust_capacity(allocator, new_cap);
    }

    // Above if branch will always set entries.
    const entry = self.find_entry(key) orelse unreachable;
    const is_new_key = entry.key == null;
    if (is_new_key and entry.val.as_nil() != null) {
        self.count += 1;
    }

    entry.key = key;
    entry.val = val;
    return is_new_key;
}

/// Deletes an entry.
/// 
pub fn delete(self: *Table, key: *Obj.String) bool {
    if (self.count == 0) return false;
    var entry = self.find_entry(key) orelse return false;
    if (entry.key == null) return false;

    entry.key = null;
    entry.val = Value.boolean(true);
    return true;
}

/// Copies all entries from one Table to another.
/// 
pub fn add_all(self: *const Table, allocator: Allocator, to: *Table) !void {
    const entries = self.entries_slice_const() orelse return;
    for (entries) |entry| {
        if (entry.key) |key| {
            _ = try to.insert(allocator, key, entry.val);
        }
    }
}

pub fn find_string(self: *Table, chars: [*]const u8, len: u32, hash: u32) ?*Obj.String {
    if (self.count == 0) return null;
    const entries = self.entries_slice() orelse return null;
    var idx = hash & (self.cap - 1);

    var entry: *Entry = undefined;
    while (true) {
        entry = &entries[idx];
        if (entry.key) |key| {
            if (key.len == len and key.hash == hash and std.mem.eql(u8, key.chars[0..key.len], chars[0..len])) return key;
        } else if (entry.val.as_nil() != null) {
            return null;
        }

        idx = (idx + 1) & (self.cap - 1);
    }
}

// Finds an entry.
//
fn find_entry(self: *Table, key: *Obj.String) ?*Entry {
    var entries = self.entries_slice() orelse return null;
    return find_entry_impl(entries, key);
}

// Actually finds the entry :)
//
fn find_entry_impl(entries: []Entry, key: *Obj.String) *Entry {
    const cap = entries.len;
    var idx = key.hash & (cap - 1);
    var tombstone: ?*Entry = null;
    while (true) {
        const entry = &entries[idx];
        if (entry.key == null) {
            if (entry.val.as_nil() != null) {
                if (tombstone) |ts| {
                    return ts;
                }
                return entry;
            }

            if (tombstone == null) {
                tombstone = entry;
            }
        } else if (entry.key == key) {
            return entry;
        }

        idx = (idx + 1) & (cap - 1);
    }
}

/// Adjusts the Table's capacity.
/// 
pub fn adjust_capacity(self: *Table, allocator: Allocator, new_cap: usize) !void {
    var new_entries = try allocator.alloc(Entry, new_cap);
    for (new_entries) |*entry| {
        entry.key = null;
        entry.val = Value.nil();
    }

    var count: u32 = 0;
    if (self.entries_slice()) |entries| {
        for (entries) |entry| {
            const key = entry.key orelse continue;

            var dest = find_entry_impl(new_entries, key);
            dest.key = key;
            dest.val = entry.val;
            count += 1;
        }
        allocator.free(entries);
    }

    self.entries = @as([*]Entry, @ptrCast(new_entries));
    self.cap = @as(u32, @intCast(new_cap));
    self.count = count;
}

pub fn remove_white(self: *Table) void {
    var i: usize = 0;
    while (i < self.cap) : (i += 1) {
        var entry = self.entries.?[i];
        if (entry.key != null and !entry.key.?.obj.is_marked) {
            _ = self.delete(entry.key.?);
        }
    }
}

pub inline fn entries_slice(self: *Table) ?[]Entry {
    if (self.entries) |entries| {
        return entries[0..self.cap];
    }

    return null;
}

pub fn entries_slice_const(self: *const Table) ?[]Entry {
    if (self.entries) |entries| {
        return entries[0..self.cap];
    }

    return null;
}

/// Prints the Table.
/// 
pub fn print(self: *Table, name: []const u8, writer: anytype) void {
    const entries = self.entries_slice() orelse return;

    writer.print("== begin {s} ==\n", .{name});
    for (entries) |entry| {
        if (entry.key == null) continue;
        writer.print("{s}: ", .{entry.key.?.as_string()});
        entry.val.print(writer);
        writer.print(", ", .{});
    }
    writer.print("\n== end {s} ==\n", .{name});
}

test "Test Hash" {
    debug.print("\n", .{});
    const hash = hash_string("ABC", 3);

    const expected: u32 = 2166136197;

    try assertEqual(expected, hash);

    printStatus("Test Hash");
}

test "Test Insert Entry" {
    debug.print("\n", .{});
    var gc: *GC = try Test.alloc.create(GC);
    try gc.init(Test.alloc);
    
    var table = Table.init();
    defer table.free(test_allocator);

    const key = try gc.copy_string("123456", 6);

    const result = try table.insert(test_allocator, key, Value.number(123456));
    const expected_count: u32 = 1;

    try assertTrue(result);
    try assertEqual(expected_count, table.count);

    printStatus("Test Insert Entry");
}

test "Test Get Entry" {
    debug.print("\n", .{});
    var gc: *GC = try Test.alloc.create(GC);
    try gc.init(Test.alloc);

    var table = Table.init();
    defer table.free(test_allocator);

    const key = try gc.copy_string("123456", 6);

    _ = try table.insert(test_allocator, key, Value.number(123456));

    const expected = Value.number(123456);
    const result = table.get(key).?;

    try assertTrue(expected.eq(result));

    printStatus("Test Get Entry");
}

test "Test Delete Entry" {
    debug.print("\n", .{});
    var gc: *GC = try Test.alloc.create(GC);
    try gc.init(Test.alloc);

    var table = Table.init();
    defer table.free(test_allocator);

    const key = try gc.copy_string("123456", 6);

    _ = try table.insert(test_allocator, key, Value.number(123456));
    
    const result = table.delete(key);

    // still has tombstone.
    const expected_count: u32 = 1;  

    try assertTrue(result);
    try assertEqual(expected_count, table.count);

    printStatus("Test Delete Entry");
}

test "Test Add All" {
    debug.print("\n", .{});
    var gc: *GC = try Test.alloc.create(GC);
    try gc.init(Test.alloc);

    var table = Table.init();
    defer table.free(test_allocator);

    const key = try gc.copy_string("123456", 6);
    const key2 = try gc.copy_string("ABCDEF", 6);

    _ = try table.insert(test_allocator, key, Value.number(123456));
    _ = try table.insert(test_allocator, key2, Value.number(123456));

    const result = table.delete(key);

    var table2 = Table.init();
    defer table2.free(test_allocator);

    try table.add_all(test_allocator, &table2);

    // tombstones removed
    const expected_count: u32 = 1;  

    try assertTrue(result);
    try assertEqual(expected_count, table2.count);

    printStatus("Test Add All");
}

test "Test Print Table" {
    debug.print("\n", .{});
    var gc: *GC = try Test.alloc.create(GC);
    try gc.init(Test.alloc);

    var table = Table.init();
    defer table.free(test_allocator);

    const key = try gc.copy_string("123456", 6);
    const key2 = try gc.copy_string("ABCDEF", 6);

    _ = try table.insert(test_allocator, key, Value.number(123456));
    _ = try table.insert(test_allocator, key2, Value.number(123456));

    table.print("test", debug);
    
    printStatus("Test Print Table");
}
