const std = @import("std");
const debug = std.debug;

// Testing
const _test = @import("test.zig");
const assertTrue = std.testing.expect;
const assertEqual = std.testing.expectEqual;
const printPass = _test.printPass;
const printStatus = _test.printStatus;

const Value = @import("value.zig").Value;

pub fn clock(arg_count: u8, args: []Value) Value {
    _ = arg_count;
    _ = args;
    return Value.number(@floatFromInt(std.time.timestamp()));
}

test "Clock" {
    debug.print("\n", .{});
    var args: []Value = &.{};

    const value = clock(0, args);
    value.print(debug);

    debug.print("\n", .{});

    printStatus("Clock");
}


pub fn __dummy(arg_count: u8, args: []Value) Value {
    _ = arg_count;
    _ = args;
    return Value.number(420);
}
