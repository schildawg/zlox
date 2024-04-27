const std = @import("std");
const debug = std.debug;

const _chunk = @import("chunk.zig");
const _obj = @import("obj.zig");
const _scanner = @import("scanner.zig");

const Allocator = std.mem.Allocator;
const Chunk = _chunk.Chunk;
const Function = _obj.Function;
const InterpretResult = @import("vm.zig").InterpretResult;
const Scanner = _scanner;
const String = _obj.String;
const Obj = _obj.Obj;
const Opcode = _chunk.Opcode;
const Token = _scanner.Token;
const TokenType = _scanner.TokenType;
const Value = @import("value.zig").Value;
const GC = @import("gc.zig");
const DEBUG_PRINT_CODE = @import("conf.zig").DEBUG_PRINT_CODE;

// Testing
const Test = @import("test.zig");
const test_allocator = std.testing.allocator;
const _test = @import("test.zig");
const assertTrue = std.testing.expect;
const assertEqual = std.testing.expectEqual;
const printPass = _test.printPass;
const printStatus = _test.printStatus;

/// Parser!
/// 
const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,

    /// Create a new Parser.
    /// 
    pub fn init() Parser {
        return Parser{
            .current = undefined,
            .previous = undefined,
            .had_error = false,
            .panic_mode = false,
        };
    }
};

/// Precedence 
/// 
const Precedence = enum {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR, // or
    PREC_AND, // and
    PREC_EQUALITY, // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM, // + -
    PREC_FACTOR, // * /
    PREC_UNARY, // ! -
    PREC_CALL, // . ()
    PREC_PRIMARY,
};

/// Upvalue
/// 
pub const Upvalue = struct {
    index: u8,
    is_local: bool,
};

/// Defines a local variable.
/// 
pub const Local = struct {
    name: Token,
    depth: i32,
    is_captured: bool,
};

pub const FunctionType = enum { Function, Initializer, Method, Script };

/// Class Compiler
/// 
pub const ClassCompiler = struct {
    enclosing: ?*ClassCompiler,
    has_superclass: bool = false,
};

/// Initializes Parser.
/// 
pub fn init_parser() Parser {
    return Parser.init();
}

/// Parse Rule.
/// 
const ParseRule = struct {
    prefix: ?*const fn (*Compiler, bool) Allocator.Error!void = null,
    infix: ?*const fn (*Compiler, bool) Allocator.Error!void = null,
    precedence: Precedence = .PREC_NONE,
};
const ParseRuleTable = std.EnumArray(TokenType, ParseRule);

/// Looks up a rule in the Pratt parser table.
/// 
fn get_rule(token_type: TokenType) ParseRule {
    return rules.get(token_type);
}

/// Pratt parser table.
/// 
const rules = ParseRuleTable.init(.{
    .LEFT_PAREN  = ParseRule{.prefix = Compiler.grouping, .infix = Compiler.call, .precedence = Precedence.PREC_CALL },
    .RIGHT_PAREN = ParseRule{},
    .LEFT_BRACE  = ParseRule{},
    .RIGHT_BRACE = ParseRule{},
    .COMMA       = ParseRule{},
    .DOT         = ParseRule{ .infix = Compiler.dot, .precedence = Precedence.PREC_CALL },
    .MINUS       = ParseRule{ .prefix = Compiler.unary, .infix = Compiler.binary, .precedence = Precedence.PREC_TERM },
    .PLUS        = ParseRule{ .infix = Compiler.binary, .precedence = Precedence.PREC_TERM },
    .SEMICOLON   = ParseRule{},
    .SLASH       = ParseRule{ .infix = Compiler.binary, .precedence = Precedence.PREC_FACTOR },
    .STAR        = ParseRule{ .infix = Compiler.binary, .precedence = Precedence.PREC_FACTOR },
    .BANG        = ParseRule{ .prefix = Compiler.unary },
    .BANG_EQUAL  = ParseRule{ .infix = Compiler.binary, .precedence = Precedence.PREC_EQUALITY },
    .EQUAL       = ParseRule{},
    .EQUAL_EQUAL = ParseRule{ .infix = Compiler.binary, .precedence = Precedence.PREC_EQUALITY },
    .GREATER       = ParseRule{ .infix = Compiler.binary, .precedence = Precedence.PREC_COMPARISON },
    .GREATER_EQUAL = ParseRule{ .infix = Compiler.binary, .precedence = Precedence.PREC_COMPARISON },
    .LESS          = ParseRule{ .infix = Compiler.binary, .precedence = Precedence.PREC_COMPARISON },
    .LESS_EQUAL    = ParseRule{ .infix = Compiler.binary, .precedence = Precedence.PREC_COMPARISON },
    .IDENTIFIER    = ParseRule{ .prefix = Compiler.variable },
    .STRING        = ParseRule{ .prefix = Compiler.string },
    .NUMBER        = ParseRule{ .prefix = Compiler.number },
    .AND           = ParseRule{ .infix = Compiler.and_, .precedence = Precedence.PREC_AND },
    .CLASS         = ParseRule{},
    .ELSE          = ParseRule{},
    .FALSE         = ParseRule{ .prefix = Compiler.literal },
    .FOR           = ParseRule{},
    .FUN           = ParseRule{},
    .IF            = ParseRule{},
    .NIL           = ParseRule{ .prefix = Compiler.literal },
    .OR            = ParseRule{ .infix = Compiler.or_, .precedence = Precedence.PREC_OR },
    .PRINT         = ParseRule{},
    .RETURN        = ParseRule{},
    .SUPER         = ParseRule{ .prefix = Compiler.super },
    .THIS          = ParseRule{ .prefix = Compiler.this },
    .TRUE          = ParseRule{ .prefix = Compiler.literal },
    .VAR           = ParseRule{},
    .WHILE         = ParseRule{},
    .ERROR         = ParseRule{},
    .EOF           = ParseRule{},
});

/// Chooses next parse function based on precedence in Pratt parser table.
/// 
fn parse_precedence(self: *Compiler, precedence: Precedence) Allocator.Error!void {
    self.advance();
    const prefix_rule = Compiler.get_rule(self.parser.previous.type).prefix orelse {
        self.report_error("Expect expression.");
        return;
    };
    const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.PREC_ASSIGNMENT);
    try prefix_rule(self, can_assign);

    const precedence_int = @intFromEnum(precedence);
    while (precedence_int <= @intFromEnum(Compiler.get_rule(self.parser.current.type).precedence)) {
        self.advance();
        const infix_rule = Compiler.get_rule(self.parser.previous.type).infix orelse {
            @panic(std.fmt.allocPrint(self.gc.as_allocator(), "No infix expression found for {s}\n", .{self.parser.previous.type.name()}) catch unreachable);
        };
        try infix_rule(self, can_assign);
    }
    if (can_assign and self.match_tok(TokenType.EQUAL)) {
        self.report_error("Invalid assignment target.");
    }
}

// Parses an identifier as a variable.
//
fn parse_variable(self: *Compiler, error_message: []const u8) Allocator.Error!u8 {
    self.consume(TokenType.IDENTIFIER, error_message);

    self.declare_variable();
    if (self.scope_depth > 0) return 0;

    return self.identifier_constant(&self.parser.previous);
}

// Marks a local as initialized.
//
fn mark_initialized(self: *Compiler) void {
    if (self.scope_depth == 0) return;
    self.locals[self.local_count - 1].depth = @as(i32, @intCast(self.scope_depth));
}


// Returns an identifier constant.
//
fn identifier_constant(self: *Compiler, name: *Token) Allocator.Error!u8 {
    return try self.make_constant(Value.obj((try self.gc.copy_string(name.content, name.len)).widen()));
}

// Do the identifiers equal?
//
fn identifiers_equal(a: *const Token, b: *const Token) bool {
    return std.mem.eql(u8, a.content[0..a.len], b.content[0..b.len]);
}

// Adds a local
//
fn add_local(self: *Compiler, name: Token) void {
    if (self.local_count == std.math.maxInt(u8)) {
        self.report_error("Too many local variables in function.");
        return;
    }
    var local = &self.locals[self.local_count];
    self.local_count += 1;
    local.name = name;
    local.depth = -1;
    local.is_captured = false;
}

// Resolves a local.
//
fn resolve_local(self: *Compiler, name: *Token) i32 {
    const locals = self.locals[0..self.local_count];
    var i = @as(i64, @intCast(self.local_count)) - 1;
    while (i >= 0) : (i -= 1) {
        const local = locals[@as(usize, @intCast(i))];
        if (identifiers_equal(name, &local.name)) {
            if (local.depth == -1) {
                self.report_error("Can't read local variable in its own initializer.");
            }
            return @as(i32, @intCast(i));
        }
    }
    return -1;
}

// Resolves an upvalue.
//
fn resolve_upvalue(self: *Compiler, name: *Token) i32 {
    const enclosing = self.enclosing orelse return -1;

    const str = name.content[0..name.len];
    _ = str;

    const local = enclosing.resolve_local(name);
    if (local != -1) {
        enclosing.locals[@as(usize, @intCast(local))].is_captured = true;
        return self.add_upvalue(@as(u8, @intCast(local)), true);
    }

    const upvalue = enclosing.resolve_upvalue(name);
    if (upvalue != -1) {
        return self.add_upvalue(@as(u8, @intCast(upvalue)), false);
    }
    return -1;
}

// Adds an upvalue.
//
// # Errors
//
// Returns a runtime error if too many closure variables.
//
fn add_upvalue(self: *Compiler, index: u8, is_local: bool) i32 {
    const upvalue_count = self.function.upvalue_count;

    var i: u32 = 0;
    while (i < upvalue_count) {
        const upvalue = &self.upvalues[i];
        if (upvalue.index == index and upvalue.is_local == is_local) {
            return @as(i32, @intCast(i));
        }
        i = i + 1;
    }

    if (upvalue_count == std.math.maxInt(u8)) {
        self.report_error("Too many closure variables in function.");
        return 0;
    }

    self.upvalues[upvalue_count].is_local = is_local;
    self.upvalues[upvalue_count].index = index;
    self.function.upvalue_count += 1;
    return @as(i32, @intCast(upvalue_count));
}

// Declares a variable.
//
fn declare_variable(self: *Compiler) void {
    if (self.scope_depth == 0) return;

    const name = self.parser.previous;
    const locals = self.locals[0..self.local_count];
    var i = @as(i64, @intCast(self.local_count)) - 1;
    while (i >= 0) : (i -= 1) {
        const local = locals[@as(usize, @intCast(i))];
        if (local.depth != -1 and local.depth < self.scope_depth) {
            break;
        }

        if (identifiers_equal(&name, &local.name)) {
            self.report_error("Already a variable with this name in this scope.");
        }
    }
    self.add_local(name);
}

// Defines a global variable.
//
fn define_variable(self: *Compiler, global: u8) Allocator.Error!void {
    if (self.scope_depth > 0) {
        self.mark_initialized();
        return;
    }
    return self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_DEFINE_GLOBAL), global });
}

/// Parse And operator.
/// 
fn and_(self: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const end_jump = try self.emit_jump(.OP_JUMP_IF_FALSE);

    try self.emit_op(.OP_POP);
    try self.parse_precedence(.PREC_AND);

    try self.patch_jump(end_jump);
}

/// Parse Or operator.
/// 
fn or_(self: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const else_jump = try self.emit_jump(.OP_JUMP_IF_FALSE);
    const end_jump = try self.emit_jump(.OP_JUMP);

    try self.patch_jump(else_jump);
    try self.emit_op(.OP_POP);

    try self.parse_precedence(.PREC_OR);
    try self.patch_jump(end_jump);
}

/// Compiler.  Reads tokens from Scanner, processes it and emits bytecode to the current chunk.
/// 
const Compiler = @This();

enclosing: ?*Compiler,
class_compiler: ?*ClassCompiler,
scanner: *Scanner,
parser: *Parser,
local_count: u32,
scope_depth: u32,
upvalues: [std.math.maxInt(u8)]Upvalue,
locals: [std.math.maxInt(u8)]Local,
function: *Obj.Function,
function_type: FunctionType,
test_mode: bool,
gc: *GC,

/// Initializes a Compiler.
/// 
pub fn init(gc: *GC, enclosing: ?*Compiler, scanner: *Scanner, parser: *Parser, class_compiler: ?*ClassCompiler, function_type: FunctionType) Allocator.Error!Compiler {
    var function = try gc.alloc_obj(Obj.Function);
    try function.init(gc.as_allocator());
    var self = Compiler{
        .gc = gc,
        .scanner = scanner,
        .parser = parser,
        .class_compiler = class_compiler,
        .function_type = function_type,
        .function = function,
        .enclosing = enclosing,
        .upvalues = undefined,
        .locals = undefined,
        .local_count = 0,
        .scope_depth = 0,
        .test_mode = false,
    };

    if (function_type != FunctionType.Script) {
        self.function.name = try self.gc.copy_string(parser.previous.content, parser.previous.len);
    }

    var local = &self.locals[0];
    self.local_count += 1;
    local.depth = 0;
    local.name.content = "";
    local.name.len = 0;
    local.is_captured = false;

    if (function_type != FunctionType.Function) {
        local.name.content = "this";
        local.name.len = 4;
    }
    else {
        local.name.content = "";
        local.name.len = 0;
    }

    return self;
}

/// Compiles a source string into Chunk bytecode.
/// 
pub fn compile(self: *Compiler) Allocator.Error!?*Obj.Function {
    self.advance();
    while (!self.match_tok(.EOF)) {
        try self.declaration();
    }
    const function = try self.end();

    if (self.parser.had_error) {
        return null;
    } 
    else {
        return function;
    }
}

/// Parses a declaration, and statements.  If panic mode, synchronizes to next statement.  Returns any runtime errors from
/// statement.
///
fn declaration(self: *Compiler) Allocator.Error!void {
    if (self.match_tok(TokenType.CLASS)) {
        try self.class_declaration();
    } 
    else if (self.match_tok(TokenType.FUN)) {
        try self.fun_declaration();
    }
    else if (self.match_tok(TokenType.VAR)) {
        try self.var_declaration();
    } 
    else {
        try self.statement();
    }

    if (self.parser.panic_mode) {
        self.synchronize();
    }
}


/// Parses a Class declaration.
/// 
/// Example:
/// 
/// ' Class Brioche {}
/// 
/// # Errors
/// 
/// Returns a runtime error if missing class name.
/// Returns a runtime error if missing superclass name (when using <).
/// Returns a runtime error if class inherits from itself.
/// Returns a runtime error if missing open brace before class body.
/// Returns a runtime error if missing closing brace after class body.
/// 
fn class_declaration(self: *Compiler) Allocator.Error!void {
    self.consume(TokenType.IDENTIFIER, "Expect class name.");
    var class_name = self.parser.previous;
    const name_constant = try self.identifier_constant(&class_name);
    self.declare_variable();

    try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_CLASS), name_constant });
    try self.define_variable(name_constant);

    var class_compiler = ClassCompiler{.enclosing = self.class_compiler,};
    self.class_compiler = &class_compiler;
    defer {
        self.class_compiler = class_compiler.enclosing;
    }

    if (self.match_tok(.LESS)) {
        self.consume(.IDENTIFIER, "Expect superclass name.");
        try self.variable(false);

        if (identifiers_equal(&class_name, &self.parser.previous)) {
            self.report_error("A class cannot inherit from itself.");
        }

        self.begin_scope();
        self.add_local(Token.synthetic("super"));
        try self.define_variable(0);

        try self.named_variable(&class_name, false);
        try self.emit_op(.OP_INHERIT);
        class_compiler.has_superclass = true;
    }

    try self.named_variable(&class_name, false);
    self.consume(TokenType.LEFT_BRACE, "Expect '{' before class body.");
    while (!self.check(TokenType.RIGHT_BRACE) and !self.check(TokenType.EOF)) {
        try self.method();
    }
    self.consume(TokenType.RIGHT_BRACE, "Expect '}' after class body.");
    try self.emit_op(.OP_POP);

    if (class_compiler.has_superclass) {
        try self.end_scope();
    }
}

/// Parses a variable declaration.
/// 
/// Example:  var abc = "abc";
/// 
/// # Errors
/// 
/// Returns a runtime error if missing variable name.
/// Returns a runtime error if missing semicolon after variable declaration.
///  
fn var_declaration(self: *Compiler) Allocator.Error!void {
    const global = try self.parse_variable("Expect variable name.");
    if (self.match_tok(TokenType.EQUAL)) {
        try self.expression();
    }
    else {
        try self.emit_op(.OP_NIL);
    }
    self.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.");
    try self.define_variable(global);
}

/// Parses a function declaration.
/// 
/// Example:  fun abc(a, b, c) {}
/// 
/// # Errors
/// 
/// Returns a runtime error if missing variable name.
/// 
fn fun_declaration(self: *Compiler) Allocator.Error!void {
    const global = try self.parse_variable("Expect function name.");
    self.mark_initialized();
    try self.func(FunctionType.Function);
    try self.define_variable(global);
}

// Parses a statement.
//
// Examples:
//
// print "123";
// if (true) { print "123"; }
// while (true) { print "ABC"; }
// 1 + 2;
//
fn statement(self: *Compiler) Allocator.Error!void {
    if (self.match_tok(TokenType.PRINT)) {
        try self.print_statement();
    }
    else if (self.match_tok(TokenType.IF)) {
        try self.if_statement();
    } 
    else if (self.match_tok(TokenType.RETURN)) {
        try self.return_statement();
    }
    else if (self.match_tok(TokenType.WHILE)) {
        try self.while_statement();
    } 
    else if (self.match_tok(TokenType.FOR)) {
        try self.for_statement();
    } 
    else if (self.match_tok(TokenType.LEFT_BRACE)) {
        self.begin_scope();
        try self.block();
        try self.end_scope();
    } 
    else {
        try self.expression_statement();
    }
}

/// Parses an expression statement.
/// 
/// Example: 1 + 2;
/// 
/// # Errors
/// 
/// Returns runtime error if missing semicolon after expression.
/// 
fn expression_statement(self: *Compiler) Allocator.Error!void {
    try self.expression();
    self.consume(TokenType.SEMICOLON, "Expect ';' after expression.");
    try self.emit_op(.OP_POP);
}

/// Parses a for statement.
/// 
/// Example: for (var test = 1; test < 5; test = test + 1) { print 123; }
/// 
/// # Errors
/// 
/// Returns a runtime error if no opening parenthesis.
/// Returns a runtime error if no semicolon after loop condition.
/// Returns a runtime error if not closing parenthesis after clauses.
/// 
fn for_statement(self: *Compiler) Allocator.Error!void {
    self.begin_scope();

    self.consume(.LEFT_PAREN, "Expect '(' after 'for'.");
    if (self.match_tok(.SEMICOLON)) {
        // No initializer.
    } 
    else if (self.match_tok(.VAR)) {
        try self.var_declaration();
    } 
    else {
        try self.expression_statement();
    }

    var loop_start = self.current_chunk().code.items.len;
    var exit_jump: i64 = -1;
    if (!self.match_tok(.SEMICOLON)) {
        try self.expression();
        self.consume(.SEMICOLON, "Expect ';' after loop condition.");

        exit_jump = @as(i64, @intCast(try self.emit_jump(.OP_JUMP_IF_FALSE)));
        try self.emit_op(.OP_POP);
    }

    if (!self.match_tok(.RIGHT_PAREN)) {
        const body_jump = try self.emit_jump(.OP_JUMP);
        const increment_start = self.current_chunk().code.items.len;
        try self.expression();
        try self.emit_op(.OP_POP);
        self.consume(.RIGHT_PAREN, "Expect ')' after for clauses.");

        try self.emit_loop(loop_start);
        loop_start = increment_start;
        try self.patch_jump(body_jump);
    }

    try self.statement();
    try self.emit_loop(loop_start);

    if (exit_jump != -1) {
        try self.patch_jump(@as(usize, @intCast(exit_jump)));
        try self.emit_op(.OP_POP); // Condition
    }

    try self.end_scope();
}

/// Parses an if statement.
/// 
/// # Example: if (abc == true) { // code } else { // code }
/// 
/// # Errors
/// 
/// Returns a runtime error if missing begin parenthesis.
/// Returns a runtime error if missing end parenthesis after expression.
/// 
fn if_statement(self: *Compiler) Allocator.Error!void {
    self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
    try self.expression();
    self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");

    const then_jump = try self.emit_jump(.OP_JUMP_IF_FALSE);
    try self.emit_op(.OP_POP);
    try self.statement();

    const else_jump = try self.emit_jump(.OP_JUMP);

    try self.patch_jump(then_jump);
    try self.emit_op(.OP_POP);

    if (self.match_tok(TokenType.ELSE)) {
        try self.statement();
    }
    try self.patch_jump(else_jump);
}

// Matches a token.  Returns false if not matched.  Otherwise advances and returns true.
//
fn match_tok(self: *Compiler, ty: TokenType) bool {
    if (!self.check(ty)) {
        return false;
    }
    self.advance();
    return true;
}

// Checks if token matches.
//
fn check(self: *Compiler, ty: TokenType) bool {
    return self.parser.current.type == ty;
}

/// Moves the parser to the next token.  Skips any errors encountered.
/// 
fn advance(self: *Compiler) void {
    self.parser.previous = self.parser.current;

    while (true) {
        const token = self.scanner.scan_token();
        if (token.type != TokenType.ERROR) {
            self.parser.current = token;
            break;
        }
        self.error_at_current(token.content[0..token.len]);
    }
}

/// Displays an error at the current token.
/// 
fn error_at_current(self: *Compiler, message: []const u8) void {
    self.error_at(self.parser.current, message);
}

/// Displays an error at previous token.
/// 
fn report_error(self: *Compiler, message: []const u8) void {
    self.error_at(self.parser.previous, message);
}

/// Displays error at specified token.
/// 
fn error_at(self: *Compiler, token: Token, message: []const u8) void {
    if (self.parser.panic_mode) return;
    self.parser.panic_mode = true;

    debug.print("[line {d}] Error", .{token.line});

    if (token.type == .EOF) {
        debug.print(" at end", .{});
    } 
    else if (token.type == .ERROR) {
        // Nothing.
    } 
    else {
        debug.print(" at '{s}'", .{token.content[0..token.len]});
    }

    debug.print(": {s}\n", .{message});
    self.parser.had_error = true;
}

// Synchronizes to beginning of next statement to avoid excessive cascading of errors.
//
fn synchronize(self: *Compiler) void {
    self.parser.panic_mode = false;

    while (self.parser.current.type != .EOF) {
        if (self.parser.previous.type == .SEMICOLON) return;

        switch (self.parser.current.type) {
            .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
            else => {},
        }
        self.advance();
    }
}

/// Checks the current token.  If it matches, then advances the parser, otherwise displays
/// an error.
/// 
fn consume(self: *Compiler, comptime token_type: TokenType, message: []const u8) void {
    if (self.parser.current.type == token_type) {
        self.advance();
    } 
    else {
        self.error_at_current(message);
    }
}

/// Writes an opcode to the current chunk.
/// 
fn emit_op(self: *Compiler, op: Opcode) Allocator.Error!void {
    try self.emit_byte(@intFromEnum(op));
}

/// Writes a byte to the current chunk.
/// 
fn emit_byte(self: *Compiler, byte: u8) Allocator.Error!void {
    try self.current_chunk().write_byte(self.gc.as_allocator(), byte, self.parser.previous.line);
}

/// Writes a "short" to the current chunk.
/// 
fn emit_u16(self: *Compiler, val: u16) Allocator.Error!void {
    try self.current_chunk().write_u16(self.gc.as_allocator(), val, self.parser.previous.line);
}

/// Writes bytes to the current chunk.
/// 
fn emit_bytes(self: *Compiler, comptime n: usize, bytes: *const [n]u8) Allocator.Error!void {
    if (comptime n != bytes.len) {
        @compileError("emit_bytes: n != bytes.len");
    }
    comptime var i = 0;
    inline while (i < n) : (i += 1) {
        try self.emit_byte(bytes[i]);
    }
}

/// Writes a loop opcode to the current chunk.
/// 
/// # Errors
/// 
/// Reports a runtime error if the loop body is larger than 16 bits.
///
fn emit_loop(self: *Compiler, loop_start: usize) Allocator.Error!void {
    try self.emit_op(.OP_LOOP);

    const offset = self.current_chunk().code.items.len - loop_start + 2;
    if (offset > std.math.maxInt(u16)) {
        self.report_error("Loop body too large.");
    }
    try self.emit_u16(@as(u16, @truncate(offset)));
}

/// Writes a jump to the current chunk, including placeholder for backpatching.
/// 
fn emit_jump(self: *Compiler, op: Opcode) Allocator.Error!usize {
    try self.emit_op(op);
    try self.emit_byte(0xff);
    try self.emit_byte(0xff);
    return self.current_chunk().code.items.len - 2;
}

/// Writes Constant opcode and constant to the current chunk.
///
fn emit_constant(self: *Compiler, value: Value) Allocator.Error!void {
    const bytes = &[_]u8{ @intFromEnum(Opcode.OP_CONSTANT), try self.make_constant(value) };
    try self.emit_bytes(bytes.len, bytes);
}

/// Back-patches a jump operation.
/// 
/// # Errors
/// 
/// Reports a runtime error if the offset is larger than 16 bits.
/// 
fn patch_jump(self: *Compiler, offset: usize) Allocator.Error!void {
    const jump_usize = self.current_chunk().code.items.len - offset - 2;

    if (jump_usize > std.math.maxInt(u16)) {
        self.report_error("Too much code to jump over.");
    }

    const jump = @as(u16, @intCast(jump_usize));

    self.current_chunk().code.items[offset] = @as(u8, @truncate(jump >> 8));
    self.current_chunk().code.items[offset + 1] = @as(u8, @truncate(jump));
}

/// Adds a constant to the chunk's values.
/// 
fn make_constant(self: *Compiler, value: Value) Allocator.Error!u8 {
    const constant = try self.current_chunk().add_constant(self.gc.as_allocator(), value);
    if (constant > std.math.maxInt(u8)) {
        self.report_error("Too many constants in one chunk.");
        return 0;
    }
    return constant;
}

/// Writes a Return opcode to current chunk.
/// 
fn emit_return(self: *Compiler) Allocator.Error!void {
    if (self.function_type == FunctionType.Initializer) {
        try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_GET_LOCAL), 0 });
    } else {
        try self.emit_op(.OP_NIL);
    }
    try self.emit_op(.OP_RETURN);
}

/// Returns the current chunk.
/// 
inline fn current_chunk(self: *Compiler) *Chunk {
    return &self.function.chunk;
}

/// At end of compile...
/// 
/// Emits a OP_RETURN.
/// Displays current chunk if enabled.
/// 
fn end(self: *Compiler) Allocator.Error!*Obj.Function {
    try self.emit_return();
    var function = self.function;
    if (DEBUG_PRINT_CODE and !self.parser.had_error) {
        self.current_chunk().disassemble(if (function.name) |name| name.chars[0..name.len] else "script");
    }
    return function;
}

// Begins a new scope.
//
fn begin_scope(self: *Compiler) void {
    self.scope_depth += 1;
}

// End the current scope.  Closes upvalues if necessary.
//
fn end_scope(self: *Compiler) Allocator.Error!void {
    self.scope_depth -= 1;

    while (self.local_count > 0 and self.locals[self.local_count - 1].depth > self.scope_depth) {
        if (self.locals[self.local_count - 1].is_captured) {
            try self.emit_op(.OP_CLOSE_UPVALUE);
        } 
        else {
            try self.emit_op(.OP_POP);
        }
        self.local_count -= 1;
    }
}

/// Parses an expression.
/// 
fn expression(self: *Compiler) Allocator.Error!void {
   try self.parse_precedence(.PREC_ASSIGNMENT);
}

/// Parses a block.
///
/// # Example:  
/// 
/// ' {
/// '    var abc = 123;
/// ' } 
/// 
/// # Errors
/// 
/// Returns a runtime error if block is not ended with right brace.
/// 
fn block(self: *Compiler) Allocator.Error!void {
    while (!self.check(.RIGHT_BRACE) and !self.check(.EOF)) {
        _ = try self.declaration();
    }

    self.consume(.RIGHT_BRACE, "Expect '}' after block.");
}

/// Parses a function.
/// 
/// Example: 
/// 
/// ' fun(a, b, c) { 
/// '    print 123;
/// '    return true;
/// ' }
/// 
/// # Errors
/// 
/// Returns a runtime error if no opening parentheses after function name.
/// Returns a runtime error if more than 255 parameters.
/// Returns a runtime error if no parameter name.
/// Returns a runtime error if no closing parenthesis after function name.
/// Returns a runtime error if no opening brace for body.
/// 
fn func(self: *Compiler, function_type: FunctionType) Allocator.Error!void {
    var compiler = try Compiler.init(self.gc, self, self.scanner, self.parser, self.class_compiler, function_type);
    compiler.begin_scope();

    compiler.consume(.LEFT_PAREN, "Expect '(' after function name.");
    if (!compiler.check(.RIGHT_PAREN)) {
        while (true) {
            if (compiler.function.arity == std.math.maxInt(u8)) {
                compiler.error_at_current("Cannot have more than 255 parameters.");
                break;
            }
            compiler.function.arity += 1;
            const constant = try compiler.parse_variable("Expect parameter name.");
            try compiler.define_variable(constant);
            if (!compiler.match_tok(.COMMA)) {
                break;
            }
        }
    }
    compiler.consume(.RIGHT_PAREN, "Expect ')' after function name.");
    compiler.consume(.LEFT_BRACE, "Expect '{' after function name.");
    try compiler.block();

    const fun = try compiler.end();
    try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_CLOSURE), try self.make_constant(Value.obj(fun.widen())) });
    var i: usize = 0;
    while (i < fun.upvalue_count) : (i += 1) {
        try self.emit_byte(if (compiler.upvalues[i].is_local) 1 else 0);
        try self.emit_byte(compiler.upvalues[i].index);
    }
}

/// Parses a method.
/// 
/// # Example
/// 
/// ' class Test {
/// '     method(arg1, arg2) {
/// '         print "ABC";      
/// '     }
/// ' }
/// 
/// # Errors 
/// 
/// Returns a runtime error if missing method name.
/// 
fn method(self: *Compiler) Allocator.Error!void {
    self.consume(TokenType.IDENTIFIER, "Expect method name.");
    const constant = try self.identifier_constant(&self.parser.previous);

    var ty = FunctionType.Method;
    if (std.mem.eql(u8, self.parser.previous.content[0..self.parser.previous.len], "init")) {
        ty = FunctionType.Initializer;
    }
    try self.func(ty);
    try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_METHOD), constant });
}

/// Parses a print statement.
///
/// Example: print 123;
/// 
/// # Error
/// 
/// Returns a runtime error if statement not ended with semicolon.
/// 
fn print_statement(self: *Compiler) Allocator.Error!void {
    try self.expression();
    self.consume(TokenType.SEMICOLON, "Expect ';' after value.");
    try self.emit_op(.OP_PRINT);
}

/// Parses a return statement.
/// 
/// Example:  return 123;
/// 
/// # Errors
/// 
/// Returns a runtime error is not in a function.
/// Returns a runtime error if returning value from initializer.
/// Returns a runtime error if no semicolon after return value.
/// 
fn return_statement(self: *Compiler) Allocator.Error!void {
    if (self.function_type == FunctionType.Script) {
        self.report_error("Cannot return from top-level code.");
    }

    if (self.match_tok(TokenType.SEMICOLON)) {
        try self.emit_return();
    } 
    else {
        if (self.function_type == FunctionType.Initializer) {
            self.report_error("Cannot return a value from an initializer.");
        }

        try self.expression();
        self.consume(TokenType.SEMICOLON, "Expect ';' after return value.");
        try self.emit_op(.OP_RETURN);
    }
}

/// Parses a while statement.
/// 
/// Example:  while (true) { print "ABC"; }
/// 
/// # Errors
/// 
/// Return a runtime error if missing beginning parenthesis before condition.
/// Return a runtime_error if missing ending parenthesis after condition.
/// 
fn while_statement(self: *Compiler) Allocator.Error!void {
    const loop_start = self.current_chunk().code.items.len;
    self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
    try self.expression();
    self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");

    const exit_jump = try self.emit_jump(.OP_JUMP_IF_FALSE);
    try self.emit_op(.OP_POP);
    try self.statement();
    try self.emit_loop(loop_start);

    try self.patch_jump(exit_jump);
    try self.emit_op(.OP_POP);
}

/// Parses grouping.
/// 
fn grouping(self: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;

    try self.expression();
    self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
}

/// Parses a number.
/// 
fn number(self: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;

    const value = std.fmt.parseFloat(f64, self.parser.previous.content[0..self.parser.previous.len]) catch {
        self.error_at(self.parser.previous, "Invalid number");
        return;
    };
    try self.emit_constant(Value.number(value));
}

/// Parses a string.
/// 
fn string(self: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const obj_string = try self.gc.copy_string(self.parser.previous.content + 1, self.parser.previous.len - 2);

    try self.emit_constant(Value.obj(obj_string.widen()));
}

// Parses a variable.
//
fn variable(self: *Compiler, can_assign: bool) Allocator.Error!void {
    try self.named_variable(&self.parser.previous, can_assign);
}

/// Parse this.
/// 
/// # Example
/// 
/// ' class Test {
/// '    method() {
/// '        this.field = "ABC";
/// '        print this.field;
/// '    }
/// ' }
/// 
/// # Errors
/// 
/// Returns a runtime error if used outside of a class.
/// 
fn this(self: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;

    if (self.class_compiler == null) {
        self.report_error("Can't use 'this' outside of a class.");
        return;
    }
    try self.variable(false);
}

// Parses a super invocation.
//
// # Example
//
// ' class Animal {
// '    talk() {
// '        print "The " + this.name + says: ";
// '    }
// ' }
// '
// ' class Cat < Animal {
// '    talk() {
// '       this.name = "Cat";
// '       super.talk();
// ' }    
//
// # Errors
// Returns a runtime error if used in a class with no superclass.
// Returns a runtime error if used outside of a class.
//
fn super(self: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    if (self.class_compiler) |class_compiler| {
        if (!class_compiler.has_superclass) {
            self.report_error("Can't use 'super' in a class with no superclass.");
        }
    } 
    else {
        self.report_error("Can't use 'super' outside of a class.");
    }

    self.consume(.DOT, "Expect '.' after 'super'.");
    self.consume(.IDENTIFIER, "Expect superclass method name.");
    const name = try self.identifier_constant(&self.parser.previous);

    var this_ = Token.synthetic("this");
    var super_ = Token.synthetic("super");

    try self.named_variable(&this_, false);

    if (self.match_tok(.LEFT_PAREN)) {
       const arg_count = try self.argument_list();
        try self.named_variable(&super_, false);
        try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_INVOKE_SUPER), name });
        try self.emit_byte(arg_count);
    } 
    else {
        try self.named_variable(&super_, false);
        try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_GET_SUPER), name });
    }
}

// Parses a variable and emits either get or set.
//
fn named_variable(self: *Compiler, name: *Token, can_assign: bool) Allocator.Error!void {
    var get_op: Opcode = undefined;
    var set_op: Opcode = undefined;
    var arg = self.resolve_local(name);

    if (arg != -1) {
        get_op = .OP_GET_LOCAL;
        set_op = .OP_SET_LOCAL;
    } 
    else {
        arg = self.resolve_upvalue(name);
        if (arg != -1) {
            get_op = .OP_GET_UPVALUE;
            set_op = .OP_SET_UPVALUE;
        } 
        else {
            arg = try self.identifier_constant(name);
            get_op = .OP_GET_GLOBAL;
            set_op = .OP_SET_GLOBAL;
        }
    }

    if (can_assign and self.match_tok(.EQUAL)) {
        debug.print("expression()", .{});
        try self.expression();
        try self.emit_bytes(2, &[_]u8{ @intFromEnum(set_op), @as(u8, @intCast(arg)) });
    } 
    else {
        try self.emit_bytes(2, &[_]u8{ @intFromEnum(get_op), @as(u8, @intCast(arg)) });
    }
}

/// Parses unary expression.
/// 
fn unary(self: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;

    const token_type = self.parser.previous.type;
    try self.parse_precedence(.PREC_UNARY);
     
    switch (token_type) {
        .BANG  => try self.emit_op(.OP_NOT),
        .MINUS => try self.emit_op(.OP_NEGATE),
        else => unreachable,
    }
}

/// Parses binary expression.
/// 
fn binary(self: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;

    const operator_type = self.parser.previous.type;
    const rule = Compiler.get_rule(operator_type);
    try self.parse_precedence(@as(Precedence, @enumFromInt(@intFromEnum(rule.precedence) + 1)));

    switch (operator_type) {
        .BANG_EQUAL    => try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_EQUAL), @intFromEnum(Opcode.OP_NOT) }),
        .EQUAL_EQUAL   => try self.emit_op(Opcode.OP_EQUAL),
        .GREATER       => try self.emit_op(Opcode.OP_GREATER),
        .GREATER_EQUAL => try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_LESS), @intFromEnum(Opcode.OP_NOT) }),
        .LESS          => try self.emit_op(Opcode.OP_LESS),
        .LESS_EQUAL    => try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_GREATER), @intFromEnum(Opcode.OP_NOT) }),

        .PLUS  => try self.emit_op(.OP_ADD),
        .MINUS => try self.emit_op(.OP_SUBTRACT),
        .STAR  => try self.emit_op(.OP_MULTIPLY),
        .SLASH => try self.emit_op(.OP_DIVIDE),
        else => unreachable,
    }
}

/// Parses a call statement.
/// 
fn call(self: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const arg_count = try self.argument_list();
    try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_CALL), arg_count });
}

/// Parses get and set properties.
/// 
/// # Examples
/// 
/// ' test.field1 = "ABC";
/// ' print test.field1;
/// 
/// # Errors
/// 
/// Return runtime error if no property name after dot.
/// 
fn dot(self: *Compiler, can_assign: bool) Allocator.Error!void {
    self.consume(TokenType.IDENTIFIER, "Expect property name after '.'.");
    const name = try self.identifier_constant(&self.parser.previous);

    if (can_assign and self.match_tok(TokenType.EQUAL)) {
        try self.expression();
        try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_SET_PROPERTY), name });
    } 
    else if (self.match_tok(TokenType.LEFT_PAREN)) {
        const arg_count = try self.argument_list();
        try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_INVOKE), name });
        try self.emit_byte(arg_count);
    } 
    else {
        try self.emit_bytes(2, &[_]u8{ @intFromEnum(Opcode.OP_GET_PROPERTY), name });
    }
}

/// Parses an argument list.
/// 
fn argument_list(self: *Compiler) Allocator.Error!u8 {
    var arg_count: u8 = 0;

    if (!self.check(.RIGHT_PAREN)) {
        while (true) {
            try self.expression();
            if (arg_count == std.math.maxInt(u8)) {
                self.report_error("Can't have more than 255 arguments.");
            }
            arg_count += 1;
            if (!self.match_tok(.COMMA)) {
                break;
            }
        }
    }
    self.consume(.RIGHT_PAREN, "Expect ')' after arguments.");

    return arg_count;
}

/// Parses a literal expression.
/// 
fn literal(self: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;

    switch (self.parser.previous.type) {
        .TRUE  => try self.emit_constant(Value.boolean(true)),
        .FALSE => try self.emit_constant(Value.boolean(false)),
        .NIL   => try self.emit_constant(Value.nil()),
        else => {},
    }
}

fn assertCompile(source: []const u8) !void {
    debug.print("\n", .{});
    var gc: *GC = try Test.alloc.create(GC);
    try gc.init(Test.alloc);

    var parser = Compiler.init_parser();
    var scanner = Scanner.init(source);
    var compiler = try Compiler.init(gc, null, &scanner, &parser, null, FunctionType.Script);

    _ = try compiler.compile();
}

// Tests parsing Nil.
//
test "Compile Nil" {
    try assertCompile("nil;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_RETURN

    printStatus("Compile Nil");    
}

// Tests parsing the boolean "true"
//
test "Compile True" {
    try assertCompile("true;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_RETURN

    printStatus("Compile True");    
}

// Tests parsing the boolean "false".
//
test "Compile False" {
    try assertCompile("false;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_RETURN

    printStatus("Compile True");    
}

// Tests parsing a number.
//
test "Compile a Number" {
    try assertCompile("1;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_RETURN

    printStatus("Compile a Number");
}

// Tests parsing a negative number.
//
test "Compile Negative Number" {
    try assertCompile("-1;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_NEGATE
    // assert chunk has OP_RETURN

    printStatus("Compile Negative Number");
}

// Duplicate?
//
test "Compile Negate Expression" {
    try assertCompile("-1;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_NEGATE
    // assert chunk has OP_RETURN
    
    printStatus("Compile Negate Expression");
}

// Tests parsing a negate expression.
//
test "Compile Negate Boolean Expression" {
    try assertCompile("!true;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_NEGATE
    // assert chunk has OP_RETURN
    
    printStatus("Compile Negate Boolean Expression");
}

// Tests parsing a string.
//
test "Compile String" {
    try assertCompile("\"ABC\";");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_RETURN
    
    printStatus("Compile String");
}

// Tests parsing a grouping.
//
test "Compile Grouping" {
    try assertCompile("(42);");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_RETURN
    
    printStatus("Compile Grouping");
}

// Parsing a grouping thats missing its right parenthesis should return a runtime error.
//
test "Compile Grouping Missing Right Paren" {
    try assertCompile("(42;");

    // assert error message

    printStatus("Compile Grouping Missing Right Paren");
}

// Test parsing plus.
//
test "Compile Plus" {
    try assertCompile("1 + 1;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_CONSTANT
    // assert chunk has OP_ADD
    // assert chunk has OP_RETURN
    
    printStatus("Compile Plus");
}

// Tests parsing minus.
//
test "Compile Minus" {
    try assertCompile("1 - 1;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_CONSTANT
    // assert chunk has OP_SUBTRACT
    // assert chunk has OP_RETURN
    
    printStatus("Compile Minus");
}

// Tests parsing multiply.
//
test "Compile Multiply" {
    try assertCompile("2 * 2;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_CONSTANT
    // assert chunk has OP_MULTIPLY
    // assert chunk has OP_RETURN
    
    printStatus("Compile Multiply");
}

// Tests parsing divide.
//
test "Compile Divide" {
    try assertCompile("2 / 2;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_CONSTANT
    // assert chunk has OP_DIVIDE
    // assert chunk has OP_RETURN
    
    printStatus("Compile Divide");
}

// Tests parsing greater operator.
//
test "Compile Greater" {
    try assertCompile("2 > 1;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_CONSTANT
    // assert chunk has OP_GREATER
    // assert chunk has OP_RETURN
    
    printStatus("Compile Greater");
}

// Tests parsing greater or equal operator.
//
test "Compile Greater Equal" {
    try assertCompile("2 >= 1;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_CONSTANT
    // assert chunk has OP_LESS
    // assert chunk has OP_NOT
    // assert chunk has OP_RETURN
    
    printStatus("Compile Greater Equal");
}

// Tests compiling less operator.
//
test "Compile Less" {
    try assertCompile("1 < 2;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_CONSTANT
    // assert chunk has OP_LESS
    // assert chunk has OP_RETURN
    
    printStatus("Compile Less");
}

// Tests compiling less or equal operator.
//
test "Compile Less Equal" {
    try assertCompile("1 <= 2;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_CONSTANT
    // assert chunk has OP_GREATER
    // assert chunk has OP_NOT
    // assert chunk has OP_RETURN
    
    printStatus("Compile Less Equal");
}

// Tests parsing equal equal operator.
//
test "Compile Equal Equal" {
    try assertCompile("1 == 1;");

    // assert chunk has OP_CONSTANT
    // assert chunk has OP_CONSTANT
    // assert chunk has OP_EQUAL
    // assert chunk has OP_RETURN
    
    printStatus("Compile Equal Equal");
}

// Tests parsing a complex expression.
//
test "Compile Complex" {
    try assertCompile("(1 + 1) / 3 * 1.5 - 2;");

    // assertions!!!

    printStatus("Compile Complex");
}

// Tests the print statement.
//
test "Compile Print" {
    try assertCompile("print 1.2;");

    // assertions!!!
    
    printStatus("Compile Print");
}

// Tests parsing a variable declaration.
//
test "Compile Variable Declaration" {
    try assertCompile("var abc = 123;");

    // assertions!!!
    
    printStatus("Compile Variable Declaration");
}

// Parsing a variable declaration should return a runtime error if missing an identifier.
//
test "Compile Variable Declaration Missing Idenfier" {
    try assertCompile("var 123 = 123;");

    // assertions!!!
    
    printStatus("Compile Variable Declaration Missing Idenfier");
}

// Parsing a variable declaration should return a runtime error if missing a semicolon at end.
//
test "Compile Variable Declaration Missing Semicolon" {
    try assertCompile("var abc = 123");

    // assertions!!!
    
    printStatus("Compile Variable Declaration Missing Idenfier");
}

// Tests parsing a block.
//
test "Compile Block" {
    try assertCompile("{ var abc = 123; }");
    
    // assertions!!!
    
    printStatus("Compile Block");
}

// Parsing a block should return runtime error if no closing brace.
//
test "Compile Block No Right Brace" {
    try assertCompile("{ var abc = 123; ");

    // assertions!!!
    
    printStatus("Compile Block No Right Brace");
}


// Tests parsing an if statement.
//
test "Compile If Statement" {
    try assertCompile("if (true) { var abc = 123; }");

    // assertions!!!
    
    printStatus("Compile If Statement");
}

// Parsing an if statement should return a runtime error if no opening parenthesis.
//
test "Compile If Statement No Left Paren" {
    try assertCompile("if {}");

    // assertions!!!
    
    printStatus("Compile If Statement No Left Paren");
}

// Parsing an if statement should return a runtime error if no closing parenthesis.
//
test "Compile If Statement No Right Paren" {
    try assertCompile("if (true {}");

    // assertions!!!
    
    printStatus("Compile If Statement No Right Paren");
}

// Tests parsing And operator.
//
test "Compile And Statement" {
    try assertCompile("true and false;");

    // assertions!!!
    
    printStatus("Compile And Statement");
}

// Tests parsing Or operator.
//
test "Compile Or Statement" {
    try assertCompile("true or false;");

    // assertions!!!
    
    printStatus("Compile Or Statement");
}

// Tests parsing While statement.
//
test "Compile While Statement" {
    try assertCompile("while (true) { print 123; }");

    // assertions!!!
    
    printStatus("Compile While Statement");
}

// Parsing an while statement should return a runtime error if no opening parenthesis.
//
test "Compile While Statement No Left Paren" {
    try assertCompile("while {}");

    // assertions!!!
    
    printStatus("Compile While Statement No Left Paren");
}

// Parsing an while statement should return a runtime error if no closing parenthesis.
//
test "Compile While Statement No Right Paren" {
    try assertCompile("while (true {}");

    // assertions!!!
    
    printStatus("Compile While Statement No Right Paren");
}

// Tests parsing For statement.
//
test "Compile For Statement" {
    try assertCompile("for (var test = 1; test < 5; test = test + 1) { print 777; }");

    // assertions!!!
    
    printStatus("Compile For Statement");
}

// Parsing an for statement should return a runtime error if no opening parenthesis.
//
test "Compile For Statement No Left Paren" {
    try assertCompile("for {}");

    // assertions!!!
    
    printStatus("Compile For Statement No Left Paren");
}

// Parsing an for statement should return a runtime error if no closing parenthesis.
//
test "Compile For Statement No Right Paren" {
    try assertCompile("for (var test = 1; test < 5; test = test + 1 { print 777; }");

    // assertions!!!
    
    printStatus("Compile For Statement No Right Paren");
}

// Parsing an for statement should return a runtime error if no semicolon.
//
test "Compile For Statement No Semicolon" {
    try assertCompile("for (var test = 1) { print 777; }");

    // assertions!!!
    
    printStatus("Compile For Statement No Semicolon");
}

// Tests parsing Function.
//
test "Compile Function" {
    try assertCompile("fun abc(a, b) { return 1; }");

    // assertions!!!
    
    printStatus("Compile Function");
}

// Parsing function should return a runtime error if no opening parenthesis.
//
test "Compile Function No Left Paren" {
    try assertCompile("fun abc { return 1; }");

    // assertions!!!
    
    printStatus("Compile Function No Left Paren");
}

// Parsing function should return a runtime error if more than 255 arguments.
//
test "Compile Function Too Many Arguments" {
   // TODO!!!
}

// Parsing function should return a runtime error if missing parameter name.
//
test "Compile Function No Parameter Name" {
    try assertCompile("fun abc(a, ) { return 1; }");

    // assertions!!!
    
    printStatus("Compile Function No Parameter Name");
}

// Parsing function should return a runtime error if no closing parenthesis.
//
test "Compile Function No Right Paren" {
    try assertCompile("fun abc(a, b { return 1; }");

    // assertions!!!
    
    printStatus("Compile Function No Right Paren");
}

// Parsing function should return a runtime error if no opening brace.
//
test "Compile Function No Opening Brace" {
    try assertCompile("fun abc(a, b) return 1;");

    // assertions!!!
    
    printStatus("Compile Function No Opening Brace");
}

// Parsing return statement should return a runtime error if not in function.
//
test "Compile Return Not In Function" {
    try assertCompile("return 1;");

    // assertions!!!
    
    printStatus("Compile Return Not In Function");
}

// Parsing return statement should return a runtime error if it doesn't finish with a semicolon.
//
test "Compile Return No Semicolon" {
    try assertCompile("fun abc(a, b) { return 1 }");

    // assertions!!!
    
    printStatus("Compile Return No Semicolon");
}

// Tests parsing call statement.
//
test "Compile Call Statement" {
    try assertCompile("abc(1, 2, 3);");

    // assertions!!!
    
    printStatus("Compile Call Statement");
}

// Tests parsing Class declaration.
//
test "Compile Class Declaration" {
    try assertCompile("class Brioche {}");

    // assertions!!!
    
    printStatus("Compile Class Declaration");
}

// Tests parsing Class Inheritance.
//
test "Compile Class Inheritance" {
    try assertCompile("class Cat < Animal {}");

    // assertions!!!
    
    printStatus("Compile Class Inheritance");
}

// Tests parsing Class No Superclass Name.
//
test "Compile Class No Superclass Name" {
    try assertCompile("class Cat <  {}");

    // assertions!!!
    
    printStatus("Compile Class No Superclass Name");
}

// Should throw a runtime error if attempting to inherit from itself.
//
test "Compile Class Inherit Self" {
    try assertCompile("class Self < Self {}");

    // assertions!!!
    
    printStatus("Compile Class Inherit Self");
}

// Parsing a class declaration should return aa runtime error if it doesn't have a name.
//
test "Compile Class Declaration Missing Name" {
    try assertCompile("class {}");

    // assertions!!!
    
    printStatus("Compile Class Declaration Missing Name");
}

// Parsing a class declaration should return aa runtime error if it doesn't have an opening brace.
//
test "Compile Class Declaration Missing Open Brace" {
    try assertCompile("class Abc }");

    // assertions!!!
    
    printStatus("Compile Class Declaration Missing Open Brace");
}

// Parsing a class declaration should return aa runtime error if it doesn't have an closing brace.
//
test "Compile Class Declaration Missing Close Brace" {
    try assertCompile("class Abc { ");

    // assertions!!!
    
    printStatus("Compile Class Declaration Missing Close Brace");
}

// Tests parsing get property.
//
test "Compile Get Property" {
    try assertCompile("print test.field1;");

    // assertions!!!
    
    printStatus("Compile Get Propery");
}


// Parsing a class declaration should return aa runtime error if it doesn't have a name.
//
test "Compile Get Property Missing Name" {
    try assertCompile("print test.;");

    // assertions!!!
    
    printStatus("Compile Get Propery Missing Name");
}

// Tests parsing set property.
//
test "Compile Set Property" {
    try assertCompile("test.field1 = 123;");

    // assertions!!!
    
    printStatus("Compile Set Propery");
}

// Tests parsing an invoke to a method.
//
test "Compile Invoke" {
    try assertCompile("test.method(a);");

    // assertions!!!
    
    printStatus("Compile Invoke");
}

// Tests parsing a method declaration.
//
test "Compile Method" {
    try assertCompile("class Abc { Def() {} }");

    // assertions!!!
    
    printStatus("Compile Method");
}

// A runtime error should be returned if missing a name when parsing a method declaration.
//
test "Compile Method Missing Name" {
    try assertCompile("class Abc { 123() {} }");

    // assertions!!!
    
    printStatus("Compile Method Missing Name");
}

// Tests parsing use of this.
//
test "Compile This" {
    try assertCompile("class Abc { Def() { this.prop = 123; } }");

    // assertions!!!
    
    printStatus("Compile This");
}

// A runtime error should be returned if using this outside of a class.
//
test "Compile This Outside Class" {
    try assertCompile("this.prop = 123;");

    // assertions!!!
    
    printStatus("Compile This Outside Class");
}

// Tests parsing use of super.
//
test "Compile Super" {
    try assertCompile("class Abc {} class Def < Abc { method() { print super.name; }}");

    // assertions!!!
    
    printStatus("Compile Super");
}

// A runtime error should be returned if used in a class with no superclass.
//
test "Compile Super No Superclass" {
    try assertCompile("class Abc { method() { print super.name; }}");

    // assertions!!!
    
    printStatus("Compile Super No Superclass");
}


// A runtime error should be returned if used outside a class.
//
test "Compile Super Outside Class" {
    try assertCompile("print super.name;");

    // assertions!!!
    
    printStatus("Compile Super Outside Class");
}