const std = @import("std");
const debug = std.debug;
const printValue = @import("chunk.zig").printValue;

// Testing
const _test = @import("test.zig");
const assertTrue = std.testing.expect;
const assertEqual = std.testing.expectEqual;
const assertEquals = _test.assertEquals;
const printPass = _test.printPass;

/// Token Type
/// 
pub const TokenType = enum {
    // Single-character tokens.
    LEFT_PAREN, RIGHT_PAREN,
    LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS,
    SEMICOLON, SLASH, STAR,

    // One or two character tokens.
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,

    // Literals.
    IDENTIFIER, STRING, NUMBER,

    // Keywords.
    AND, CLASS, ELSE, FALSE,
    FOR, FUN, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS,
    TRUE, VAR, WHILE,

    ERROR, EOF,

    /// Gets the name of the token type.
    /// 
    pub fn name(self: TokenType) []const u8 {
        switch (self) {
            inline else => |shape| {
                const str = comptime blk: {
                    const idx = @intFromEnum(shape);
                    const str = @typeInfo(TokenType).Enum.fields[idx].name;
                    break :blk str;
                };
                return str;
            },
        }
    }
};

pub const TokenLen = u47;

/// Token.
/// 
pub const Token = struct {
    type: TokenType,
    line: u16,
    len: u32,
    content: [*]const u8,

    /// Prints out a debug string of token.
    /// 
    pub fn debug(self: Token, writer: anytype) void {
        //writer.print("Token{{type: {s}, content: \"{s}\"}}", .{ @tagName(self.type), self.content[0..self.len] });
        //writer.print("{s} '{s}'", .{ @tagName(self.type), self.content[0..self.len] });
        
        if (self.type == .EOF) {
            writer.print("{d:0>2}  Eof", .{ @intFromEnum(self.type)});           
        }
        else {
            writer.print("{d:0>2}  '{s}'", .{ @intFromEnum(self.type), self.content[0..self.len] });
        }
    }

    /// Creates a synthetic token.
    /// 
    pub fn synthetic(text: []const u8) Token {
        var token: Token = undefined;
        token.content = text.ptr;
        token.len = @as(u32, @intCast(text.len));
        return token;
    }
};

pub const Scanner = @This();

end: [*]const u8,
start: [*]const u8,
current: [*]const u8,
line: u16,

/// Creates a Scanner.
/// 
/// source - string to be scanned. 
/// 
pub fn init(source: []const u8) Scanner {
    var start = @as([*]const u8, @ptrCast(source));
    return Scanner{
        .end = @as([*]const u8, @ptrCast(if (source.len == 0) &start[0] else &source[source.len - 1])),
        .start = start,
        .current = @as([*]const u8, @ptrCast(source)),
        .line = 1,
    };
}

/// Performs scan.
/// 
pub fn scan(self: *Scanner) void {
    var line: u64 = 0;

    while (true) {
        const token = self.scan_token();
        if (token.line != line) {
            line = token.line;
            debug.print("{:4} ", .{line});
        }
        else {
            debug.print("   | ", .{});
        }
        token.debug(debug);
        debug.print("\n", .{});

        if (token.type == .EOF) break;
    }
}

/// Scans a single token from source.
/// 
pub fn scan_token(self: *Scanner) Token {
    self.skip_whitespace();
    self.start = self.current;
    if (self.is_at_end()) return self.make_token(TokenType.EOF);

    const c = self.advance();

    if (is_digit(c)) return self.number();
    if (is_alpha(c)) return self.identifier();

    switch (c) {
        '(' => return self.make_token(TokenType.LEFT_PAREN),
        ')' => return self.make_token(TokenType.RIGHT_PAREN),
        '{' => return self.make_token(TokenType.LEFT_BRACE),
        '}' => return self.make_token(TokenType.RIGHT_BRACE),
        ';' => return self.make_token(TokenType.SEMICOLON),
        ',' => return self.make_token(TokenType.COMMA),
        '.' => return self.make_token(TokenType.DOT),
        '-' => return self.make_token(TokenType.MINUS),
        '+' => return self.make_token(TokenType.PLUS),
        '/' => return self.make_token(TokenType.SLASH),
        '*' => return self.make_token(TokenType.STAR),

        '!' => return if (self.match('=')) self.make_token(TokenType.BANG_EQUAL) else self.make_token(TokenType.BANG),
        '=' => return if (self.match('=')) self.make_token(TokenType.EQUAL_EQUAL) else self.make_token(TokenType.EQUAL),
        '<' => return if (self.match('=')) self.make_token(TokenType.LESS_EQUAL) else self.make_token(TokenType.LESS),
        '>' => return if (self.match('=')) self.make_token(TokenType.GREATER_EQUAL) else self.make_token(TokenType.GREATER),

        '"' => return self.string(),
        else => {},
    }

    //debug.print("unknown character: {c}\n", .{c});
    return self.error_token("Unexpected character.");
}

/// Parses a String token.
/// 
pub fn string(self: *Scanner) Token {
    while (self.peek() != '"' and !self.is_at_end()) {
        if (self.peek() == '\n') self.line += 1;
        _ = self.advance();
    }

    if (self.is_at_end()) return self.error_token("Unterminated string.");
    _ = self.advance();
    return self.make_token(TokenType.STRING);
}

/// Parses a Number token.
/// 
pub fn number(self: *Scanner) Token {
    while (is_digit(self.peek())) {
        _ = self.advance();
    }

    if (self.peek() == '.' and is_digit(self.peek_next())) {
        _ = self.advance();
        while (is_digit(self.peek())) {
            _ = self.advance();
        }
    }

    return self.make_token(TokenType.NUMBER);
}

/// Parses an Identifier token.
/// 
pub fn identifier(self: *Scanner) Token {
    while (is_alpha(self.peek()) or is_digit(self.peek())) {
        _ = self.advance();
    }

    return self.make_token(self.identifier_type());
}

/// Does token match?
/// 
pub fn match(self: *Scanner, comptime expected: u8) bool {
    if (self.is_at_end()) return false;
    if (self.peek() != expected) return false;

    self.current += 1;
    return true;
}

/// Makes a token.
/// 
/// token_type - the token type.
/// 
pub fn make_token(self: *Scanner, token_type: TokenType) Token {
    if (token_type != TokenType.EOF) {
        return Token {
            .type = token_type,
            .content = self.start,
            .len = @as(u16, @intCast(@intFromPtr(self.current) - @intFromPtr(self.start))),
            .line = self.line,
        };
    } 
    else {
        return Token{
            .type = token_type,
            .content = "",
            .len = 0,
            .line = self.line,
        };
    }
}

/// Makes an "error" token.
/// 
pub fn error_token(self: *Scanner, message: []const u8) Token {
    return Token{
        .type = TokenType.ERROR,
        .content = @as([*]const u8, @ptrCast(message)),
        .len = @as(u16, @intCast(message.len)),
        .line = self.line,
    };
}

/// Is end of source?
///
pub fn is_at_end(self: *Scanner) bool {
    return @intFromPtr(self.current) > @intFromPtr(self.end);
}

/// Advances to next token.
/// 
pub fn advance(self: *Scanner) u8 {
    const ret = self.current[0];
    self.current += 1;
    return ret;
}

/// Returns the current character.
/// 
pub fn peek(self: *Scanner) u8 {
    return self.current[0];
}

/// Returns the next character.
/// 
pub fn peek_next(self: *Scanner) u8 {
    if (self.is_at_end()) return 0;
    return self.current[1];
}

/// Skips all whitespace, and comments, incrementing the line counter if necessary.
/// 
pub fn skip_whitespace(self: *Scanner) void {
    while (true) {
        const c = self.peek();
        switch (c) {
            ' ', '\r', '\t' => {
                _ = self.advance();
            },
            '\n' => {
                self.line += 1;
                _ = self.advance();
            },
            '/' => {
                if (self.peek_next() == '/') {
                    // A comment goes until the end of the line.
                    while (self.peek() != '\n' and !self.is_at_end()) {
                        _ = self.advance();
                    }
                } 
                else {
                    return;
                }
            },
            else => return,
        }
    }
}

/// Is the character a digit?
/// 
pub fn is_digit(c: u8) bool {
    return '0' <= c and c <= '9';
}

/// Is the character alphabetic?  
pub fn is_alpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        c == '_';
}

/// Parses identifiers and keywords.
/// 
pub fn identifier_type(self: *Scanner) TokenType {
    switch (self.start[0]) {
        'a' => return self.check_keyword(1, "nd", TokenType.AND),
        'c' => return self.check_keyword(1, "lass", TokenType.CLASS),
        'e' => return self.check_keyword(1, "lse", TokenType.ELSE),
        'f' => {
            if (@intFromPtr(self.current) - @intFromPtr(self.start) > 1) {
                switch (self.start[1]) {
                    'a' => return self.check_keyword(2, "lse", TokenType.FALSE),
                    'o' => return self.check_keyword(2, "r", TokenType.FOR),
                    'u' => return self.check_keyword(2, "n", TokenType.FUN),
                    else => return TokenType.IDENTIFIER,
                }
            } 
            else {
                return TokenType.IDENTIFIER;
            }
        },
        'i' => return self.check_keyword(1, "f", TokenType.IF),
        'n' => return self.check_keyword(1, "il", TokenType.NIL),
        'o' => return self.check_keyword(1, "r", TokenType.OR),
        'p' => return self.check_keyword(1, "rint", TokenType.PRINT),
        'r' => return self.check_keyword(1, "eturn", TokenType.RETURN),
        's' => return self.check_keyword(1, "uper", TokenType.SUPER),
        't' => {
            if (@intFromPtr(self.current) - @intFromPtr(self.start) > 1) {
                switch (self.start[1]) {
                    'h' => return self.check_keyword(2, "is", TokenType.THIS),
                    'r' => return self.check_keyword(2, "ue", TokenType.TRUE),
                    else => return TokenType.IDENTIFIER,
                }
            } 
            else {
                return TokenType.IDENTIFIER;
            }
        },
        'v' => return self.check_keyword(1, "ar", TokenType.VAR),
        'w' => return self.check_keyword(1, "hile", TokenType.WHILE),
        else => return TokenType.IDENTIFIER,
    }
}

/// Checks the remainder of the keyword.
/// 
pub fn check_keyword(self: *Scanner, start: usize, rest: []const u8, token_type: TokenType) TokenType {
    const len = rest.len;
    const tgt = self.start[start .. start + len];
    const lhs = @intFromPtr(self.current) - @intFromPtr(self.start);
    const rhs = start + len;
    _ = lhs;
    _ = rhs;
    if (@intFromPtr(self.current) - @intFromPtr(self.start) == start + len and std.mem.eql(u8, tgt, rest)) {
        return token_type;
    }

    return TokenType.IDENTIFIER;
}


// TESTS!!
//
test "Scan Left Paren" {
   var scanner = Scanner.init("(");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.LEFT_PAREN);
   try assertEqual(token.len, 1);   
   try assertEquals("(", token.content[0..token.len]); 
   printPass("scan left paren");
}

test "Scan Right Paren" {
   var scanner = Scanner.init(")");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.RIGHT_PAREN);
   try assertEqual(token.len, 1);   
   try assertEquals(")", token.content[0..token.len]); 
   printPass("scan right paren");
}

test "Scan Left Brace" {
   var scanner = Scanner.init("{");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.LEFT_BRACE);
   try assertEqual(token.len, 1);   
   try assertEquals("{", token.content[0..token.len]); 
   printPass("scan left brace");
}

test "Scan Right Brace" {
   var scanner = Scanner.init("}");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.RIGHT_BRACE);
   try assertEqual(token.len, 1);   
   try assertEquals("}", token.content[0..token.len]); 
   printPass("scan right paren");
}

test "Scan Semicolon" {
   var scanner = Scanner.init(";");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.SEMICOLON);
   try assertEqual(token.len, 1);   
   try assertEquals(";", token.content[0..token.len]); 
   printPass("Scan semicolon");
}

test "Scan comma" {
   var scanner = Scanner.init(",");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.COMMA);
   try assertEqual(token.len, 1);   
   try assertEquals(",", token.content[0..token.len]); 
   printPass("Scan comma");
}

test "Scan Dot" {
   var scanner = Scanner.init(".");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.DOT);
   try assertEqual(token.len, 1);   
   try assertEquals(".", token.content[0..token.len]); 
   printPass("Scan dot");
}

test "Scan Minus" {
   var scanner = Scanner.init("-");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.MINUS);
   try assertEqual(token.len, 1);   
   try assertEquals("-", token.content[0..token.len]); 
   printPass("Scan minus");
}

test "Scan Plus" {
   var scanner = Scanner.init("+");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.PLUS);
   try assertEqual(token.len, 1);   
   try assertEquals("+", token.content[0..token.len]); 
   printPass("Scan Plus");
}

test "Scan Slash" {
   var scanner = Scanner.init("/");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.SLASH);
   try assertEqual(token.len, 1);   
   try assertEquals("/", token.content[0..token.len]); 
   printPass("Scan Slash");
}

test "Scan Star" {
   var scanner = Scanner.init("*");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.STAR);
   try assertEqual(token.len, 1);   
   try assertEquals("*", token.content[0..token.len]); 
   printPass("Scan Star");
}

test "Scan Bang Equal" {
   var scanner = Scanner.init("!=");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.BANG_EQUAL);
   try assertEqual(token.len, 2);   
   try assertEquals("!=", token.content[0..token.len]); 
   printPass("Scan Bang Equal");
}

test "Scan Bang" {
   var scanner = Scanner.init("!");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.BANG);
   try assertEqual(token.len, 1);   
   try assertEquals("!", token.content[0..token.len]); 
   printPass("Scan Bang");
}

test "Scan Equal" {
   var scanner = Scanner.init("=");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.EQUAL);
   try assertEqual(token.len, 1);   
   try assertEquals("=", token.content[0..token.len]); 
   printPass("Scan Equal");
}

test "Scan Equal Equal" {
   var scanner = Scanner.init("==");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.EQUAL_EQUAL);
   try assertEqual(token.len, 2);   
   try assertEquals("==", token.content[0..token.len]); 
   printPass("Scan Equal Equal");
}

test "Scan Less" {
   var scanner = Scanner.init("<");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.LESS);
   try assertEqual(token.len, 1);   
   try assertEquals("<", token.content[0..token.len]); 
   printPass("Scan Less");
}

test "Scan Less Equal" {
   var scanner = Scanner.init("<=");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.LESS_EQUAL);
   try assertEqual(token.len, 2);   
   try assertEquals("<=", token.content[0..token.len]); 
   printPass("Scan Less Equal");
}

test "Scan Greater" {
   var scanner = Scanner.init(">");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.GREATER);
   try assertEqual(token.len, 1);   
   try assertEquals(">", token.content[0..token.len]); 
   printPass("Scan Greater");
}

test "Scan Greater Equal" {
   var scanner = Scanner.init(">=");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.GREATER_EQUAL);
   try assertEqual(token.len, 2);   
   try assertEquals(">=", token.content[0..token.len]); 
   printPass("Scan Greater Equal");
}

test "Scan Identifier" {
   var scanner = Scanner.init("test");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.IDENTIFIER);
   try assertEqual(token.len, 4);   
   try assertEquals("test", token.content[0..token.len]); 
   printPass("Scan Identifier");
}

test "Scan String" {
   var scanner = Scanner.init("\"this is a string\"");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.STRING);
   try assertEqual(token.len, 18);   
   try assertEquals("\"this is a string\"", token.content[0..token.len]); 
   printPass("Scan String");
}

test "Unterminated String" {
   var scanner = Scanner.init("\"this is an unterminated string");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.ERROR);
   try assertEqual(token.len, 20);   
   try assertEquals("Unterminated string.", token.content[0..token.len]); 
   printPass("Unterminated string");
}

test "Scan Number" {
   var scanner = Scanner.init("1");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.NUMBER);
   try assertEqual(token.len, 1);   
   try assertEquals("1", token.content[0..token.len]); 
   printPass("Scan Number");
}

test "Scan Float" {
   var scanner = Scanner.init("3.14");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.NUMBER);
   try assertEqual(token.len, 4);   
   try assertEquals("3.14", token.content[0..token.len]); 
   printPass("Scan Float");
}

test "Unterminated Float" {
   var scanner = Scanner.init("3.");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.NUMBER);
   try assertEqual(token.len, 1);   
   try assertEquals("3", token.content[0..token.len]); 
   printPass("Unterminated Float");
}

test "Scan And" {
   var scanner = Scanner.init("and");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.AND);
   try assertEqual(token.len, 3);   
   try assertEquals("and", token.content[0..token.len]); 
   printPass("Scan And");
}

test "Scan Class" {
   var scanner = Scanner.init("class");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.CLASS);
   try assertEqual(token.len, 5);   
   try assertEquals("class", token.content[0..token.len]); 
   printPass("Scan Class");
}

test "Scan Else" {
   var scanner = Scanner.init("else");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.ELSE);
   try assertEqual(token.len, 4);   
   try assertEquals("else", token.content[0..token.len]); 
   printPass("Scan Else");
}

test "Scan False" {
   var scanner = Scanner.init("false");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.FALSE);
   try assertEqual(token.len, 5);   
   try assertEquals("false", token.content[0..token.len]); 
   printPass("Scan False");
}

test "Scan For" {
   var scanner = Scanner.init("for");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.FOR);
   try assertEqual(token.len, 3);   
   try assertEquals("for", token.content[0..token.len]); 
   printPass("Scan For");
}

test "Scan Fun" {
   var scanner = Scanner.init("fun");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.FUN);
   try assertEqual(token.len, 3);   
   try assertEquals("fun", token.content[0..token.len]); 
   printPass("Scan Fun");
}

test "Scan If" {
   var scanner = Scanner.init("if");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.IF);
   try assertEqual(token.len, 2);   
   try assertEquals("if", token.content[0..token.len]); 
   printPass("Scan If");
}


test "Scan Nil" {
   var scanner = Scanner.init("nil");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.NIL);
   try assertEqual(token.len, 3);   
   try assertEquals("nil", token.content[0..token.len]); 
   printPass("Scan Nil");
}

test "Scan Or" {
   var scanner = Scanner.init("or");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.OR);
   try assertEqual(token.len, 2);   
   try assertEquals("or", token.content[0..token.len]); 
   printPass("Scan Or");
}

test "Scan Print" {
   var scanner = Scanner.init("print");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.PRINT);
   try assertEqual(token.len, 5);   
   try assertEquals("print", token.content[0..token.len]); 
   printPass("Scan Print");
}

test "Scan Return" {
   var scanner = Scanner.init("return");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.RETURN);
   try assertEqual(token.len, 6);   
   try assertEquals("return", token.content[0..token.len]); 
   printPass("Scan Return");
}

test "Scan Super" {
   var scanner = Scanner.init("super");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.SUPER);
   try assertEqual(token.len, 5);   
   try assertEquals("super", token.content[0..token.len]); 
   printPass("Scan Super");
}

test "Scan This" {
   var scanner = Scanner.init("this");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.THIS);
   try assertEqual(token.len, 4);   
   try assertEquals("this", token.content[0..token.len]); 
   printPass("Scan This");
}

test "Scan True" {
   var scanner = Scanner.init("true");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.TRUE);
   try assertEqual(token.len, 4);   
   try assertEquals("true", token.content[0..token.len]); 
   printPass("Scan True");
}

test "Scan Var" {
   var scanner = Scanner.init("var");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.VAR);
   try assertEqual(token.len, 3);   
   try assertEquals("var", token.content[0..token.len]); 
   printPass("Scan Var");
}

test "Scan While" {
   var scanner = Scanner.init("while");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.WHILE);
   try assertEqual(token.len, 5);   
   try assertEquals("while", token.content[0..token.len]); 
   printPass("Scan While");
}

test "Unexpected character" {
   var scanner = Scanner.init("@");

   var token = scanner.scan_token();

   try assertEqual(token.type, TokenType.ERROR);
   try assertEqual(token.len, 21);   
   try assertEquals("Unexpected character.", token.content[0..token.len]); 
   printPass("Unexpected character");
}

test "Full Expression" {
   var scanner = Scanner.init("2 + 2 = 4");

   var token = scanner.scan_token();
   try assertEqual(token.type, TokenType.NUMBER);
   
   token = scanner.scan_token();
   try assertEqual(token.type, TokenType.PLUS);

   token = scanner.scan_token();
   try assertEqual(token.type, TokenType.NUMBER);

   token = scanner.scan_token();
   try assertEqual(token.type, TokenType.EQUAL);

   token = scanner.scan_token();
   try assertEqual(token.type, TokenType.NUMBER);

   token = scanner.scan_token();
   try assertEqual(token.type, TokenType.EOF);
}