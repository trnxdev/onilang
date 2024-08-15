const std = @import("std");

pub const Token = struct {
    kind: Kind,
    lexeme: []const u8,
    location: Location,

    pub fn init(kind: Kind, lexeme: []const u8, loc: Location) @This() {
        return .{
            .kind = kind,
            .lexeme = lexeme,
            .location = loc,
        };
    }

    /// Keywords start from 120, see `is_keyword`
    pub const Kind = enum(u8) {
        EOF = 0,

        // Literals
        Identifier,
        Number, // Can be parsed by std.fmt.parseFloat(f64, ...), see numberLiteralToken
        String,

        Plus,
        Bang,
        BangEqual,
        PlusPlus, // concat
        Minus,
        Star,
        Slash,
        Percent,
        Caret,

        Equal,
        EqualEqual,

        Less,
        LessLess,
        LessEqual,
        Greater,
        GreaterGreater,
        GreaterEqual,

        Hash,
        Ampersand,
        Pipe,
        LeftParenthesis,
        RightParenthesis,
        LeftCurlyBrace,
        RightCurlyBrace,
        LeftBracket,
        RightBracket,
        Semicolon,
        Comma,
        Colon,
        ColonColon,
        Dot,

        @"fn" = 120,
        @"return",
        declare,
        @"or",
        @"and",
        not,
        goto,
        true,
        false,
        @"if",
        @"else",
        nil,

        fn is_keyword(self: @This()) bool {
            return @intFromEnum(self) >= 120;
        }
    };

    pub const Location = struct {
        start: usize = 0,
        offset: usize = 0,
        line: usize = 1,
        line_offset: usize = 0,
    };
};

allocator: std.mem.Allocator,
location: Token.Location,
source: []const u8,

pub fn init(allocator: std.mem.Allocator, source: []const u8) @This() {
    return .{
        .allocator = allocator,
        .location = .{},
        .source = source,
    };
}

pub fn deinit(_: @This()) void {}

pub fn scanArray(self: *@This()) ![]Token {
    var tokens = std.ArrayListUnmanaged(Token){};
    errdefer tokens.deinit(self.allocator);

    o: while (true) {
        const token = try self.scan();
        try tokens.append(self.allocator, token);

        if (tokens.getLast().kind == .EOF)
            break :o;
    }

    return try tokens.toOwnedSlice(self.allocator);
}

pub fn scan(self: *@This()) !Token {
    self.skipWhitespace();

    if (self.isEOF())
        return Token.init(.EOF, "<EOF>", self.location);

    self.location.start = self.location.offset;
    const c = self.advance();

    return switch (c) {
        '+' => if (self.match("+"))
            Token.init(.PlusPlus, "++", self.location)
        else
            Token.init(.Plus, "+", self.location),
        '!' => if (self.match("="))
            Token.init(.BangEqual, "!=", self.location)
        else
            Token.init(.Bang, "!", self.location),
        '-' => Token.init(.Minus, "-", self.location),
        '*' => Token.init(.Star, "*", self.location),
        '%' => Token.init(.Percent, "%", self.location),
        '^' => Token.init(.Caret, "^", self.location),
        '#' => Token.init(.Hash, "#", self.location),
        '&' => Token.init(.Ampersand, "&", self.location),
        '|' => Token.init(.Pipe, "|", self.location),
        '(' => Token.init(.LeftParenthesis, "(", self.location),
        ')' => Token.init(.RightParenthesis, ")", self.location),
        '{' => Token.init(.LeftCurlyBrace, "{", self.location),
        '}' => Token.init(.RightCurlyBrace, "}", self.location),
        '[' => Token.init(.LeftBracket, "[", self.location),
        ']' => Token.init(.RightBracket, "]", self.location),
        ';' => Token.init(.Semicolon, ";", self.location),
        ',' => Token.init(.Comma, ",", self.location),
        '/' => Token.init(.Slash, "/", self.location),
        // -- One or Two Character Tokens
        // =, ==
        '=' => if (self.match("="))
            Token.init(.EqualEqual, "==", self.location)
        else
            Token.init(.Equal, "=", self.location),
        // :, ::
        ':' => if (self.match(":"))
            Token.init(.ColonColon, "::", self.location)
        else
            Token.init(.Colon, ":", self.location),
        '.' => Token.init(.Dot, ".", self.location),
        // <, <<, <=
        '<' => if (self.match("="))
            Token.init(.LessEqual, "<=", self.location)
        else if (self.match("<"))
            Token.init(.LessLess, "<<", self.location)
        else
            Token.init(.Less, "<", self.location),
        // >, >>, >=
        '>' => if (self.match("="))
            Token.init(.GreaterEqual, ">=", self.location)
        else if (self.match(">"))
            Token.init(.GreaterGreater, ">>", self.location)
        else
            Token.init(.Greater, ">", self.location),
        '0'...'9' => try self.numberLiteralToken(),
        // Matches `isIdentifierLike`
        'a'...'z', 'A'...'Z', '_' => {
            o: while (true) {
                if (self.isEOF())
                    break :o;

                if (!isIdentifierLike(self.peek()))
                    break :o;

                _ = self.advance();
            }

            const identifier = self.source[self.location.start..self.location.offset];

            if (std.meta.stringToEnum(Token.Kind, identifier)) |maybe_keyword_kind| {
                // NOTE: Make sure it's an actual keyword so
                // the user cannot pass "Identifier"
                if (maybe_keyword_kind.is_keyword())
                    return Token.init(maybe_keyword_kind, identifier, self.location);
            }

            return Token.init(.Identifier, identifier, self.location);
        },
        '\'' => try self.stringLiteralToken(.SingleQuote),
        '"' => try self.stringLiteralToken(.DoubleQuote),
        else => {
            std.log.err("Unrecognized Character \"{c}\" at {}", .{ c, self.location });
            return error.HandledByOni;
        },
    };
}

fn skipWhitespace(self: *@This()) void {
    o: while (true) {
        switch (self.peek()) {
            ' ', // the standard ASCII whitespace characters space,
            std.ascii.control_code.ff, // form feed,
            '\n', // newline,
            std.ascii.control_code.cr, // carriage return,
            std.ascii.control_code.ht, // horizontal tab,
            std.ascii.control_code.vt, // vertical tab.
            => {
                if (self.peek() == '\n') {
                    self.location.line += 1;
                    self.location.line_offset = 0;
                }

                _ = self.advance();
            },
            '/' => {
                if (self.match("//")) {
                    while (self.peek() != '\n' and !self.isEOF()) {
                        _ = self.advance();
                    }

                    self.newLine();
                } else {
                    break :o;
                }
            },
            else => break :o,
        }
    }

    return;
}

// Literal helpers
const NumberLiteralTokenError = std.mem.Allocator.Error;
fn numberLiteralToken(self: *@This()) !Token {
    var num_literal = std.ArrayList(u8).init(self.allocator);
    defer num_literal.deinit();

    var is_hexadecimal: bool = false;
    var is_float: bool = false;
    var has_exponent: bool = false;

    self.location.offset -= 1;
    self.location.line_offset -= 1;

    if (self.match("0x") or self.match("0X")) {
        is_hexadecimal = true;

        // == Hexadecimal
        try num_literal.appendSlice("0x");
    }

    while (true) : (_ = self.advance()) {
        if (is_hexadecimal and isHexadecimalLike(self.peek()))
            try num_literal.append(self.peek())
        else if (!is_hexadecimal and std.ascii.isDigit(self.peek()))
            try num_literal.append(self.peek())
        else
            break;
    }

    if (self.match(".")) {
        is_float = true;
        try num_literal.append('.');

        while (true) : (_ = self.advance()) {
            if (is_hexadecimal and isHexadecimalLike(self.peek()))
                try num_literal.append(self.peek())
            else if (!is_hexadecimal and std.ascii.isDigit(self.peek()))
                try num_literal.append(self.peek())
            else
                break;
        }
    }

    // zig fmt: off
    if ((is_hexadecimal and self.match("P") or self.match("p"))
    or (!is_hexadecimal and self.match("E") or self.match("e"))) {
    // zig fmt: on
        has_exponent = true;
        try num_literal.append(std.ascii.toUpper(self.source[self.location.offset - 1]));

        if (self.match("-"))
            try num_literal.append('-')
        else if (self.match("+"))
            try num_literal.append('+');

        while (true) : (_ = self.advance()) {
            if (is_hexadecimal and isHexadecimalLike(self.peek()))
                try num_literal.append(self.peek())
            else if (!is_hexadecimal and std.ascii.isDigit(self.peek()))
                try num_literal.append(self.peek())
            else
                break;
        }
    }

    return Token.init(.Number, try num_literal.toOwnedSlice(), self.location);
}

const StringType = enum {
    SingleQuote,
    DoubleQuote,
};
fn stringLiteralToken(self: *@This(), string_type: StringType) !Token {
    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(self.allocator);

    o: while (true) {
        if (self.isEOF())
            return error.UnterminatedString;

        const char = self.advance();

        if (string_type == .SingleQuote and char == '\'')
            break :o;

        if (string_type == .DoubleQuote and char == '"')
            break :o;

        if (string_type == .SingleQuote or string_type == .DoubleQuote) {
            if (char == '\n')
                return error.UnterminatedString;

            if (char == '\\') {
                const escaping_char = self.advance();

                // 3.1 - Lexical Conventions (https://www.lua.org/manual/5.4/manual.html#3)
                switch (escaping_char) {
                    'a' => try output.append(self.allocator, std.ascii.control_code.bel), // Bell
                    'b' => try output.append(self.allocator, std.ascii.control_code.bs), // Backspace
                    'f' => try output.append(self.allocator, std.ascii.control_code.ff), // Form Feed
                    'n' => try output.append(self.allocator, '\n'), // Newline
                    'r' => try output.append(self.allocator, '\r'), // Carriage Return
                    't' => try output.append(self.allocator, '\t'), // Horizontal Tab
                    'v' => try output.append(self.allocator, std.ascii.control_code.vt), // Vertical Tab
                    '"' => try output.append(self.allocator, '"'),
                    '[' => try output.append(self.allocator, '['),
                    ']' => try output.append(self.allocator, ']'),
                    '\'' => try output.append(self.allocator, '\''),
                    '\\' => try output.append(self.allocator, '\\'),
                    'z' => {
                        _ = self.advance();
                    },
                    '\n' => {
                        try output.append(self.allocator, '\n');
                    },
                    'u' => {
                        if (!self.match("{"))
                            unreachable;

                        const start = self.location.offset;

                        while (self.peek() != '}') {
                            _ = self.advance();
                        }

                        const unicode_codepoint = self.source[start..self.location.offset];
                        _ = self.advance(); // Closing }

                        var utf8_output = try self.allocator.alloc(u8, 8);
                        errdefer self.allocator.free(utf8_output);

                        const bytes_written = try std.unicode.utf8Encode(
                            try std.fmt.parseInt(u21, unicode_codepoint, 16),
                            utf8_output,
                        );

                        // TODO: Do we need to resize?
                        if (bytes_written != 8) {
                            std.debug.assert(self.allocator.resize(utf8_output, bytes_written));
                            utf8_output = utf8_output[0..bytes_written];
                        }

                        try output.appendSlice(self.allocator, utf8_output);
                    },
                    'x' => {
                        const hexadecimal_0 = self.advance();
                        const hexadecimal_1 = self.advance();

                        if (!isHexadecimalLike(hexadecimal_0)) {
                            return error.NotHexadec;
                        }

                        if (!isHexadecimalLike(hexadecimal_1)) {
                            return error.NotHexadec;
                        }

                        const hexadecimal_char = try std.fmt.parseInt(
                            u8,
                            &[_]u8{ hexadecimal_0, hexadecimal_1 },
                            16,
                        );

                        try output.append(self.allocator, hexadecimal_char);
                    },
                    '0'...'9' => {
                        const start = self.location.offset - 1;

                        if (std.ascii.isDigit(self.peek()))
                            _ = self.advance();

                        if (std.ascii.isDigit(self.peek()))
                            _ = self.advance();

                        const ascii_char_codepoint = self.source[start..self.location.offset];

                        const ascii_char = try std.fmt.parseInt(
                            u8,
                            ascii_char_codepoint,
                            10,
                        );

                        try output.append(
                            self.allocator,
                            ascii_char,
                        );
                    },
                    else => {
                        std.debug.print("Invalid Escape Char {c}\n", .{escaping_char});
                        return error.InvalidEscapeChar;
                    },
                }

                continue :o;
            }
        }

        try output.append(self.allocator, char);
    }

    return Token.init(.String, try output.toOwnedSlice(self.allocator), self.location);
}

// Location helpers
fn advance(self: *@This()) u8 {
    if (self.isEOF())
        unreachable;

    defer self.location.offset += 1;
    defer self.location.line_offset += 1;
    return self.peek();
}

fn peek(self: *@This()) u8 {
    return if (self.isEOF()) 0 else self.source[self.location.offset];
}

fn peekNext(self: *@This()) u8 {
    return if (self.location.offset + 1 >= self.source.len) 0 else self.source[self.location.offset + 1];
}

fn match(self: *@This(), to_match: []const u8) bool {
    const adjust = to_match.len;
    const end = self.location.offset + adjust;

    if (end > self.source.len)
        return false;

    const matches = std.mem.eql(
        u8,
        self.source[self.location.offset..end],
        to_match,
    );

    if (matches) {
        self.location.offset += adjust;
        self.location.line_offset += adjust;
    }

    return matches;
}

fn newLine(self: *@This()) void {
    self.location.line += 1;
    self.location.line_offset = 0;
}

fn isEOF(self: @This()) bool {
    return self.location.offset >= self.source.len;
}

// Character helpers
fn isHexadecimalLike(c: u8) bool {
    return switch (c) {
        '0'...'9' => true,
        'a'...'f', 'A'...'F' => true,
        else => false,
    };
}

fn isIdentifierLike(c: u8) bool {
    return switch (c) {
        'a'...'z', 'A'...'Z' => true,
        '_' => true,
        else => false,
    };
}
