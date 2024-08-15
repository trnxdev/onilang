const std = @import("std");
const AST = @import("ast.zig");
const Token = @import("scanner.zig").Token;

allocator: std.mem.Allocator,
tokens: []const Token,
token_idx: usize,

pub fn init(allocator: std.mem.Allocator, tokens: []const Token) @This() {
    return .{
        .allocator = allocator,
        .tokens = tokens,
        .token_idx = 0,
    };
}

pub fn deinit(_: *@This()) void {}

pub inline fn parseRoot(self: *@This()) anyerror![]*AST.Statement {
    return try self.parseUntil(&.{.EOF});
}

pub fn parseUntil(self: *@This(), comptime tags: []const Token.Kind) anyerror![]*AST.Statement {
    var block = std.ArrayList(*AST.Statement).init(self.allocator);
    defer block.deinit();

    o: while (true) {
        inline for (tags) |tag| {
            if (self.peekToken().kind == tag)
                break :o;
        }

        const stmt = try self.parseStatement();
        try block.append(stmt);
    }

    return try block.toOwnedSlice();
}

// Statements
pub fn parseStatement(self: *@This()) anyerror!*AST.Statement {
    const cur = self.token_idx;

    const val = self.parseStatementInner() catch v: {
        self.token_idx = cur;
        const prefix_expr = try self.parsePrefixExpression();

        if (!(prefix_expr.* == .Prefix and prefix_expr.Prefix == .FunctionCall))
            return error.FrickYou;

        const function_call_stmt = try self.allocator.create(AST.Statement);

        function_call_stmt.* = .{
            .FunctionCall = prefix_expr.Prefix.FunctionCall,
        };

        break :v function_call_stmt;
    };

    if (switch (val.*) {
        .Label, .If => false,
        else => true,
    })
        _ = try self.advanceIfCurrentKindEql(.Semicolon);

    return val;
}

fn parseStatementInner(self: *@This()) anyerror!*AST.Statement {
    const val = switch (self.peekToken().kind) {
        .ColonColon => try self.parseLabelStatement(),
        .goto => try self.parseGotoStatement(),
        .declare => try self.parseDeclareStatement(),
        .@"if" => try self.parseIfStatement(),
        .Identifier => try self.parseAssignmentStatement(),
        .@"return" => {
            _ = try self.advanceIfCurrentKindEql(.@"return");
            const return_stmt = try self.allocator.create(AST.Statement);
            const backed = self.token_idx;

            return_stmt.* = .{
                .Return = self.parseExpression() catch v: {
                    self.token_idx = backed;
                    break :v null;
                },
            };

            return return_stmt;
        },
        else => {
            return error.WrongStatement;
        },
    };

    return val;
}

pub fn parseAssignmentStatement(self: *@This()) anyerror!*AST.Statement {
    var varlist = std.ArrayList(AST.Expression.Prefix.Var).init(self.allocator);
    defer varlist.deinit();

    o: while (true) {
        switch (self.peekToken().kind) {
            .Identifier => {
                const @"var" = try self.parsePrefixExpression();
                if (!(@"var".* == .Prefix and @"var".Prefix == .Var)) {
                    return error.NotPrefixVar;
                }

                try varlist.append(@"var".Prefix.Var);

                if (self.peekToken().kind == .Comma) {
                    _ = try self.advanceIfCurrentKindEql(.Comma);
                    continue :o;
                }

                break :o;
            },
            else => unreachable,
        }
    }

    _ = try self.advanceIfCurrentKindEql(.Equal);

    const expressions: []*AST.Expression = try self.parseExplist();
    const assign = try self.allocator.create(AST.Statement);
    const vars = try varlist.toOwnedSlice();

    if (vars.len != expressions.len)
        return error.MustBeSameLen;

    assign.* = .{
        .Assignment = .{
            .Values = expressions,
            .Variables = vars,
        },
    };

    return assign;
}

pub fn parseIfStatement(self: *@This()) anyerror!*AST.Statement {
    _ = try self.advanceIfCurrentKindEql(.@"if");
    _ = try self.advanceIfCurrentKindEql(.LeftParenthesis);

    const if_stmt = try self.allocator.create(AST.Statement);
    const if_expr = try self.parseExpression();

    _ = try self.advanceIfCurrentKindEql(.RightParenthesis);
    _ = try self.advanceIfCurrentKindEql(.LeftCurlyBrace);

    const if_block = try self.parseUntil(&.{.RightCurlyBrace});

    if_stmt.* = .{
        .If = .{
            .@"if" = .{
                .Block = if_block,
                .Condition = if_expr,
            },
            .@"else" = null,
        },
    };

    _ = try self.advanceIfCurrentKindEql(.RightCurlyBrace);

    if (self.peekToken().kind == .@"else") {
        _ = try self.advanceIfCurrentKindEql(.@"else");
        _ = try self.advanceIfCurrentKindEql(.LeftCurlyBrace);

        const else_block = try self.parseUntil(&.{.RightCurlyBrace});

        if_stmt.If.@"else" = .{
            .Block = else_block,
        };
        _ = try self.advanceIfCurrentKindEql(.RightCurlyBrace);
    }

    return if_stmt;
}

pub fn parseGotoStatement(self: *@This()) anyerror!*AST.Statement {
    _ = try self.advanceIfCurrentKindEql(.goto);
    const label = try self.advanceIfCurrentKindEql(.Identifier);

    const goto_stmt = try self.allocator.create(AST.Statement);

    goto_stmt.* = .{
        .Goto = label.lexeme,
    };

    return goto_stmt;
}

pub fn parseLabelStatement(self: *@This()) anyerror!*AST.Statement {
    _ = try self.advanceIfCurrentKindEql(.ColonColon);
    const name = try self.advanceIfCurrentKindEql(.Identifier);
    _ = try self.advanceIfCurrentKindEql(.ColonColon);

    const label_stmt = try self.allocator.create(AST.Statement);

    label_stmt.* = .{ .Label = name.lexeme };

    return label_stmt;
}

pub fn namelist(self: *@This()) anyerror![]const []const u8 {
    var names = std.ArrayList([]const u8).init(self.allocator);
    defer names.deinit();

    var must_be_next: bool = false;
    o: while (true) {
        const exp = self.advanceIfCurrentKindEql(.Identifier) catch {
            if (must_be_next)
                unreachable;

            return try names.toOwnedSlice();
        };
        try names.append(exp.lexeme);

        if (self.peekToken().kind == .Comma) {
            must_be_next = true;
            _ = try self.advanceIfCurrentKindEql(.Comma);
            continue :o;
        } else {
            must_be_next = false;
        }

        break :o;
    }

    return try names.toOwnedSlice();
}

pub fn parseDeclareStatement(self: *@This()) anyerror!*AST.Statement {
    _ = try self.advanceIfCurrentKindEql(.declare);

    var names = std.ArrayList([]const u8).init(self.allocator);
    defer names.deinit();

    var attributes = std.ArrayList([]const AST.Statement.Declare.Attribute).init(self.allocator);
    defer attributes.deinit();

    var must_be_next: bool = false;
    o: while (true) {
        const name = try self.advanceIfCurrentKindEql(.Identifier);
        try names.append(name.lexeme);

        var cur_attributes = std.ArrayList(AST.Statement.Declare.Attribute).init(self.allocator);
        defer cur_attributes.deinit();

        if (self.peekToken().kind == .Less) {
            _ = try self.advanceIfCurrentKindEql(.Less);

            var must_be_next_attribute: bool = false;

            i: while (self.peekToken().kind != .Greater) {
                const identifier = (try self.advanceIfCurrentKindEql(.Identifier)).lexeme;
                const attribute = std.meta.stringToEnum(
                    AST.Statement.Declare.Attribute,
                    identifier,
                ) orelse return error.InvalidAttribute;

                try cur_attributes.append(attribute);

                if (self.peekToken().kind == .Comma) {
                    must_be_next_attribute = true;
                    _ = try self.advanceIfCurrentKindEql(.Comma);
                    continue :i;
                } else {
                    must_be_next_attribute = false;
                }

                break :i;
            }

            _ = try self.advanceIfCurrentKindEql(.Greater);
        }

        try attributes.append(try cur_attributes.toOwnedSlice());

        if (self.peekToken().kind == .Comma) {
            must_be_next = true;
            _ = try self.advanceIfCurrentKindEql(.Comma);
            continue :o;
        } else {
            must_be_next = false;
        }

        break :o;
    }

    _ = try self.advanceIfCurrentKindEql(.Equal);

    const expressions_arr = try self.parseExplist();
    const names_arr = try names.toOwnedSlice();
    const attributes_arr = try attributes.toOwnedSlice();

    std.debug.assert(names_arr.len == attributes_arr.len);

    if (expressions_arr.len != names_arr.len) {
        return error.DeclareExpressionMustBeSameLenAsNames;
    }

    const declare_m = try self.allocator.create(AST.Statement);

    declare_m.* = .{
        .Declare = .{
            .attributes = attributes_arr,
            .names = names_arr,
            .expressions = expressions_arr,
        },
    };

    return declare_m;
}

pub fn parseExplist(self: *@This()) anyerror![]*AST.Expression {
    var expressions = std.ArrayList(*AST.Expression).init(self.allocator);
    defer expressions.deinit();

    var must_be_next: bool = false;

    o: while (true) {
        const exp = self.parseExpression() catch {
            if (must_be_next)
                unreachable;

            // FIXME: check that the error is for wrong expression
            return try expressions.toOwnedSlice();
        };
        try expressions.append(exp);

        if (self.peekToken().kind == .Comma) {
            must_be_next = true;
            _ = try self.advanceIfCurrentKindEql(.Comma);
            continue :o;
        } else {
            must_be_next = false;
        }

        break :o;
    }

    return try expressions.toOwnedSlice();
}

pub fn parseMultiArg(self: *@This()) ![]AST.FunctionCall.Arg {
    var arglist = std.ArrayList(AST.FunctionCall.Arg).init(self.allocator);
    defer arglist.deinit();

    var must_be_next: bool = false;
    o: while (self.peekToken().kind != .RightParenthesis) {
        try arglist.append(
            self.parseExpression() catch {
                if (must_be_next)
                    unreachable;

                break :o;
            },
        );

        if (self.peekToken().kind == .Comma) {
            must_be_next = true;
            _ = try self.advanceIfCurrentKindEql(.Comma);
        }
    }

    return try arglist.toOwnedSlice();
}
// Precedence
//     or
//     and
//     <     >     <=    >=    ==
//     ++ (string concat)
//     +     -
//     *     /    %
//     unary operators (!  -)
//     ^
pub fn parseOr(self: *@This()) !*AST.Expression {
    const next = parseAnd;
    const lhs = try next(self);

    while (self.peekToken().kind == .@"or") {
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = .Or,
        } };
    }

    return lhs;
}

pub fn parseAnd(self: *@This()) !*AST.Expression {
    const next = parseComparison;
    const lhs = try next(self);

    while (self.peekToken().kind == .@"and") {
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = .And,
        } };
    }

    return lhs;
}

pub fn parseComparison(self: *@This()) !*AST.Expression {
    const next = parseConcat;
    const lhs = try next(self);

    while (switch (self.peekToken().kind) {
        .Less, .Greater, .LessEqual, .GreaterEqual, .BangEqual, .EqualEqual => true,
        else => false,
    }) {
        const op = self.peekToken().kind;
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = switch (op) {
                .Less => .Lt,
                .Greater => .Gt,
                .LessEqual => .Lte,
                .GreaterEqual => .Gte,
                .BangEqual => .Neql,
                .EqualEqual => .Eql,
                else => unreachable,
            },
        } };
    }

    return lhs;
}

pub fn parseConcat(self: *@This()) !*AST.Expression {
    const next = parseTerm;
    const lhs = try next(self);

    while (self.peekToken().kind == .PlusPlus) {
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = .Concat,
        } };
    }

    return lhs;
}

pub fn parseTerm(self: *@This()) !*AST.Expression {
    const next = parseMul;
    const lhs = try next(self);

    while (self.peekToken().kind == .Plus or self.peekToken().kind == .Minus) {
        const op = self.peekToken().kind;
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = if (op == .Plus) .Add else .Sub,
        } };
    }

    return lhs;
}

pub fn parseMul(self: *@This()) !*AST.Expression {
    const next = parseUnary;
    const lhs = try next(self);

    while (switch (self.peekToken().kind) {
        .Star, .Slash, .Percent => true,
        else => false,
    }) {
        const op = self.peekToken().kind;
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = switch (op) {
                .Star => .Mul,
                .Slash => .Div,
                .Percent => .Modulo,
                else => unreachable,
            },
        } };
    }

    return lhs;
}

pub fn parseUnary(self: *@This()) !*AST.Expression {
    const next = parseExponentiation;
    var changed: bool = false;
    const changed_expr: *AST.Expression = try self.allocator.create(AST.Expression);

    while (switch (self.peekToken().kind) {
        .Hash, .Minus, .not => true,
        else => false,
    }) {
        const op = self.peekToken().kind;
        _ = self.advance();
        const rhs = try next(self);

        changed = true;
        changed_expr.* = .{ .Unary = .{
            .op = switch (op) {
                .Hash => .Len,
                .Minus => .UnaryMinus,
                .Bang => .Neg,
                else => unreachable,
            },
            .rhs = try rhs.clone(self.allocator),
        } };
    }

    return if (changed)
        changed_expr
    else
        next(self);
}

pub fn parseExponentiation(self: *@This()) !*AST.Expression {
    const next = parsePrimary;
    const lhs = try next(self);

    while (self.peekToken().kind == .Caret) {
        _ = self.advance();
        const rhs = try next(self);

        lhs.* = .{ .Binary = .{
            .lhs = try lhs.clone(self.allocator),
            .rhs = rhs,
            .op = .Exponentiation,
        } };
    }

    return lhs;
}

// Expressions
pub fn parseExpression(self: *@This()) anyerror!*AST.Expression {
    return try self.parseOr();
}

pub fn parsePrimary(self: *@This()) anyerror!*AST.Expression {
    const exp = try self.allocator.create(AST.Expression);

    switch (self.peekToken().kind) {
        .String => {
            const str = try self.advanceIfCurrentKindEql(.String);

            exp.* = .{ .String = str.lexeme };
        },
        .Number => {
            const num_tok = try self.advanceIfCurrentKindEql(.Number);
            const num_val = try std.fmt.parseFloat(f64, num_tok.lexeme);

            exp.* = .{ .Number = num_val };
        },
        .true => {
            _ = try self.advanceIfCurrentKindEql(.true);
            exp.* = .{ .Bool = true };
        },
        .false => {
            _ = try self.advanceIfCurrentKindEql(.false);
            exp.* = .{ .Bool = false };
        },
        .nil => {
            _ = try self.advanceIfCurrentKindEql(.nil);
            exp.* = .Nil;
        },
        .LeftBracket => {
            _ = try self.advanceIfCurrentKindEql(.LeftBracket);
            const expressions = try self.parseExplist();
            _ = try self.advanceIfCurrentKindEql(.RightBracket);
            exp.* = .{ .Array = expressions };
        },
        .@"fn" => exp.* = (try self.parseFunctionLiteral()).*,
        else => exp.* = (try self.parsePrefixExpression()).*,
    }

    return exp;
}

// https://github.com/GavinHigham/lpil53/blob/90815131726cc4a917fc777ba1268b507456a774/parser.lua#L350
pub fn parsePrefixExpression(self: *@This()) anyerror!*AST.Expression {
    const prefixexp: *AST.Expression = try self.allocator.create(AST.Expression);

    if (self.peekToken().kind == .LeftParenthesis) {
        _ = try self.advanceIfCurrentKindEql(.LeftParenthesis);
        prefixexp.* = .{ .Prefix = .{
            .Group = try self.parseExpression(),
        } };
        _ = try self.advanceIfCurrentKindEql(.RightParenthesis);
    } else if (self.peekToken().kind == .Identifier) {
        const name = try self.advanceIfCurrentKindEql(.Identifier);
        prefixexp.* = .{ .Prefix = .{ .Var = .{ .Identifier = name.lexeme } } };
    } else {
        return error.Idk;
    }

    o: while (true) {
        const token = self.peekToken();

        switch (token.kind) {
            .LeftBracket => {
                _ = try self.advanceIfCurrentKindEql(.LeftBracket);
                const exp = try self.parseExpression();
                _ = try self.advanceIfCurrentKindEql(.RightBracket);

                const pref = try self.allocator.create(AST.Expression.Prefix);
                pref.* = (try prefixexp.clone(self.allocator)).Prefix;
                prefixexp.* = .{ .Prefix = .{ .Var = .{ .Access = .{
                    .Prefix = pref,
                    .Field = exp,
                } } } };
            },
            .Dot => {
                _ = try self.advanceIfCurrentKindEql(.Dot);
                const field = try self.advanceIfCurrentKindEql(.Identifier);

                const pref = try self.allocator.create(AST.Expression.Prefix);
                pref.* = (try prefixexp.clone(self.allocator)).Prefix;
                prefixexp.* = .{ .Prefix = .{ .Var = .{ .SimpleAccess = .{
                    .Prefix = pref,
                    .Field = field.lexeme,
                } } } };
            },
            .LeftParenthesis => {
                const pref = try self.allocator.create(AST.Expression.Prefix);
                pref.* = (try prefixexp.clone(self.allocator)).Prefix;

                prefixexp.* = .{ .Prefix = .{
                    .FunctionCall = .{
                        .args = try self.parseFuncArgs(),
                        .callee = pref,
                    },
                } };
            },
            else => break :o,
        }
    }

    return prefixexp;
}

pub fn parseFuncArgs(self: *@This()) anyerror![]AST.FunctionCall.Arg {
    _ = try self.advanceIfCurrentKindEql(.LeftParenthesis);
    const args = try self.parseMultiArg();
    _ = try self.advanceIfCurrentKindEql(.RightParenthesis);
    return args;
}

// only var, var.var, var[var]
pub fn parseVarPrefixExprIndependant(self: *@This()) anyerror!*AST.Expression {
    const name = try self.advanceIfCurrentKindEql(.Identifier);
    const var_prefix = try self.allocator.create(AST.Expression);

    var_prefix.* = .{
        .Prefix = .{
            .Var = .{
                .Identifier = name.lexeme,
            },
        },
    };

    o: while (true) {
        switch (self.peekToken().kind) {
            .Dot => {
                _ = try self.advanceIfCurrentKindEql(.Dot);
                const field = try self.advanceIfCurrentKindEql(.Identifier);
                const pref = try self.allocator.create(AST.Expression.Prefix);
                pref.* = (try var_prefix.clone(self.allocator)).Prefix;

                var_prefix.* = .{ .Prefix = .{
                    .Var = .{ .DotAccess = .{
                        .Field = field.lexeme,
                        .Prefix = pref,
                    } },
                } };
            },
            .LeftBracket => {
                _ = try self.advanceIfCurrentKindEql(.LeftBracket);
                const exp = try self.parseExpression();
                _ = try self.advanceIfCurrentKindEql(.RightBracket);

                const pref = try self.allocator.create(AST.Expression.Prefix);
                pref.* = (try var_prefix.clone(self.allocator)).Prefix;

                var_prefix.* = .{ .Prefix = .{
                    .Var = .{ .Access = .{
                        .Field = exp,
                        .Prefix = pref,
                    } },
                } };
            },
            else => break :o,
        }
    }

    return var_prefix;
}

pub fn parseFunctionLiteral(self: *@This()) anyerror!*AST.Expression {
    _ = try self.advanceIfCurrentKindEql(.@"fn");
    _ = try self.advanceIfCurrentKindEql(.LeftParenthesis);

    const arglist = try self.namelist();

    _ = try self.advanceIfCurrentKindEql(.RightParenthesis);
    _ = try self.advanceIfCurrentKindEql(.LeftCurlyBrace);

    const block = try self.parseUntil(&.{.RightCurlyBrace});

    _ = try self.advanceIfCurrentKindEql(.RightCurlyBrace);
    const fa = try self.allocator.create(AST.Expression);

    fa.* = .{ .Function = .{
        .args = arglist,
        .block = block,
    } };

    return fa;
}

// Helpers
inline fn peekTokenIfKindEql(self: @This(), kind: Token.Kind) !Token {
    const token = self.peekToken();

    if (token.kind != kind)
        return error.ExpectedAnotherKind;

    return token;
}

inline fn advanceIfCurrentKindEql(self: *@This(), kind: Token.Kind) !Token {
    _ = try self.peekTokenIfKindEql(kind);
    return self.advance();
}

inline fn advance(self: *@This()) Token {
    defer self.token_idx += 1;
    return self.peekToken();
}

inline fn peekToken(self: @This()) Token {
    if (self.isEOF())
        unreachable;

    return self.tokens[self.token_idx];
}

inline fn isEOF(self: @This()) bool {
    return self.token_idx >= self.tokens.len;
}
