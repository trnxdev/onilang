/// Inspired by Lua (https://www.lua.org/manual/5.4/manual.html#3.3)
const std = @import("std");
const utils = @import("utils.zig");

pub const Name = []const u8;
pub const Block = []*Statement;
pub const Explist = []*Expression;

pub const Statement = union(enum) {
    Declare: Declare,
    Assignment: Assignment,
    Goto: Goto,
    Label: Label,
    FunctionCall: FunctionCall,
    Return: Return,
    If: If,

    pub const Assignment = struct {
        Variables: []Expression.Prefix.Var,
        Values: []*Expression,
    };

    pub const If = struct {
        @"if": struct {
            Block: Block,
            Condition: *Expression,
        },
        @"else": ?struct {
            Block: Block,
        },
    };

    pub const Declare = struct {
        names: []const []const u8,
        attributes: []const []const Attribute,
        expressions: Explist,

        pub const Attribute = enum {
            constant,
            rational,
        };
    };
    pub const Goto = Name;
    pub const Label = Name;
    pub const Return = ?*Expression;
};

pub const Expression = union(enum) {
    Array: []*Expression,
    Binary: Binary,
    Unary: Unary,
    FunctionCall: FunctionCall,
    Prefix: Prefix,
    // Literals
    Number: f64,
    String: []const u8,
    Nil: void,
    Function: Function,
    Bool: bool,

    pub const Prefix = union(enum) {
        Var: Var,
        FunctionCall: FunctionCall,
        Group: *Expression,

        pub const Var = union(enum) {
            Identifier: []const u8,
            Access: Access,
            SimpleAccess: SimpleAccess,

            pub const Access = struct {
                Prefix: *Prefix,
                Field: *Expression,
            };

            pub const SimpleAccess = struct {
                Prefix: *Prefix,
                Field: []const u8,
            };
        };
    };

    pub const Function = struct {
        args: []const Arg,
        block: Block,

        const Arg = []const u8;
    };

    pub const Binary = struct {
        lhs: *Expression,
        rhs: *Expression,
        op: BinaryOperation,

        pub const BinaryOperation = enum {
            Add,
            Sub,
            Mul,
            Div,

            Or,
            And,

            Lt,
            Gt,
            Lte,
            Gte,
            Neql,
            Eql,

            Concat,
            Modulo,

            Exponentiation,
        };
    };

    pub const Unary = struct {
        rhs: *Expression,
        op: UnaryOperation,

        pub const UnaryOperation = enum {
            Len,
            UnaryMinus,
            Neg,
        };
    };

    pub const clone = utils.defineClone(Expression);
};

pub const FunctionCall = struct {
    callee: Callee,
    args: []Arg,

    pub const Callee = *Expression.Prefix;
    pub const Arg = *Expression;
};
