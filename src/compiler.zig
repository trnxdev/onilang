const std = @import("std");
const AST = @import("ast.zig");
const Instruction = @import("vm/instruction.zig");
const Value = @import("vm/value.zig");
const Object = @import("vm/object.zig");
const VM = @import("vm/vm.zig");

const BuiltinRational = @import("./vm/builtin/rational.zig");
const BuiltinConsole = @import("./vm/builtin/console.zig");

// null
allocator: std.mem.Allocator,
scope: *Scope,
globals: Variables,
globals_hash: Hash,

const Hash = struct {
    back: Back,

    const Back = std.StringHashMap(usize);

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .back = Back.init(allocator),
        };
    }

    pub fn hash(self: *@This(), input: []const u8) !usize {
        const gop = try self.back.getOrPut(input);

        if (!gop.found_existing)
            gop.value_ptr.* = self.back.count() - 1;

        std.debug.assert(gop.value_ptr.* <= Max_Variables);

        return gop.value_ptr.*;
    }

    pub fn only_set_hash(self: *@This(), input: []const u8) !usize {
        const gop = try self.back.getOrPut(input);

        if (gop.found_existing)
            return error.HashAlreadyExists;

        gop.value_ptr.* = self.back.count() - 1;

        std.debug.assert(gop.value_ptr.* <= Max_Variables);
        return gop.value_ptr.*;
    }

    pub fn only_get_hash(self: *@This(), input: []const u8) !usize {
        const gop = try self.back.getOrPut(input);

        if (!gop.found_existing)
            return error.HashNotFound;

        return gop.value_ptr.*;
    }
};

const Max_Variables = 6000;
const Variables = []Variable;

const Variable = struct {
    attributes: []const AST.Statement.Declare.Attribute,
};

const Chunk = std.ArrayList(Instruction);

const Scope = struct {
    index: usize,
    locals: Variables,
    locals_hash: Hash,
    chunk: Chunk,
    constants: Constants,
    previous: *Scope,
    labels: Labels,

    const Labels = std.StringHashMap(usize);
    const Constants = std.ArrayList(Value);

    pub fn init(self: *@This(), allocator: std.mem.Allocator, index: usize) !void {
        self.* = .{
            .index = index,
            .chunk = Chunk.init(allocator),
            .locals = try allocator.alloc(Variable, Max_Variables),
            .locals_hash = Hash.init(allocator),
            .constants = Constants.init(allocator),
            .labels = Labels.init(allocator),
            .previous = undefined,
        };
    }

    pub fn is_root(self: @This()) bool {
        return self.index == 0;
    }
};

pub fn init(allocator: std.mem.Allocator) !@This() {
    const root_scope = try allocator.create(Scope);
    try root_scope.init(allocator, 0);

    // TODO: No need for locals, since it's the root scope

    var cmp: @This() = .{
        .allocator = allocator,
        .scope = root_scope,
        .globals = try allocator.alloc(Variable, Max_Variables),
        .globals_hash = Hash.init(allocator),
    };

    const builtin_names: []const []const u8 = &.{
        BuiltinConsole.SelfTag,
        BuiltinRational.SelfTag,
    };

    const builtins: []const *Object.Table = &.{
        try BuiltinConsole.create(allocator),
        try BuiltinRational.create(allocator),
    };

    for (builtins, builtin_names) |builtin, builtin_name| {
        const global_idx = try cmp.globals_hash.only_set_hash(builtin_name);

        try cmp.scope.chunk.append(.{
            .kind = .LoadConst,
            .A = cmp.scope.constants.items.len,
        });
        try cmp.scope.constants.append(builtin.object.asValue());
        try cmp.scope.chunk.append(.{
            .kind = .SetGlobal,
            .A = global_idx,
        });
    }

    return cmp;
}

pub fn deinit(_: *@This()) void {}

pub fn compileRoot(self: *@This(), root: AST.Block) !*Object.Function {
    try self.compileBlock(root);
    std.debug.assert(self.scope.is_root());

    try self.scope.chunk.append(.{
        .kind = .Exit,
    });

    return try Object.Function.create(
        self.allocator,
        null,
        0,
        try self.scope.chunk.toOwnedSlice(),
        try self.scope.constants.toOwnedSlice(),
        true,
    );
}

pub fn compileBlock(self: *@This(), block: AST.Block) anyerror!void {
    for (block) |stmt| {
        try self.compileStmt(stmt.*);
    }
}

pub fn compileStmt(self: *@This(), stmt: AST.Statement) anyerror!void {
    switch (stmt) {
        .Assignment => |a| {
            for (a.Variables, a.Values) |prefix, val| {
                if (prefix != .Identifier)
                    @panic("TODO");

                const prefix_ident = prefix.Identifier;

                const name = self.globals_hash.back.get(prefix_ident) orelse unreachable;

                try self.compileExpr(val.*, .{});
                try self.scope.chunk.append(.{
                    .kind = .SetGlobal,
                    .A = name,
                });
            }
        },
        .FunctionCall => |fc| {
            try self.compileFunccall(fc, true);
        },
        .Return => |r| {
            if (r) |expr|
                try self.compileExpr(expr.*, .{});

            try self.scope.chunk.append(.{
                .kind = .Return,
                .A = @intFromBool(r != null),
            });
        },
        .Declare => |d| {
            for (d.names, d.attributes, d.expressions) |name, attrs, assign_expr| {
                const name_hash = if (self.scope.is_root())
                    try self.globals_hash.only_set_hash(name)
                else v: {
                    if (self.globals_hash.back.get(name)) |_|
                        return error.VariableAlreadyDeclaredGlobally;

                    break :v try self.scope.locals_hash.only_set_hash(name);
                };

                if (self.scope.is_root())
                    self.globals[name_hash] = .{
                        .attributes = attrs,
                    }
                else
                    self.scope.locals[name_hash] = .{
                        .attributes = attrs,
                    };

                try self.compileExpr(assign_expr.*, .{
                    .var_decl_name = name,
                    .rational = v: {
                        for (attrs) |attr| {
                            if (attr == .rational)
                                break :v true;
                        }
                        break :v false;
                    },
                });

                try self.scope.chunk.append(Instruction{
                    .kind = if (self.scope.is_root()) .SetGlobal else .SetLocal,
                    .A = name_hash,
                });
            }
        },
        .If => |if_stmt| {
            if (if_stmt.@"else") |_|
                unreachable;

            try self.compileExpr(if_stmt.@"if".Condition.*, .{});

            const patch_end: usize = self.scope.chunk.items.len;

            try self.scope.chunk.append(.{
                .kind = .Jump_False,
                .A = 0,
            });

            try self.compileBlock(if_stmt.@"if".Block);

            self.scope.chunk.items[patch_end].A = self.scope.chunk.items.len;
        },
        .Label => |l| {
            try self.scope.labels.putNoClobber(l, self.scope.chunk.items.len);
        },
        .Goto => |l| {
            const label = self.scope.labels.get(l) orelse return error.LabelNotFound;

            try self.scope.chunk.append(Instruction{
                .kind = .Jump,
                .A = label,
            });
        },
    }
}

const Hint = struct {
    var_decl_name: ?[]const u8 = null,
    rational: bool = false,
};

pub fn compileExpr(self: *@This(), expr: AST.Expression, hint: Hint) anyerror!void {
    switch (expr) {
        .Binary => |b| {
            try self.compileExpr(b.lhs.*, .{});
            try self.compileExpr(b.rhs.*, .{});

            try self.scope.chunk.append(.{
                .kind = switch (b.op) {
                    .Add => .Add,
                    .Sub => .Sub,
                    .Mul => .Mul,
                    .Div => .Div,
                    .Lte => .Lte,
                    .Lt => .Lt,
                    .Gte => .Gte,
                    .Gt => .Gt,
                    .Neql => .Neql,
                    else => @panic("TODO"),
                },
                .A = 0,
            });
        },
        .Number => |num| {
            try self.scope.chunk.append(Instruction{
                .kind = .LoadConst,
                .A = self.scope.constants.items.len,
            });

            if (hint.rational) {
                const rational = try Object.Rational.create(self.allocator);
                try rational.setFromFloat(f64, num);
                try self.scope.constants.append(rational.object.asValue());
            } else {
                try self.scope.constants.append(Value.initNumber(num));
            }
        },
        .Bool => |bl| {
            try self.scope.chunk.append(Instruction{
                .kind = .LoadConst,
                .A = self.scope.constants.items.len,
            });

            try self.scope.constants.append(Value.initBool(bl));
        },
        .Nil => {
            try self.scope.chunk.append(Instruction{
                .kind = .LoadConst,
                .A = self.scope.constants.items.len,
            });

            try self.scope.constants.append(Value.initNil());
        },
        .Array => |a| {
            for (a) |aexpr| {
                try self.compileExpr(aexpr.*, .{});
            }

            try self.scope.chunk.append(Instruction{
                .kind = .MakeArray,
                .A = a.len,
            });
        },
        .String => |s| {
            try self.scope.chunk.append(.{
                .kind = .LoadConst,
                .A = self.scope.constants.items.len,
            });
            try self.scope.constants.append(
                (try Object.String.create(self.allocator, s)).object.asValue(),
            );
        },
        .Function => |f| {
            if (!self.scope.is_root())
                return error.NoFunctionInFunctionYet;

            const new_scope = try self.allocator.create(Scope);
            const old_scope = self.scope;

            try new_scope.init(self.allocator, old_scope.index + 1);
            new_scope.previous = old_scope;

            self.scope = new_scope;

            // Reserve const 0 for itself
            const itself_index = self.scope.constants.items.len;
            try self.scope.constants.append(Value.initNil());

            for (f.args) |arg| {
                _ = try self.scope.locals_hash.only_set_hash(arg);
            }

            try self.compileBlock(f.block);

            try self.scope.chunk.append(Instruction{
                .kind = .Return,
                .A = @intFromBool(false),
            });

            const function = try Object.Function.create(
                self.allocator,
                hint.var_decl_name,
                f.args.len,
                try self.scope.chunk.toOwnedSlice(),
                try self.scope.constants.toOwnedSlice(),
                false,
            );

            function.constants[itself_index] = function.object.asValue();
            self.scope = old_scope;

            try self.scope.chunk.append(Instruction{
                .kind = .LoadConst,
                .A = self.scope.constants.items.len,
            });

            try self.scope.constants.append(function.object.asValue());
        },
        .Prefix => |p| {
            switch (p) {
                .FunctionCall => |fc| {
                    try self.compileFunccall(fc, false);
                },
                .Var => |v| {
                    switch (v) {
                        .Identifier => |i| o: {
                            if (self.scope.locals_hash.back.get(i)) |local| {
                                try self.scope.chunk.append(.{
                                    .kind = .GetLocal,
                                    .A = local,
                                });
                                break :o;
                            }
                            if (self.globals_hash.back.get(i)) |global| {
                                try self.scope.chunk.append(.{
                                    .kind = .GetGlobal,
                                    .A = global,
                                });
                                break :o;
                            }
                            std.debug.print("VARNOTFOUND {s}\n", .{i});
                            return error.VariableNotFound;
                        },
                        .Access => |a| {
                            try self.compileExpr(.{ .Prefix = a.Prefix.* }, .{});
                            try self.compileExpr(a.Field.*, .{});

                            try self.scope.chunk.append(Instruction{
                                .kind = .Access,
                            });
                        },
                        .SimpleAccess => |a| {
                            try self.compileExpr(.{ .Prefix = a.Prefix.* }, .{});
                            try self.scope.chunk.append(.{
                                .kind = .LoadConst,
                                .A = self.scope.constants.items.len,
                            });
                            try self.scope.constants.append((try Object.String.create(self.allocator, a.Field)).object.asValue());

                            try self.scope.chunk.append(Instruction{
                                .kind = .Access,
                            });
                        },
                    }
                },
                .Group => |g| {
                    try self.compileExpr(g.*, .{});
                },
            }
        },
        .Unary => |u| {
            try self.compileExpr(u.rhs.*, .{});

            switch (u.op) {
                .UnaryMinus => try self.scope.chunk.append(.{
                    .kind = .UnaryMinus,
                }),
                .Neg => try self.scope.chunk.append(.{
                    .kind = .Negate,
                }),
                .Len => @panic("TODO"),
            }
        },
        else => {
            std.debug.print("ERROREXP: {}\n", .{expr});
            unreachable;
        },
    }
}

pub fn compileFunccall(self: *@This(), fc: AST.FunctionCall, is_stmt: bool) anyerror!void {
    // TODO: check that there are enough args
    for (fc.args) |arg| {
        try self.compileExpr(arg.*, .{});
    }

    try self.compileExpr(.{ .Prefix = fc.callee.* }, .{});

    try self.scope.chunk.append(.{
        .kind = .Call,
        .A = fc.args.len,
        .B = @intFromBool(!is_stmt),
    });
}
