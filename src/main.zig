const std = @import("std");
const builtin = @import("builtin");
const Scanner = @import("scanner.zig");
const Parser = @import("parser.zig");
const Compiler = @import("compiler.zig");
const VMInterpreter = @import("vm/interpreter.zig");

const Command = enum {
    @"allocator-benchmark",
    @"list-tokens",
    @"list-ast",
    run,
};

/// Indicates wheteher the backing allocator is Jdz (true) or GeneralPurposeAllocator (false)
pub const FastAllocator = builtin.mode != .Debug;

const jdz = if (FastAllocator) @import("jdz_allocator");

pub fn main() !u8 {
    var gpa_maybe_jdz = if (FastAllocator)
        jdz.JdzAllocator(.{}).init()
    else
        std.heap.GeneralPurposeAllocator(.{}){};
    const backing_allocator = gpa_maybe_jdz.allocator();
    defer _ = gpa_maybe_jdz.deinit();

    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // First argmuent must be an executable
    std.debug.assert(args.len >= 1);

    if (args.len < 2) {
        help_menu();
        return 0;
    }

    const command: Command = std.meta.stringToEnum(Command, args[1]) orelse {
        help_menu();
        return 0;
    };

    _ = switch (command) {
        .@"allocator-benchmark" => allocator_benchmark(allocator),
        .@"list-tokens" => list_tokens(allocator, args),
        .@"list-ast" => list_ast(allocator, args),
        .run => run(allocator, args),
    } catch |e| {
        if (error_handled(e)) {
            std.log.err("Unexpected error happened while executing the program.", .{});
        } else {
            std.log.err("Unexpected error \"{s}\" happened while executing the program.", .{@errorName(e)});
        }

        return 1;
    };

    return 0;
}

fn help_menu() void {
    std.io.getStdOut().writer().writeAll(
        \\allocator-benchmark - Benchmark the allocator, also tells if the uses an optimized allocator 
        \\list-tokens <file to scan> <optional filepath, stdout default> - Scan a file and list all it's tokens 
        \\list-ast <file to parse> <optional filepath, stdout default> - Scan a file and list it's ast 
        \\run <file to run> - You know what it does? Yeah me neither
        \\
    ) catch unreachable;
}

fn error_handled(e: anyerror) bool {
    return e == error.HandledByOni;
}

// allocator-benchmark
const BenchmarkAmount = 100_000;
const Item = [64]u21;

fn allocator_benchmark(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Allocating an item of size {d} {d} times for the benchmark\n", .{ @sizeOf(Item), BenchmarkAmount });

    const start = std.time.milliTimestamp();

    for (0..BenchmarkAmount) |_| {
        const alloc = try allocator.create(Item);
        allocator.free(alloc);
    }

    const end = std.time.milliTimestamp();

    try stdout.print("Took {d}ms\n", .{end - start});
    return;
}

// list-tokens
fn list_tokens(allocator: std.mem.Allocator, args: [][]u8) !void {
    if (args.len < 3)
        return error.NotEnoughArguments;

    const scanning_file = try std.fs.cwd().readFileAlloc(
        allocator,
        args[2],
        std.math.maxInt(usize),
    );
    defer allocator.free(scanning_file);

    const file = if (args.len >= 4)
        try create_file(args[3], .AskToOverwrite)
    else
        std.io.getStdOut();

    var buffered = std.io.bufferedWriter(file.writer());
    const writer = buffered.writer();

    var scanner = Scanner.init(allocator, scanning_file);
    defer scanner.deinit();

    for (try scanner.scanArray()) |token| {
        try writer.print("{}: {s}\n", .{ token.kind, token.lexeme });
    }

    try buffered.flush();
}

// list-ast
fn list_ast(allocator: std.mem.Allocator, args: [][]u8) !void {
    if (args.len < 3)
        return error.NotEnoughArguments;

    const scanning_file = try std.fs.cwd().readFileAlloc(
        allocator,
        args[2],
        std.math.maxInt(usize),
    );
    defer allocator.free(scanning_file);

    const file = if (args.len >= 4)
        try create_file(args[3], .AskToOverwrite)
    else
        std.io.getStdOut();

    var buffered = std.io.bufferedWriter(file.writer());
    const writer = buffered.writer();

    var scanner = Scanner.init(allocator, scanning_file);
    defer scanner.deinit();

    const tokens = try scanner.scanArray();

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    const root = try parser.parseRoot();

    for (root, 0..) |tree, idx| {
        try writer.print("{d}: {}\n", .{ idx, deeperFmt(tree, 32) });
    }

    try buffered.flush();
}

// list-ast
fn run(allocator: std.mem.Allocator, args: [][]u8) !void {
    if (args.len < 3)
        return error.NotEnoughArguments;

    const scanning_file = try std.fs.cwd().readFileAlloc(
        allocator,
        args[2],
        std.math.maxInt(usize),
    );
    defer allocator.free(scanning_file);

    var scanner = Scanner.init(allocator, scanning_file);
    defer scanner.deinit();

    const tokens = try scanner.scanArray();

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    const root = try parser.parseRoot();

    var compiler = try Compiler.init(allocator);
    defer compiler.deinit();

    const root_func = try compiler.compileRoot(root);
    try VMInterpreter.execute(allocator, root_func);
}

pub fn create_file(path: []const u8, exists_behaviour: enum { AskToOverwrite, Error }) !std.fs.File {
    return std.fs.cwd().createFile(path, .{
        .exclusive = true,
    }) catch |e| switch (e) {
        error.PathAlreadyExists => {
            switch (exists_behaviour) {
                .AskToOverwrite => {
                    std.log.err("Path \"{s}\" already exists, enter Y to overwrite or N to exit", .{path});

                    while (true) {
                        const c = try std.io.getStdIn().reader().readByte();

                        switch (c) {
                            'Y', 'y' => return std.fs.cwd().createFile(path, .{
                                .exclusive = false,
                                .truncate = true,
                            }),
                            'N', 'n' => return error.HandledByOni,
                            else => {},
                        }
                    }
                },
                .Error => return e,
            }
        },
        else => return e,
    };
}

pub fn deeperFmt(value: anytype, max_depth: usize) DeeperFmt(@TypeOf(value)) {
    return .{ .value = value, .max_depth = max_depth };
}

fn DeeperFmt(comptime T: type) type {
    return struct {
        value: T,
        max_depth: usize,

        pub fn format(self: @This(), comptime fmt_str: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            try std.fmt.formatType(self.value, fmt_str, options, writer, self.max_depth);
        }
    };
}
