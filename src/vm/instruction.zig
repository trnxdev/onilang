kind: Kind,
A: usize = 0,
B: usize = 0,

pub const Kind = enum {
    SetLocal, // A: Local Index
    GetLocal, // A: Local Index

    SetGlobal, // A: Global Index
    GetGlobal, // A: Global Index

    LoadConst, // A: Const Index
    Call, // A: Arity, B: true if it needs a return val

    Jump, // A: Where to jump
    Jump_True, // A: Where to jump
    Jump_False, // A: Where to jump

    Access, // Pops the value (1) and pops again (2). 2 is accessed with 1
    MakeArray, // A: How many items to pop and put into array

    UnaryMinus,
    Negate,
    Exit,

    Return, // A: If a value is returned

    Add,
    Sub,
    Mul,
    Div,
    Lte,
    Gte,
    Gt,
    Lt,
    Neql,
};
