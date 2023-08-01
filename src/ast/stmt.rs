use crate::ast::{Expr, Identifier, Type, Literal};

#[derive(Debug)]
pub enum FileMode {
    Read,
    Write
}

pub enum Stmt {
    ProcedureDecl(ProcedureDeclStmt),
    FunctionDecl(FunctionDeclStmt),
    If(IfStmt),
    CaseOf(Box<CaseOfStmt>),
    ForLoop(ForLoopStmt),
    RepeatUntil(RepeatUntilstmt),
    While(WhileStmt),
    VariableDecl(VariableDeclStmt),
    ConstantDecl(ConstantDeclStmt),
    Input(InputStmt),
    Output(OutputStmt),
    Return(ReturnStmt),
    FileOpen(FileOpenStmt),
    FileRead(FileReadStmt),
    FileWrite(FileWriteStmt),
    FileClose(FileCloseStmt),
    Procedure(ProcedureStmt),
    Assignment(AssignmentStmt),
}

pub struct Block {
    contents: Vec<Stmt>,
}

pub struct Parameter {
    name: Identifier,
    type_: Type,
}

pub struct ProcedureDeclStmt {
    name: Identifier,
    params: Option<Vec<Parameter>>,
    body: Block,
}

pub struct FunctionDeclStmt {
    name: Identifier,
    params: Option<Vec<Parameter>>,
    body: Block,
}

pub struct IfStmt {
    condition: Expr,
    then_branch: Block,
    else_branch: Option<Block>,
}

pub struct CaseOfStmt {
    condition: Expr,
    cases: Vec<(Expr, Stmt)>,
    otherwise: Option<Stmt>,
}

pub struct ForLoopStmt {
    target: Expr,
    start: Expr,
    end: Expr,
    step: Option<Expr>,
    body: Block,
}

pub struct RepeatUntilstmt {
    body: Block,
    condition: Expr,
}

pub struct WhileStmt {
    condition: Expr,
    body: Block,
}

pub struct VariableDeclStmt {
    name: Identifier,
    type_: Type,
}

pub struct ConstantDeclStmt {
    name: Identifier,
    value: Literal,
}

pub struct InputStmt {
    target: Expr,
}

pub struct OutputStmt {
    items: Vec<Expr>,
}

pub struct ReturnStmt {
    value: Expr,
}

pub struct FileOpenStmt {
    file: Literal,
    mode: FileMode,
}

pub struct FileReadStmt {
    file: Literal,
    target: Expr,
}

pub struct FileWriteStmt {
    file: Literal,
    value: Expr,
}

pub struct FileCloseStmt {
    file: Literal,
}

pub struct ProcedureStmt {
    name: Identifier,
    args: Option<Vec<Expr>>,
}

pub struct AssignmentStmt {
    target: Expr,
    value: Expr,
}
