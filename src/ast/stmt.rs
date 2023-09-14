use crate::ast::{Expr, Literal, Type};

#[derive(Debug)]
pub enum FileMode {
    Read,
    Write,
}

#[derive(Debug)]
pub enum Stmt {
    ProcedureDecl {
        name: Expr,
        params: Option<Vec<Parameter>>,
        body: Block,
    },
    FunctionDecl {
        name: Expr,
        params: Option<Vec<Parameter>>,
        body: Block,
    },
    If {
        condition: Expr,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    CaseOf {
        condition: Expr,
        cases: Vec<(Expr, Stmt)>,
        otherwise: Option<Box<Stmt>>,
    },
    ForLoop {
        target: Expr,
        start: Expr,
        end: Expr,
        step: Option<Expr>,
        body: Block,
    },
    RepeatUntil {
        body: Block,
        condition: Expr,
    },
    While {
        condition: Expr,
        body: Block,
    },
    VariableDecl {
        name: Expr,
        type_: Type,
    },
    ConstantDecl {
        name: Expr,
        value: Literal,
    },
    Input(Vec<Expr>),
    Output(Vec<Expr>),
    Return(Expr),
    FileOpen {
        file: Literal,
        mode: FileMode,
    },
    FileRead {
        file: Literal,
        target: Expr,
    },
    FileWrite {
        file: Literal,
        value: Expr,
    },
    FileClose {
        file: Literal,
    },
    Procedure {
        name: Expr,
        args: Option<Vec<Expr>>,
    },
    Assignment {
        target: Expr,
        value: Expr,
    },
}

#[derive(Debug)]
pub struct Block {
    contents: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Parameter {
    name: Expr,
    type_: Type,
}
