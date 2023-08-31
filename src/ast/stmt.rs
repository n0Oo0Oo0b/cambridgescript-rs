use crate::ast::{Expr, Identifier, Type, Literal};

#[derive(Debug)]
pub enum FileMode {
    Read,
    Write
}

pub enum Stmt {
    ProcedureDecl {
        name: Identifier,
        params: Option<Vec<Parameter>>,
        body: Block,
    },
    FunctionDecl {
        name: Identifier,
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
        name: Identifier,
        type_: Type,
    },
    ConstantDecl {
        name: Identifier,
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
        name: Identifier,
        args: Option<Vec<Expr>>,
    },
    Assignment {
        target: Expr,
        value: Expr,
    },
}

pub struct Block {
    contents: Vec<Stmt>,
}

pub struct Parameter {
    name: Identifier,
    type_: Type,
}
