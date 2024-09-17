use crate::ast::{Expr, Type, Value};

#[derive(Debug)]
pub enum FileMode {
    Read,
    Write,
}

#[derive(Debug)]
pub enum Stmt {
    ProcedureDecl {
        name: Expr,
        params: Vec<Parameter>,
        body: Block,
    },
    FunctionDecl {
        name: Expr,
        params: Vec<Parameter>,
        return_type: Type,
        body: Block,
    },
    If {
        condition: Expr,
        then_branch: Block,
        else_branch: Block,
    },
    CaseOf {
        condition: Expr,
        cases: Vec<(Expr, Stmt)>,
        otherwise: Box<Stmt>,
    },
    ForLoop {
        target: Expr,
        start: Expr,
        end: Expr,
        step: Expr,
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
        value: Value,
    },
    Input(Vec<Expr>),
    Output(Vec<Expr>),
    Return(Expr),
    FileOpen {
        file: Value,
        mode: FileMode,
    },
    FileRead {
        file: Value,
        target: Expr,
    },
    FileWrite {
        file: Value,
        value: Expr,
    },
    FileClose {
        file: Value,
    },
    Procedure {
        name: Expr,
        args: Vec<Expr>,
    },
    Assignment {
        target: Expr,
        value: Expr,
    },
}

#[derive(Debug, Default)]
pub struct Block(pub Vec<Stmt>);

#[derive(Debug)]
pub struct Parameter {
    pub name: Expr,
    pub type_: Type,
}
