use codespan::Span;

use crate::{
    ast::Value,
    interpreter::{BoxEval, BoxExec},
};

use super::{expr, MaybeSpanned};

#[derive(Debug)]
pub enum FileMode {
    Read,
    Write,
}

#[derive(Debug)]
pub enum FileOp {
    Open(FileMode),
    ReadTo(BoxEval),
    WriteFrom(BoxEval),
    Close,
}

type Type = ();

#[derive(Debug)]
pub struct Parameter {
    pub name: expr::Identifier,
    pub type_: Type,
}

pub mod stmt {
    use super::*;

    type Block = [BoxExec];

    // Basic statements

    #[derive(Debug)]
    pub struct Input(pub Box<[BoxEval]>);

    #[derive(Debug)]
    pub struct Output(pub Box<[BoxEval]>);

    #[derive(Debug)]
    pub struct Assignment {
        pub target: expr::Identifier,
        pub value: BoxEval,
    }

    #[derive(Debug)]
    pub struct FileOperation {
        pub file: expr::Literal,
        pub operation: FileOp,
    }

    #[derive(Debug)]
    pub struct ProcedureCall {
        pub name: BoxEval,
        pub args: Vec<BoxEval>,
    }

    // Control flow

    #[derive(Debug)]
    pub struct If {
        pub condition: BoxEval,
        pub then_branch: Box<Block>,
        pub else_branch: Box<Block>,
    }

    #[derive(Debug)]
    pub struct CaseOf {
        pub condition: BoxEval,
        pub cases: [(expr::Literal, BoxExec)],
    }

    #[derive(Debug)]
    pub struct ForLoop {
        pub target: BoxEval,
        pub range: (BoxEval, BoxEval, BoxEval),
        pub body: Block,
    }

    #[derive(Debug)]
    pub struct ConditionalLoop {
        pub condition: BoxEval,
        pub body: Box<Block>,
        pub post_condition: bool,
    }

    // Declarations

    #[derive(Debug)]
    pub struct VariableDecl {
        pub name: BoxEval,
        pub type_: Type,
    }

    #[derive(Debug)]
    pub struct ConstantDecl {
        pub name: BoxEval,
        pub value: Value,
    }

    #[derive(Debug)]
    pub struct ProcedureDecl {
        pub name: expr::Identifier,
        pub params: Box<[Parameter]>,
        pub body: Block,
    }

    #[derive(Debug)]
    pub struct FunctionDecl {
        pub name: BoxEval,
        pub return_type: Type,
        pub params: Box<[Parameter]>,
        pub body: Block,
    }

    #[derive(Debug)]
    pub struct Return(BoxEval);
}

impl MaybeSpanned for stmt::Output {
    fn get_span(&self) -> Option<Span> {
        if self.0.is_empty() {
            None
        } else {
            self.0[0].get_span()
        }
    }
}
