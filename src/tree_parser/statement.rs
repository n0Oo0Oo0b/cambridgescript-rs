use codespan::Span;

use crate::{
    interpreter::{BoxEval, BoxExec, Exec},
    scanner::ScanResult,
    token::{Token, TokenType},
};

use super::{
    expr, parse_arguments, parse_expr,
    parser::{token_of, Parse, ParseErrorKind, ParseResult, ParseStream},
    MaybeSpanned, Value,
};

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

    #[derive(Debug, Default)]
    pub struct Block(pub Box<[BoxExec]>);

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
        pub then_branch: Block,
        pub else_branch: Block,
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
        pub body: Block,
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

impl Parse for stmt::Block {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        let mut result = Vec::new();
        while stream.peek().is_some() {
            result.push(ParseableStmt::parse(stream)?.into())
        }
        Ok(stmt::Block(result.into_boxed_slice()))
    }
}

macro_rules! block_ending_with {
    ($stream:expr; $end:pat) => {{
        let mut items = Vec::new();
        while !matches!($stream.peek(), Some($end)) {
            items.push(ParseableStmt::parse($stream)?.into());
        }
        Ok(stmt::Block(items.into_boxed_slice()))
    }};
}

impl Parse for stmt::If {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        let condition = parse_expr(stream)?;
        stream.consume(TokenType::Then);
        let then_branch = block_ending_with!(stream; token_of!(Else) | token_of!(EndIf))?;
        let else_branch = if stream.consume(TokenType::Else).is_some() {
            block_ending_with!(stream; token_of!(EndIf))?
        } else {
            stmt::Block::default()
        };
        stream.consume(TokenType::EndIf);
        Ok(stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }
}

impl Parse for stmt::Output {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        stream.consume(TokenType::Output).expect("Literal token");
        Ok(Self(parse_arguments(stream)?))
    }
}

pub enum ParseableStmt {
    If(stmt::If),
    Output(stmt::Output),
}

impl Parse for ParseableStmt {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        match stream.peek() {
            Some(token_of!(If)) => Ok(Self::If(stream.parse()?)),
            Some(token_of!(Output)) => Ok(Self::Output(stream.parse()?)),
            Some(t) => stream.error(ParseErrorKind::UnexpectedToken(t.type_), todo!()),
            None => stream.error(ParseErrorKind::ExpectedStatement, todo!()),
        }
    }
}

impl From<ParseableStmt> for Box<dyn Exec> {
    fn from(value: ParseableStmt) -> Self {
        match value {
            ParseableStmt::If(x) => Box::new(x) as Self,
            ParseableStmt::Output(x) => Box::new(x) as Self,
        }
    }
}
