use codespan::Span;

use crate::{
    interpreter::{Assign, BoxEval, BoxExec, Exec},
    scanner::ScanResult,
    token::{Token, TokenType},
};

use super::{
    expr, parse_arguments, parse_assignable, parse_expr,
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
    pub struct AssignStmt {
        pub target: Box<dyn Assign>,
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
    pub struct IfStmt {
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
    pub struct WhileLoop {
        pub condition: BoxEval,
        pub body: Block,
    }

    #[derive(Debug)]
    pub struct UntilLoop {
        pub condition: BoxEval,
        pub body: Block,
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

impl Parse for stmt::IfStmt {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        stream.consume(TokenType::If).expect("If statement");
        let condition = parse_expr(stream)?;
        stream.force_consume(TokenType::Then, ("", None))?;
        let then_branch = block_ending_with!(stream; token_of!(Else) | token_of!(EndIf))?;
        let else_branch = if stream.consume(TokenType::Else).is_some() {
            block_ending_with!(stream; token_of!(EndIf))?
        } else {
            stmt::Block::default()
        };
        stream.force_consume(TokenType::EndIf, ("", None))?;
        Ok(stmt::IfStmt {
            condition,
            then_branch,
            else_branch,
        })
    }
}

impl Parse for stmt::WhileLoop {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        stream.consume(TokenType::While).expect("While loop");
        let condition = parse_expr(stream)?;
        stream.force_consume(TokenType::Do, ("", None))?;
        let body = block_ending_with!(stream; token_of!(EndWhile))?;
        stream.force_consume(TokenType::EndWhile, ("", None))?;
        Ok(Self { condition, body })
    }
}

impl Parse for stmt::UntilLoop {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        stream.consume(TokenType::Repeat).expect("Until loop");
        let body = block_ending_with!(stream; token_of!(Until))?;
        stream.force_consume(TokenType::Until, ("", None))?;
        let condition = parse_expr(stream)?;
        Ok(Self { condition, body })
    }
}

impl Parse for stmt::Output {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        stream.peek();
        stream.consume(TokenType::Output).expect("Output stmt");
        Ok(Self(parse_arguments(stream)?))
    }
}

impl Parse for stmt::AssignStmt {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        let target = parse_assignable(stream)?;
        stream.force_consume(TokenType::LArrow, ("", None))?;
        let value = parse_expr(stream)?;
        Ok(Self { target, value })
    }
}

pub enum ParseableStmt {
    If(stmt::IfStmt),
    While(stmt::WhileLoop),
    Until(stmt::UntilLoop),
    Output(stmt::Output),
    Assign(stmt::AssignStmt),
}

impl Parse for ParseableStmt {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        match stream.peek() {
            Some(token_of!(If)) => Ok(Self::If(stream.parse()?)),
            Some(token_of!(Output)) => Ok(Self::Output(stream.parse()?)),
            Some(token_of!(While)) => Ok(Self::While(stream.parse()?)),
            Some(token_of!(Repeat)) => Ok(Self::Until(stream.parse()?)),
            Some(t) => Ok(Self::Assign(stream.parse()?)),
            None => stream.error(ParseErrorKind::ExpectedStatement, todo!()),
        }
    }
}

impl From<ParseableStmt> for Box<dyn Exec> {
    fn from(value: ParseableStmt) -> Self {
        match value {
            ParseableStmt::If(x) => Box::new(x) as Self,
            ParseableStmt::While(x) => Box::new(x) as Self,
            ParseableStmt::Until(x) => Box::new(x) as Self,
            ParseableStmt::Output(x) => Box::new(x) as Self,
            ParseableStmt::Assign(x) => Box::new(x) as Self,
        }
    }
}
