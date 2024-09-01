use std::{collections::HashMap, io::Cursor};

use crate::{
    ast::{Expr, Value},
    parser::{ParseResult, Parser, ParserError, TokenTypeExtractor},
    scanner::{iter_tokens, ScannerError},
};

use super::eval::Eval;

#[derive(Debug)]
pub enum RuntimeError {
    UndeclaredVariable(usize),
    UndefinedVariable(usize),
    IncompatibleTypes(Value, Value),
}

#[derive(Debug)]
pub enum InterpretError<'a> {
    Scanner(ScannerError<'a>),
    Parser(ParserError),
    Runtime(RuntimeError),
}

impl<'a> From<ScannerError<'a>> for InterpretError<'a> {
    fn from(value: ScannerError<'a>) -> Self {
        InterpretError::Scanner(value)
    }
}

impl From<ParserError> for InterpretError<'_> {
    fn from(value: ParserError) -> Self {
        InterpretError::Parser(value)
    }
}

impl From<RuntimeError> for InterpretError<'_> {
    fn from(value: RuntimeError) -> Self {
        InterpretError::Runtime(value)
    }
}

pub(super) type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Default)]
pub(super) struct ProgramState {
    pub variables: HashMap<usize, Option<Value>>,
    stdout: Cursor<Vec<u8>>,
}

pub struct Interpreter {
    state: ProgramState,
    parser: Parser,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            state: ProgramState::default(),
            parser: Parser::new(),
        }
    }

    pub fn eval(&mut self, source: &str) -> Result<Value, InterpretError> {
        let mut tokens = TokenTypeExtractor::new(iter_tokens(source));
        let expr = self.parser.parse_expr(&mut tokens);
        dbg!(tokens.errors);
        let result = (expr?).eval(&self.state)?;
        Ok(result)
    }
}
