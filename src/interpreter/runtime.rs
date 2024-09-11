use codespan::Span;
use codespan_reporting::{
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use std::{
    collections::HashMap,
    io::{Cursor, Read},
};

use crate::{
    ast::Value,
    parser::{ParseResult, Parser, ParserError, TokenTypeExtractor},
    scanner::{scan_tokens, ScannerError, ScannerStream},
};

use super::{eval::Eval, exec::Exec};

#[derive(Debug)]
pub enum RuntimeError {
    UndeclaredVariable(usize),
    UndefinedVariable(usize),
    IncompatibleTypes(Value, Value),
    InvalidBool(Value),
}

#[derive(Debug)]
pub enum InterpretError {
    Scanner(ScannerError),
    Parser(ParserError, Span),
    Runtime(RuntimeError),
}

impl From<ScannerError> for InterpretError {
    fn from(value: ScannerError) -> Self {
        InterpretError::Scanner(value)
    }
}

impl From<RuntimeError> for InterpretError {
    fn from(value: RuntimeError) -> Self {
        InterpretError::Runtime(value)
    }
}

pub(super) type RuntimeResult<T> = Result<T, RuntimeError>;
pub(super) type InterpretResult<T> = Result<T, Vec<InterpretError>>;

impl TryFrom<Value> for bool {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Boolean(b) => Ok(b),
            val => Err(RuntimeError::InvalidBool(val)),
        }
    }
}

#[derive(Default)]
pub(super) struct ProgramState {
    pub variables: HashMap<usize, Option<Value>>,
    pub(super) stdout: Vec<u8>,
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

    fn parse<'s, T, F>(&mut self, source: &'s str, parse_fn: F) -> InterpretResult<T>
    where
        F: Fn(&mut Parser, &mut TokenTypeExtractor<ScannerStream<'s>>) -> ParseResult<T>,
    {
        let mut tokens = TokenTypeExtractor::new(scan_tokens(source));
        let maybe_parsed = parse_fn(&mut self.parser, &mut tokens);
        if !tokens.scan_errors.is_empty() {
            return Err(tokens
                .scan_errors
                .into_iter()
                .map(InterpretError::from)
                .collect());
        }
        match maybe_parsed {
            Ok(v) => Ok(v),
            Err(e) => {
                let eof_span = Span::new(source.len() as u32, source.len() as u32);
                let span = tokens.previous.unwrap().map_or(eof_span, |t| t.span);
                Err(vec![InterpretError::Parser(e, span)])
            }
        }
    }

    #[allow(unused)]
    pub fn show_diagnostic(&self, source: &str, errors: Vec<InterpretError>) {
        let stderr = StandardStream::stderr(ColorChoice::Always);
        let mut wlock = stderr.lock();
        let config = term::Config::default();
        let files = SimpleFile::new("program", source);
        for error in errors {
            term::emit(&mut wlock, &config, &files, &error.into())
                .expect("Failed to display error");
        }
    }

    pub fn exec_src(&mut self, source: &str) -> InterpretResult<()> {
        let stmt = self.parse(source, Parser::parse_stmt)?;
        stmt.exec(&mut self.state).map_err(|e| vec![e.into()])
    }

    #[allow(unused)]
    pub fn eval_src(&mut self, source: &str) -> InterpretResult<Value> {
        let expr = self.parse(source, Parser::parse_expr)?;
        expr.eval(&self.state).map_err(|e| vec![e.into()])
    }

    pub fn get_stdout(&self) -> impl Read + use<'_> {
        Cursor::new(&self.state.stdout)
    }
}
