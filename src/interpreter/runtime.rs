use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{files::SimpleFile, term};
use std::io::{Cursor, Read, Write};
use std::{collections::HashMap, fmt};

use crate::{
    ast::Value,
    parser::{ParseError, ParseResult, Parser, TokenExtractor},
    scanner::{Scanner, ScannerError},
};

use super::Exec;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    UndeclaredVariable(usize),
    UndefinedVariable(usize),
    IncompatibleTypes(Value, Value),
    InvalidBool(Value),
}

#[derive(Debug, Clone)]
pub enum InterpretError {
    Scanner(ScannerError),
    Parser(ParseError),
    Runtime(RuntimeError),
}

impl From<ScannerError> for InterpretError {
    fn from(value: ScannerError) -> Self {
        InterpretError::Scanner(value)
    }
}

impl From<ParseError> for InterpretError {
    fn from(value: ParseError) -> Self {
        InterpretError::Parser(value)
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
pub(crate) struct ProgramState {
    variables: HashMap<usize, Option<Value>>,
    pub(super) stdout: Vec<u8>,
}

impl ProgramState {
    pub fn get_variable(&self, handle: usize) -> RuntimeResult<Value> {
        Ok(self
            .variables
            .get(&handle)
            .ok_or(RuntimeError::UndeclaredVariable(handle))?
            .as_ref()
            .ok_or(RuntimeError::UndefinedVariable(handle))?
            .clone())
    }

    pub fn set_variable(&mut self, handle: usize, value: Value) -> RuntimeResult<()> {
        self.variables.insert(handle, Some(value));
        Ok(())
    }

    pub(super) fn write(&mut self, thing: impl AsRef<str>) {
        self.stdout
            .write_all(thing.as_ref().as_bytes())
            .expect("Failed to output");
    }

    pub(super) fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) {
        self.stdout.write_fmt(fmt).expect("Failed to output");
    }
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
        F: Fn(&mut Parser, &mut TokenExtractor<Scanner<'s, 's>>) -> ParseResult<T>,
    {
        let mut tokens = TokenExtractor::new(Scanner::new(source));
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
            Err(e) => Err(vec![e.into()]),
        }
    }

    pub fn show_diagnostic(&self, source: &str, errors: &Vec<InterpretError>) {
        let stderr = StandardStream::stderr(ColorChoice::Always);
        let mut wlock = stderr.lock();
        let config = term::Config::default();
        let files = SimpleFile::new("program", source);
        for error in errors {
            term::emit(&mut wlock, &config, &files, &error.clone().into())
                .expect("Failed to display error");
        }
    }

    pub fn exec_src(&mut self, source: &str) -> InterpretResult<()> {
        let block = self
            .parse(source, Parser::parse_program)
            .inspect_err(|e| self.show_diagnostic(source, e))?;
        block
            .exec(&mut self.state)
            .map_err(|e| vec![e.into()])
            .inspect_err(|e| self.show_diagnostic(source, e))
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
