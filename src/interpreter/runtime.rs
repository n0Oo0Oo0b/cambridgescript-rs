use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{files::SimpleFile, term};
use std::io::{self, Cursor, Read, Write};
use std::mem;

use crate::tree_parser::parse_expr;
use crate::{
    scanner::{Scanner, ScannerError},
    tree_parser::parser::{Parse, ParseError, ParseResult, ParseStream},
    tree_parser::{stmt, Value},
};

use super::{Exec, ProgramState};

#[derive(Debug, Clone)]
pub enum RuntimeError {
    UndeclaredVariable(usize),
    UndefinedVariable(usize),
    InvalidAssignmentType(usize, Value),
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

pub struct Interpreter {
    state: ProgramState,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            state: ProgramState::default(),
        }
    }

    pub fn repl(&mut self) -> io::Result<()> {
        let mut stdout = io::stdout();
        let stdin = io::stdin();

        loop {
            write!(stdout, "> ")?;
            stdout.flush()?;

            let mut input = String::new();
            stdin.read_line(&mut input)?;
            // match self.eval_src(&input) {
            //     Err(e) => self.show_diagnostic(&input, &e),
            //     Ok(v) => writeln!(stdout, "{v}")?,
            // }
            let result = self.exec_src(&input);
            io::copy(&mut self.take_stdout().as_ref(), &mut stdout)?;
            if let Err(e) = result {
                self.show_diagnostic(&input, &e);
            }
        }
    }

    fn parse<T, F>(&mut self, source: &str, parse_fn: F) -> InterpretResult<T>
    where
        F: Fn(&mut ParseStream) -> ParseResult<T>,
    {
        let mut scanner = Box::new(Scanner::new(source));
        let maybe_parsed = parse_fn(&mut ParseStream::from_scanner(&mut scanner));
        if !scanner.errors.is_empty() {
            return Err(scanner
                .errors
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
        let config = term::Config {
            start_context_lines: 3,
            end_context_lines: 3,
            ..Default::default()
        };
        let files = SimpleFile::new("program", source);
        for error in errors {
            term::emit(&mut wlock, &config, &files, &error.clone().into())
                .expect("Failed to display error");
        }
    }

    pub fn exec_src(&mut self, source: &str) -> InterpretResult<()> {
        let block = self.parse(source, stmt::Block::parse)?;
        block.exec(&mut self.state).map_err(|e| vec![e.into()])
    }

    #[allow(unused)]
    pub fn eval_src(&mut self, source: &str) -> InterpretResult<Value> {
        let expr = self.parse(source, parse_expr)?;
        expr.eval(&self.state).map_err(|e| vec![e.into()])
    }

    pub fn take_stdout(&mut self) -> Box<[u8]> {
        let mut value = Vec::new();
        mem::swap(&mut self.state.stdout, &mut value);
        value.into_boxed_slice()
    }
}
