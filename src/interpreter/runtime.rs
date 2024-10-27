use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{files::SimpleFile, term};
use colored::{Color, Colorize};
use std::collections::HashMap;
use std::io::{self, Write as _};
use std::rc::Rc;

use crate::tree_parser::{expr, PrimitiveType};
use crate::{
    scanner::{Scanner, ScannerError},
    tree_parser::parser::{Parse, ParseError, ParseResult, ParseStream},
    tree_parser::{stmt, Value},
};

use super::{Assign, Eval, Exec, ProgramState};

macro_rules! write_color {
    ($stream:expr, $color:expr, $($fmt:tt)*) => {
        write!($stream, "{}", format!($($fmt)*).color($color))
    };
}

#[derive(Debug, Clone)]
pub enum RuntimeError<'a> {
    UndeclaredVariable {
        var: &'a dyn Assign,
        required_type: Option<PrimitiveType>,
    },
    UndefinedVariable {
        var: &'a dyn Assign,
    },
    InvalidAssignType {
        target: &'a dyn Assign,
        expected_type: PrimitiveType,
        value: Value,
    },
    InvalidBinOpTypes {
        tree: &'a expr::BinaryExpr,
        lhs: Value,
        rhs: Value,
    },
    InvalidUnOpType {
        tree: &'a expr::UnaryExpr,
        rhs: Value,
    },
    InvalidBool {
        tree: &'a dyn Eval,
        value: Value,
    },
}

#[derive(Debug, Clone)]
pub enum InterpretError<'a> {
    Scanner(ScannerError),
    Parser(ParseError),
    Runtime(RuntimeError<'a>),
}

impl From<ScannerError> for InterpretError<'_> {
    fn from(value: ScannerError) -> Self {
        InterpretError::Scanner(value)
    }
}

impl From<ParseError> for InterpretError<'_> {
    fn from(value: ParseError) -> Self {
        InterpretError::Parser(value)
    }
}

impl<'a> From<RuntimeError<'a>> for InterpretError<'a> {
    fn from(value: RuntimeError<'a>) -> Self {
        InterpretError::Runtime(value)
    }
}

pub(super) type RuntimeResult<'a, T> = Result<T, RuntimeError<'a>>;
pub(super) type InterpretResult<'a, T> = Result<T, Vec<InterpretError<'a>>>;

pub struct Interpreter {
    state: ProgramState,
    ident_map: HashMap<Rc<str>, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            state: ProgramState::default(),
            ident_map: HashMap::new(),
        }
    }

    pub fn repl(&mut self) -> io::Result<()> {
        let mut stdout = io::stdout();
        let stdin = io::stdin();

        writeln!(stdout, "CambridgeScript REPL")?;
        loop {
            let mut lineno = 1usize;
            let mut input = String::new();
            loop {
                write_color!(stdout, Color::Blue, "{:2} │ ", lineno)?;
                lineno += 1;
                stdout.flush()?;
                if stdin.read_line(&mut input)? <= 1 {
                    break;
                }
            }
            writeln!(stdout)?;
            self.full_exec(&input);
        }
    }

    fn parse<'a, T, F>(
        ident_map: &mut HashMap<Rc<str>, usize>,
        source: &str,
        parse_fn: F,
    ) -> InterpretResult<'a, T>
    where
        F: FnOnce(&mut ParseStream) -> ParseResult<T>,
        T: 'a,
    {
        let mut scanner = Scanner::new(source);
        let scanner_ptr: *mut Scanner<'_, '_> = &mut scanner;
        let maybe_parsed = parse_fn(&mut ParseStream::from_scanner(&mut scanner, ident_map));
        // SAFETY: <Scanner as Iterator> doesn't access the scanner's errors
        let errors = unsafe { scanner_ptr.as_mut().unwrap().take_errors() };
        if !errors.is_empty() {
            return Err(errors.into_iter().map(InterpretError::from).collect());
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

    pub fn full_exec(&mut self, source: &str) {
        let mut maybe_block = None;
        let result = match Self::parse(&mut self.ident_map, source, stmt::Block::parse) {
            Ok(block) => maybe_block
                .insert(block)
                .exec(&mut self.state)
                .map_err(|e| vec![e.into()]),
            Err(e) => Err(e),
        };
        io::copy(&mut self.state.take_stdout().as_ref(), &mut io::stdout())
            .expect("Writing to stdout");
        if let Err(e) = result {
            self.show_diagnostic(source, &e);
        }
    }

    // pub fn exec_src<'a, 's>(&'a mut self, source: &'s str) -> InterpretResult<'a, ()> {
    //     let block = self.parse(source, stmt::Block::parse)?;
    //     block.exec(&mut self.state).map_err(|e| vec![e.into()])
    // }
    //
    // #[allow(unused)]
    // pub fn eval_src(&mut self, source: &str) -> InterpretResult<Value> {
    //     let expr = self.parse(source, parse_expr)?;
    //     expr.eval(&self.state).map_err(|e| vec![e.into()])
    // }
}
