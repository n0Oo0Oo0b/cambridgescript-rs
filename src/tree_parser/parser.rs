use std::collections::HashMap;
use std::rc::Rc;

use codespan::{ByteIndex, Span};

use crate::interpreter::{BoxEval, BoxExec};
use crate::scanner::{ScanResult, Scanner, ScannerError};
use crate::token::{ErrorLocation, Token, TokenType};
use crate::tree_parser::{expr, stmt, BinaryOp, UnaryOp};

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnexpectedToken(TokenType),
    ExpectedExpression,
    ExpectedStatement,
    ExpectedType,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub context: ParseContext,
    pub location: ErrorLocation,
    pub kind: ParseErrorKind,
}

type ParseContext = (&'static str, Option<Span>);
pub type ParseResult<T> = Result<T, ParseError>;

pub(crate) trait Parse: Sized {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self>;
}

// Helpers

macro_rules! token_of {
    ($($t:tt)*) => {
        Token {
            inner: TokenType::$($t)*,
            ..
        }
    };
}
pub(super) use token_of;

macro_rules! block_ending_with {
    ($end:pat) => {
        |stream: &mut ParseStream| {
            let mut items = Vec::new();
            loop {
                match stream.peek() {
                    Some($end) => break,
                    None => break,
                    _ => (),
                }
                items.push(stream.parse::<ParseableStmt>()?.into());
            }
            Ok(stmt::Block(items.into_boxed_slice()))
        }
    };
}
pub(super) use block_ending_with;

pub(crate) struct ParseStream<'s> {
    pub stream: &'s mut dyn Iterator<Item = Token>,
    pub peeked: Option<Option<Token>>,
    ident_map: &'s mut HashMap<Rc<str>, usize>,
    eof_index: Option<ByteIndex>,
}

impl<'s> ParseStream<'s> {
    pub fn new(
        stream: &'s mut dyn Iterator<Item = Token>,
        ident_map: &'s mut HashMap<Rc<str>, usize>,
    ) -> Self {
        Self {
            stream,
            peeked: None,
            ident_map,
            eof_index: None,
        }
    }

    pub fn from_scanner<'sc: 's>(
        scanner: &'sc mut Scanner<'sc, 'sc>,
        ident_map: &'s mut HashMap<Rc<str>, usize>,
    ) -> Self {
        let source_len = scanner.source.len() as u32;
        Self {
            stream: scanner as &'sc mut dyn Iterator<Item = Token>,
            peeked: None,
            ident_map,
            eof_index: Some(ByteIndex(source_len)),
        }
    }

    pub fn parse<T: Parse>(&mut self) -> ParseResult<T> {
        T::parse(self)
    }

    fn get_next(&mut self) -> Option<Token> {
        loop {
            // Skip whitespace and comment
            match self.stream.next() {
                Some(Token {
                    inner: TokenType::Whitespace | TokenType::Comment,
                    ..
                }) => continue,
                other => break other,
            }
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        // Manual get_or_insert_with() because of borrow rules
        if self.peeked.is_none() {
            self.peeked = Some(self.get_next());
        }
        unsafe { self.peeked.as_ref().unwrap_unchecked() }
            .as_ref()
            .cloned()
    }

    pub fn advance(&mut self) -> Option<Token> {
        self.peeked
            .take()
            .unwrap_or_else(|| self.get_next())
            .as_ref()
            .cloned()
    }

    pub fn consume(&mut self, kind: TokenType) -> Option<Token> {
        if self.peek().is_some_and(|t| t.inner == kind) {
            self.advance()
        } else {
            None
        }
    }

    #[inline]
    pub fn force_consume(&mut self, kind: TokenType, context: ParseContext) -> ParseResult<Token> {
        self.consume(kind.clone()).ok_or(
            self.error::<()>(ParseErrorKind::UnexpectedToken(kind), context)
                .unwrap_err(),
        )
    }

    #[inline]
    pub fn error<T>(
        &mut self,
        kind: ParseErrorKind,
        context: ParseContext,
    ) -> Result<T, ParseError> {
        Err(ParseError {
            context,
            location: match self.peek() {
                Some(t) => t.into(),
                None => ErrorLocation::Eof(self.eof_index),
            },
            kind,
        })
    }

    pub fn get_ident_handle(&mut self, name: Rc<str>) -> usize {
        *self
            .ident_map
            .try_insert(name, self.ident_map.len())
            .as_deref()
            .unwrap_or_else(|e| e.entry.get())
    }
}
