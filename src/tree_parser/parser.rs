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

type PrefixRule = fn(&mut ParseStream) -> ParseResult<BoxEval>;
type InfixRule = fn(&mut ParseStream, left: BoxEval) -> ParseResult<BoxEval>;

// Helpers

macro_rules! force_consume {
    ($self:ident, $stream:ident; $ttype:expr) => {
        $stream
            .consume($ttype)
            .ok_or_else(|| $self.make_error($stream, ParseErrorKind::UnexpectedToken($ttype)))?;
    };
}

macro_rules! token_of {
    ($($t:tt)*) => {
        Token {
            type_: TokenType::$($t)*,
            ..
        }
    };
}
pub(super) use token_of;

macro_rules! parse_block {
    ($self:ident, $stream:expr; $end:pat) => {{
        let mut items = Vec::new();
        while !matches!($stream.peek(), Some($end)) {
            items.push($self.parse_stmt($stream)?);
        }
        Ok(items.into_boxed_slice())
    }};
    ($self:ident, $stream:expr) => {{
        let mut items = Vec::new();
        while $stream.peek().is_some() {
            items.push($self.parse_stmt($stream)?);
        }
        Ok(items.into_boxed_slice())
    }};
}

pub(crate) struct ParseStream<'a> {
    stream: &'a mut dyn Iterator<Item = Token>,
    pub peeked: Option<Option<Token>>,
    ident_map: HashMap<Rc<str>, usize>,
    eof_index: Option<ByteIndex>,
}

impl<'a> ParseStream<'a> {
    pub fn new(stream: &'a mut dyn Iterator<Item = Token>) -> Self {
        Self {
            stream,
            peeked: None,
            ident_map: HashMap::new(),
            eof_index: None,
        }
    }

    pub fn from_scanner<'s>(scanner: &'a mut Scanner<'s, 's>) -> Self {
        let source_len = scanner.source.len() as u32;
        Self {
            stream: scanner as &'a mut dyn Iterator<Item = Token>,
            peeked: None,
            ident_map: HashMap::new(),
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
                    type_: TokenType::Whitespace | TokenType::Comment,
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
        if self.peek().is_some_and(|t| t.type_ == kind) {
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
