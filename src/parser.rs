use std::collections::HashMap;
use std::rc::Rc;

use codespan::{ByteIndex, Span};

use crate::ast::*;
use crate::interpreter::{BoxEval, BoxExec};
use crate::scanner::{ScanResult, Scanner, ScannerError};
use crate::token::{ErrorLocation, Token, TokenType};

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

#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
enum Precedence {
    None,
    LogicOr,
    LogicAnd,
    LogicNot,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Exponent,
    Call,
    Primary,
}

impl Precedence {
    fn next(self) -> Self {
        if self == Self::Primary {
            panic!("Precedence::Primary is the highest precedence")
        }
        // Safety: transmuting x+1 is safe because x comes from a non-Primary variant
        unsafe { std::mem::transmute::<u8, Self>(self as u8 + 1) }
    }
}

/// Parses an iterator of tokens into an AST.
/// I've tried to separate this from Scanner as much as possible.
#[derive(Debug)]
pub struct Parser {
    cur_prec: Precedence,
    cur_ctx: ParseContext,
    ident_map: HashMap<Rc<str>, usize>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            cur_prec: Precedence::None,
            cur_ctx: ("", None),
            ident_map: HashMap::new(),
        }
    }
}

type PrefixRule<S> = fn(&mut Parser, &mut S) -> ParseResult<BoxEval>;
type InfixRule<S> = fn(&mut Parser, left: BoxEval, &mut S) -> ParseResult<BoxEval>;

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

impl Parser {
    fn make_error<S: TokenStream>(&self, stream: &mut S, kind: ParseErrorKind) -> ParseError {
        ParseError {
            context: self.cur_ctx,
            location: match stream.peek() {
                Some(t) => t.into(),
                None => ErrorLocation::Eof(stream.eof_index()),
            },
            kind,
        }
    }

    fn get_ident_handle(&mut self, name: Rc<str>) -> usize {
        *self
            .ident_map
            .try_insert(name, self.ident_map.len())
            .as_deref()
            .unwrap_or_else(|e| e.entry.get())
    }
}

/// Expression parsing
impl Parser {
    #[inline(always)] // Rule tables are only used in parse_precedence
    fn get_prefix_rule<S: TokenStream>(token: TokenType) -> Option<(PrefixRule<S>, Precedence)> {
        use TokenType as TT;
        // Wrapping the whole thing in to avoid needing Some((a, b)) everywhere
        Some(match token {
            TT::Minus => (Parser::unary, Precedence::Unary),
            TT::Not => (Parser::unary, Precedence::LogicNot),
            TT::LParen => (Parser::grouping, Precedence::Primary),
            TT::Identifier(_) => (Parser::ident, Precedence::Primary),
            TT::CharLiteral(_)
            | TT::StringLiteral(_)
            | TT::IntegerLiteral(_)
            | TT::RealLiteral(_)
            | TT::BooleanLiteral(_) => (Parser::literal, Precedence::Primary),
            _ => return None,
        })
    }

    #[inline(always)]
    fn get_infix_rule<S: TokenStream>(token: TokenType) -> Option<(InfixRule<S>, Precedence)> {
        use TokenType as TT;
        Some(match token {
            TT::Or => (Parser::binary, Precedence::LogicOr),
            TT::And => (Parser::binary, Precedence::LogicAnd),
            TT::Equal | TT::NotEqual => (Parser::binary, Precedence::Equality),
            TT::Less | TT::Greater | TT::LessEqual | TT::GreaterEqual => {
                (Parser::binary, Precedence::Comparison)
            }
            TT::Plus | TT::Minus => (Parser::binary, Precedence::Term),
            TT::Star | TT::Slash => (Parser::binary, Precedence::Factor),
            TT::Caret => (Parser::binary, Precedence::Exponent),
            TT::LParen => (Parser::function_call, Precedence::Call),
            TT::LBracket => unimplemented!("array indexing"),
            _ => return None,
        })
    }

    fn literal<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<BoxEval> {
        let token = stream.advance().expect("Literal token");
        Ok(Box::new(expr::Literal {
            value: token.type_.try_into().expect("Literal token"),
            span: token.span,
        }))
    }

    fn ident<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<BoxEval> {
        let (ident, span) = match stream.advance() {
            Some(Token {
                type_: TokenType::Identifier(i),
                span,
            }) => (i, span),
            _ => panic!("ident() called without identifier token"),
        };
        let handle = self.get_ident_handle(ident);
        Ok(Box::new(expr::Identifier { handle, span }))
    }

    fn grouping<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<BoxEval> {
        // Propagate errors early
        self.cur_ctx = (
            "close paren",
            stream.consume(TokenType::LParen).expect("Grouping").span,
        );
        let expr = self.parse_expr(stream)?;
        force_consume!(self, stream; TokenType::RParen);
        Ok(expr)
    }

    fn unary<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<BoxEval> {
        let (op, op_span) = match stream.advance().expect("Unary operator") {
            Token {
                type_: TokenType::Minus,
                span,
            } => (UnaryOp::Neg, span),
            Token {
                type_: TokenType::Not,
                span,
            } => (UnaryOp::Not, span),
            t => panic!("Invalid unary operator {t:?}"),
        };
        let right = self.parse_precedence(stream, self.cur_prec.next(), "after unary operator")?;
        Ok(Box::new(expr::UnaryExpr { op, right, op_span }))
    }

    fn function_call<S: TokenStream>(
        &mut self,
        _left: BoxEval,
        stream: &mut S,
    ) -> ParseResult<BoxEval> {
        self.cur_ctx = (
            "function call paren",
            stream.advance().expect("LParen").span,
        );
        force_consume!(self, stream; TokenType::RParen);
        todo!("Function call parsing")
    }

    fn binary<S: TokenStream>(&mut self, left: BoxEval, stream: &mut S) -> ParseResult<BoxEval> {
        let op = match stream.advance().expect("Binary operator") {
            token_of!(And) => BinaryOp::And,
            token_of!(Or) => BinaryOp::Or,
            token_of!(Plus) => BinaryOp::Add,
            token_of!(Minus) => BinaryOp::Sub,
            token_of!(Star) => BinaryOp::Mul,
            token_of!(Slash) => BinaryOp::Div,
            token_of!(Caret) => BinaryOp::Pow,
            token_of!(Equal) => BinaryOp::Eq,
            token_of!(NotEqual) => BinaryOp::Ne,
            token_of!(LessEqual) => BinaryOp::Le,
            token_of!(GreaterEqual) => BinaryOp::Ge,
            token_of!(Less) => BinaryOp::Lt,
            token_of!(Greater) => BinaryOp::Gt,
            t => panic!("Invalid binary operator {t:?}"),
        };
        let right = self.parse_precedence(stream, self.cur_prec.next(), "after binary operator")?;
        Ok(Box::new(expr::BinaryExpr { left, op, right }))
    }

    fn parse_precedence<S: TokenStream>(
        &mut self,
        stream: &mut S,
        prec: Precedence,
        _context: &'static str,
    ) -> ParseResult<BoxEval> {
        let unary_func: PrefixRule<S>;
        match stream.peek().and_then(|t| Self::get_prefix_rule(t.type_)) {
            Some(t) => (unary_func, self.cur_prec) = t,
            None => return Err(self.make_error(stream, ParseErrorKind::ExpectedExpression)),
        };
        let mut res = unary_func(self, stream)?;

        loop {
            let binary_func: InfixRule<S>;
            match stream.peek().and_then(|t| Self::get_infix_rule(t.type_)) {
                Some((_, p)) if prec > p => break,
                Some(rule) => (binary_func, self.cur_prec) = rule,
                None => break,
            };
            res = binary_func(self, res, stream)?;
        }
        Ok(res)
    }

    pub fn parse_expr<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<BoxEval> {
        self.parse_precedence(stream, Precedence::LogicOr, "expression")
    }
}

/// Statement parsing
impl Parser {
    fn arguments<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<Box<[BoxEval]>> {
        let mut items = vec![self.parse_expr(stream)?];
        while stream.consume(TokenType::Comma).is_some() {
            items.push(self.parse_expr(stream)?);
        }
        Ok(items.into())
    }

    #[inline]
    fn if_stmt<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<BoxExec> {
        self.cur_ctx = (
            "after if condition",
            stream.consume(TokenType::If).expect("IF").span,
        );
        let condition = self.parse_expr(stream)?;
        force_consume!(self, stream; TokenType::Then);
        let then_branch = parse_block!(self, stream; token_of!(Else) | token_of!(EndIf))?;
        let else_branch = if stream.consume(TokenType::Else).is_some() {
            parse_block!(self, stream; token_of!(EndIf))?
        } else {
            Box::new([])
        };
        self.cur_ctx.0 = "after if statement";
        force_consume!(self, stream; TokenType::EndIf);
        Ok(Box::new(stmt::If {
            condition,
            then_branch,
            else_branch,
        }))
    }

    #[inline]
    fn repeat_until<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<BoxExec> {
        self.cur_ctx = (
            "after repeat-until condition",
            stream.consume(TokenType::Repeat).expect("REPEAT").span,
        );
        let body = parse_block!(self, stream; token_of!(Until))?;
        force_consume!(self, stream; TokenType::Until);
        let condition = self.parse_expr(stream)?;
        Ok(Box::new(stmt::ConditionalLoop {
            body,
            condition,
            post_condition: true,
        }))
    }

    pub fn parse_stmt<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<BoxExec> {
        match stream
            .peek()
            .ok_or_else(|| self.make_error(stream, ParseErrorKind::ExpectedStatement))?
        {
            token_of!(If) => self.if_stmt(stream),
            token_of!(Repeat) => self.repeat_until(stream),
            token_of!(While) => {
                self.cur_ctx = ("after while condition", stream.advance().unwrap().span);
                let condition = self.parse_expr(stream)?;
                force_consume!(self, stream; TokenType::Do);
                let body = parse_block!(self, stream; token_of!(EndWhile))?;
                self.cur_ctx.0 = "after while loop";
                force_consume!(self, stream; TokenType::EndWhile);
                Ok(Box::new(stmt::ConditionalLoop {
                    condition,
                    body,
                    post_condition: false,
                }))
            }

            token_of!(Output) => {
                stream.advance();
                Ok(Box::new(stmt::Output(self.arguments(stream)?)))
            }
            token_of!(Input) => {
                stream.advance();
                Ok(Box::new(stmt::Input(self.arguments(stream)?)))
            }

            // Try to interpret as assignment
            // NOTE: no consume guarantees?
            _ => match stream.peek() {
                Some(Token {
                    type_: TokenType::Identifier(i),
                    span,
                }) => {
                    // TODO: update after spanned expr
                    self.cur_ctx = ("for assignment", Some(Span::initial()));
                    force_consume!(self, stream; TokenType::LArrow);
                    let value = self.parse_expr(stream)?;
                    let target = expr::Identifier {
                        handle: self.get_ident_handle(i),
                        span,
                    };
                    Ok(Box::new(stmt::Assignment { target, value }))
                }
                _ => Err(self.make_error(stream, ParseErrorKind::ExpectedStatement)),
            },
        }
    }

    pub fn parse_program<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<Box<[BoxExec]>> {
        parse_block!(self, stream)
    }
}

pub trait TokenStream {
    fn peek(&mut self) -> Option<Token>;

    fn advance(&mut self) -> Option<Token>;

    fn consume(&mut self, ttype: TokenType) -> Option<Token> {
        if self.peek().map_or(false, |t| t.type_ == ttype) {
            self.advance()
        } else {
            None
        }
    }

    fn eof_index(&self) -> Option<ByteIndex> {
        None
    }
}

#[derive(Debug)]
pub struct TokenExtractor<I>
where
    I: Iterator<Item = ScanResult>,
{
    stream: I,
    pub previous: Option<Option<Token>>,
    pub scan_errors: Vec<ScannerError>,
}

impl<I> TokenExtractor<I>
where
    I: Iterator<Item = ScanResult>,
{
    pub fn new(stream: I) -> Self {
        Self {
            stream,
            previous: None,
            scan_errors: Vec::new(),
        }
    }

    fn next_inner(&mut self) -> Option<Token> {
        loop {
            match self.stream.next() {
                Some(Err(e)) => {
                    self.scan_errors.push(e);
                }
                Some(Ok(Token {
                    type_: TokenType::Whitespace | TokenType::Comment,
                    ..
                })) => (),
                Some(Ok(t)) => break Some(t),
                None => break None,
            }
        }
    }
}

impl<'s> TokenStream for TokenExtractor<Scanner<'s, 's>> {
    fn peek(&mut self) -> Option<Token> {
        // Manual version of get_or_insert_with due to borrow checking rules
        if self.previous.is_none() {
            self.previous = Some(self.next_inner());
        }
        unsafe { self.previous.as_ref().unwrap_unchecked() }
            .as_ref()
            .cloned()
    }

    fn advance(&mut self) -> Option<Token> {
        self.previous
            .take()
            .unwrap_or_else(|| self.next_inner())
            .as_ref()
            .cloned()
    }

    fn eof_index(&self) -> Option<ByteIndex> {
        let len = self.stream.source.len() as u32;
        Some(ByteIndex(len))
    }
}
