use std::iter::{Filter, Peekable};

use crate::ast::*;
use crate::scanner::{ScanResult, ScannerError};
use crate::token::{Token, TokenType};

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

type TokenPredicate = fn(&TokenType) -> bool;

type ParseResult<T> = Result<T, &'static str>;
type PrefixRule<I> = fn(&mut Parser<I>) -> ParseResult<Box<Expr>>;
type InfixRule<I> = fn(&mut Parser<I>, left: Box<Expr>) -> ParseResult<Box<Expr>>;

/// Parses an iterator of tokens into an AST.
/// I've tried to separate this from Scanner as much as possible.
pub struct Parser<I>
where
    I: Iterator<Item = TokenType>,
{
    tokens: Peekable<Filter<I, TokenPredicate>>,
    cur_prec: Precedence,
}

impl<I> Parser<I>
where
    I: Iterator<Item = TokenType>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens
                .filter(Self::token_predicate as TokenPredicate)
                .peekable(),
            cur_prec: Precedence::None,
        }
    }

    fn token_predicate(token: &TokenType) -> bool {
        !matches!(token, TokenType::Whitespace | TokenType::Comment)
    }

    #[inline(always)] // Rule tables are only used in parse_precedence
    fn unary_rule(token: TokenType) -> Option<(PrefixRule<I>, Precedence)> {
        use TokenType as TT;
        // Wrapping the whole thing in to avoid needing Some((a, b)) everywhere
        Some(match token {
            TT::Minus => (Parser::unary, Precedence::Unary),
            TT::Not => (Parser::unary, Precedence::LogicNot),
            TT::LParen => (Parser::grouping, Precedence::Primary),
            TT::CharLiteral(_)
            | TT::StringLiteral(_)
            | TT::IntegerLiteral(_)
            | TT::RealLiteral(_)
            | TT::BooleanLiteral(_) => (Parser::literal, Precedence::Primary),
            _ => return None,
        })
    }

    #[inline(always)]
    fn binary_rule(token: TokenType) -> Option<(InfixRule<I>, Precedence)> {
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
            TT::LParen => unimplemented!("function call"),
            TT::LBracket => unimplemented!("array indexing"),
            _ => return None,
        })
    }

    #[inline]
    fn peek(&mut self) -> Option<TokenType> {
        self.tokens.peek().cloned()
    }

    #[inline]
    fn advance(&mut self) -> ParseResult<TokenType> {
        self.tokens.next().ok_or("Unexpected EOF")
    }

    #[inline]
    fn consume(&mut self, ttype: TokenType, message: &'static str) -> ParseResult<()> {
        (self.advance()? == ttype).then_some(()).ok_or(message)
    }

    fn literal(&mut self) -> ParseResult<Box<Expr>> {
        let val = match self.advance()? {
            TokenType::IntegerLiteral(i) => Value::Integer(i),
            t => panic!("Invalid literal {t:?}"),
        };
        Ok(Expr::Literal(val).into())
    }

    fn grouping(&mut self) -> ParseResult<Box<Expr>> {
        // Propagate errors early
        let expr = self.parse_expr()?;
        self.consume(TokenType::RParen, "Expected ')' after expression")?;
        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<Box<Expr>> {
        let op = match self.advance()? {
            TokenType::Minus => UnaryOp::Neg,
            TokenType::Not => UnaryOp::Not,
            t => panic!("Invalid unary operator {t:?}"),
        };
        let right = self.parse_precedence(self.cur_prec.next())?;
        Ok(Expr::Unary { op, right }.into())
    }

    fn binary(&mut self, left: Box<Expr>) -> ParseResult<Box<Expr>> {
        let op = match self.advance()? {
            TokenType::And => BinaryOp::And,
            TokenType::Or => BinaryOp::Or,
            TokenType::Plus => BinaryOp::Add,
            TokenType::Minus => BinaryOp::Sub,
            TokenType::Star => BinaryOp::Mul,
            TokenType::Slash => BinaryOp::Div,
            TokenType::Equal => BinaryOp::Eq,
            TokenType::NotEqual => BinaryOp::Ne,
            TokenType::LessEqual => BinaryOp::Le,
            TokenType::GreaterEqual => BinaryOp::Ge,
            TokenType::Less => BinaryOp::Lt,
            TokenType::Greater => BinaryOp::Gt,
            t => panic!("Invalid binary operator {t:?}"),
        };
        let right = self.parse_precedence(self.cur_prec.next())?;
        Ok(Expr::Binary { left, op, right }.into())
    }

    fn parse_precedence(&mut self, prec: Precedence) -> ParseResult<Box<Expr>> {
        let unary_func: PrefixRule<I>;
        match self.peek().and_then(Self::unary_rule) {
            Some(t) => (unary_func, self.cur_prec) = t,
            None => return Err("Expected expression"),
        };
        let mut res = unary_func(self)?;

        loop {
            let binary_func: InfixRule<I>;
            match self.peek().and_then(Self::binary_rule) {
                Some((_, p)) if prec > p => break,
                Some(rule) => (binary_func, self.cur_prec) = rule,
                None => break,
            };
            res = binary_func(self, res)?;
        }
        Ok(res)
    }

    pub fn parse_expr(&mut self) -> ParseResult<Box<Expr>> {
        self.parse_precedence(Precedence::LogicOr)
    }

    pub fn parse_stmt(&mut self) -> ParseResult<Box<Stmt>> {
        match self.peek().ok_or("Expected Statement")? {
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(TokenType),
    UnexpectedEOF,
}

#[derive(Debug)]
pub enum CompileError<'a> {
    ScannerError(ScannerError<'a>),
    ParserError(ParserError),
}

impl<'a> From<ScannerError<'a>> for CompileError<'a> {
    fn from(value: ScannerError<'a>) -> Self {
        Self::ScannerError(value)
    }
}

impl From<ParserError> for CompileError<'_> {
    fn from(value: ParserError) -> Self {
        Self::ParserError(value)
    }
}

pub struct TokenTypeExtractor<'s, I>
where
    I: Iterator<Item = ScanResult<'s>>,
{
    stream: I,
    previous: Option<Token<'s>>,
    errors: Vec<CompileError<'s>>,
}

impl<'s, I> TokenTypeExtractor<'s, I>
where
    I: Iterator<Item = ScanResult<'s>>,
{
    pub fn new(stream: I) -> Self {
        Self {
            stream,
            previous: None,
            errors: Vec::new(),
        }
    }
}

impl<'s, I> Iterator for TokenTypeExtractor<'s, I>
where
    I: Iterator<Item = ScanResult<'s>>,
{
    type Item = TokenType;

    fn next(&mut self) -> Option<Self::Item> {
        // Consume and store any scanner errors
        self.previous = loop {
            match self.stream.next() {
                Some(Err(e)) => self.errors.push(e.into()),
                Some(Ok(t)) => break Some(t),
                None => break None,
            }
        };
        self.previous.clone().map(|t| t.type_)
    }
}
