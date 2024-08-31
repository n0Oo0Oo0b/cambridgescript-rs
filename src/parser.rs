use std::iter::Peekable;

use crate::ast::*;
use crate::scanner::{ScanResult, ScannerError};
use crate::token::{Token, TokenType};

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken {
        expected: TokenType,
        context: &'static str,
    },
    UnexpectedEOF {
        context: &'static str,
    },
    ExpectedExpression {
        context: &'static str,
    },
    ExpectedStatement,
}

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

type ParseResult<T> = Result<T, ParserError>;
type PrefixRule<S> = fn(&mut Parser, &mut S) -> ParseResult<Expr>;
type InfixRule<S> = fn(&mut Parser, left: Box<Expr>, &mut S) -> ParseResult<Expr>;

/// Parses an iterator of tokens into an AST.
/// I've tried to separate this from Scanner as much as possible.
pub struct Parser {
    cur_prec: Precedence,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            cur_prec: Precedence::None,
        }
    }
}

macro_rules! force_consume {
    ($stream:ident, $ttype:expr, $context:literal) => {
        $stream.consume($ttype).ok_or(ParserError::UnexpectedToken {
            expected: $ttype,
            context: $context,
        })
    };
}

/// Expression parsing
impl Parser {
    #[inline(always)] // Rule tables are only used in parse_precedence
    fn unary_rule<S: TokenStream>(token: TokenType) -> Option<(PrefixRule<S>, Precedence)> {
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
    fn binary_rule<S: TokenStream>(token: TokenType) -> Option<(InfixRule<S>, Precedence)> {
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

    fn literal<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<Expr> {
        let val = match stream.advance().expect("Literal token") {
            TokenType::IntegerLiteral(i) => Value::Integer(i),
            t => panic!("Invalid literal {t:?}"),
        };
        Ok(Expr::Literal(val))
    }

    fn ident<S: TokenStream>(&mut self, _stream: &mut S) -> ParseResult<Expr> {
        Ok(Expr::Identifier { handle: 0 })
    }

    fn grouping<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<Expr> {
        // Propagate errors early
        let expr = self.parse_expr(stream)?;
        force_consume!(stream, TokenType::RParen, "after expression")?;
        Ok(expr)
    }

    fn unary<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<Expr> {
        let op = match stream.advance().expect("Unary operator") {
            TokenType::Minus => UnaryOp::Neg,
            TokenType::Not => UnaryOp::Not,
            t => panic!("Invalid unary operator {t:?}"),
        };
        let right = Box::new(self.parse_precedence(
            self.cur_prec.next(),
            "after unary operator",
            stream,
        )?);
        Ok(Expr::Unary { op, right })
    }

    fn binary<S: TokenStream>(&mut self, left: Box<Expr>, stream: &mut S) -> ParseResult<Expr> {
        let op = match stream.advance().expect("Binary operator") {
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
        let right = Box::new(self.parse_precedence(
            self.cur_prec.next(),
            "after binary operator",
            stream,
        )?);
        Ok(Expr::Binary { left, op, right })
    }

    fn parse_precedence<S: TokenStream>(
        &mut self,
        prec: Precedence,
        context: &'static str,
        stream: &mut S,
    ) -> ParseResult<Expr> {
        let unary_func: PrefixRule<S>;
        match stream.peek().and_then(Self::unary_rule) {
            Some(t) => (unary_func, self.cur_prec) = t,
            None => return Err(ParserError::ExpectedExpression { context }),
        };
        let mut res = unary_func(self, stream)?;

        loop {
            let binary_func: InfixRule<S>;
            match stream.peek().and_then(Self::binary_rule) {
                Some((_, p)) if prec > p => break,
                Some(rule) => (binary_func, self.cur_prec) = rule,
                None => break,
            };
            res = binary_func(self, Box::new(res), stream)?;
        }
        Ok(res)
    }

    pub fn parse_expr<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<Expr> {
        self.parse_precedence(Precedence::LogicOr, "", stream)
    }
}

/// Statement parsing
impl Parser {
    fn arguments<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<Vec<Expr>> {
        let mut items = vec![self.parse_expr(stream)?];
        while stream.consume(TokenType::Comma).is_some() {
            items.push(self.parse_expr(stream)?);
        }
        Ok(items)
    }

    #[inline]
    fn if_stmt<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<Stmt> {
        stream.consume(TokenType::If).expect("IF");
        let condition = self.parse_expr(stream)?;
        force_consume!(stream, TokenType::Then, "after IF condition")?;
        let then_branch = self.parse_block(stream)?;
        let else_branch = if stream.consume(TokenType::Else).is_some() {
            self.parse_block(stream)?
        } else {
            Block::default()
        };
        force_consume!(stream, TokenType::EndIf, "at the end of the IF statement")?;
        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    #[inline]
    fn repeat_until<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<Stmt> {
        stream.consume(TokenType::Repeat).expect("REPEAT");
        let body = self.parse_block(stream)?;
        force_consume!(stream, TokenType::Until, "after REPEAT-UNTIL condition")?;
        let condition = self.parse_expr(stream)?;
        Ok(Stmt::RepeatUntil { body, condition })
    }

    pub fn parse_stmt<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<Stmt> {
        match stream.peek().ok_or(ParserError::ExpectedStatement)? {
            TokenType::If => self.if_stmt(stream),
            TokenType::While => {
                stream.advance();
                let condition = self.parse_expr(stream)?;
                force_consume!(stream, TokenType::Do, "after WHILE condition")?;
                let body = self.parse_block(stream)?;
                force_consume!(stream, TokenType::EndWhile, "after while loop")?;
                Ok(Stmt::While { condition, body })
            }
            TokenType::Output => {
                stream.advance();
                Ok(Stmt::Output(self.arguments(stream)?))
            }
            TokenType::Input => {
                stream.advance();
                Ok(Stmt::Input(self.arguments(stream)?))
            }
            TokenType::EndIf
            | TokenType::EndCase
            | TokenType::EndWhile
            | TokenType::EndFunction
            | TokenType::EndProcedure => Err(ParserError::ExpectedStatement),
            t => todo!("{:?}", t),
        }
    }

    pub fn parse_block<S: TokenStream>(&mut self, stream: &mut S) -> ParseResult<Block> {
        let mut items = Vec::new();
        while let Ok(stmt) = self.parse_stmt(stream) {
            items.push(stmt);
        }
        Ok(Block(items))
    }
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
    pub previous: Option<Token<'s>>,
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

pub trait TokenStream {
    fn peek(&mut self) -> Option<TokenType>;
    fn advance(&mut self) -> Option<TokenType>;
    fn consume(&mut self, ttype: TokenType) -> Option<TokenType>;
}

impl<I> TokenStream for Peekable<I>
where
    I: Iterator<Item = TokenType>,
{
    #[inline]
    fn peek(&mut self) -> Option<TokenType> {
        // Ignore irrelevant tokens
        loop {
            match self.peek() {
                Some(TokenType::Comment | TokenType::Whitespace) => (),
                other => break other.cloned(),
            }
        }
    }

    #[inline]
    fn advance(&mut self) -> Option<TokenType> {
        // Peeked value was already checked
        self.next()
    }

    #[inline]
    fn consume(&mut self, ttype: TokenType) -> Option<TokenType> {
        if self.peek().map_or(false, |t| *t == ttype) {
            self.advance()
        } else {
            None
        }
    }
}
