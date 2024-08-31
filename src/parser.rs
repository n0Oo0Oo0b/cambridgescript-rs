use std::iter::{Filter, Peekable};

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
type PrefixRule<I> = fn(&mut Parser<I>) -> ParseResult<Expr>;
type InfixRule<I> = fn(&mut Parser<I>, left: Box<Expr>) -> ParseResult<Expr>;

/// Parses an iterator of tokens into an AST.
/// I've tried to separate this from Scanner as much as possible.
pub struct Parser<I>
where
    I: Iterator<Item = TokenType>,
{
    tokens: Peekable<I>,
    cur_prec: Precedence,
}

impl<J> Parser<Filter<J, TokenPredicate>>
where
    J: Iterator<Item = TokenType>,
{
    pub fn new(tokens: J) -> Self {
        Self {
            tokens: tokens
                .filter(Self::token_predicate as TokenPredicate)
                .peekable(),
            cur_prec: Precedence::None,
        }
    }
}

impl<'a, J> Parser<Filter<TokenTypeExtractor<'a, J>, TokenPredicate>>
where
    J: Iterator<Item = ScanResult<'a>>,
{
    pub fn from_scan(tokens: J) -> Self {
        Self::new(TokenTypeExtractor::new(tokens))
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = TokenType>,
{
    fn token_predicate(token: &TokenType) -> bool {
        !matches!(token, TokenType::Whitespace | TokenType::Comment)
    }

    #[inline]
    fn peek(&mut self) -> Option<TokenType> {
        self.tokens.peek().cloned()
    }

    #[inline]
    fn advance(&mut self) -> Option<TokenType> {
        self.tokens.next()
    }

    #[inline]
    fn consume(&mut self, ttype: TokenType) -> Option<TokenType> {
        if self.peek().map_or(false, |t| t == ttype) {
            self.advance()
        } else {
            None
        }
    }

    #[inline]
    fn force_consume(&mut self, ttype: TokenType, context: &'static str) -> ParseResult<TokenType> {
        self.consume(ttype.clone())
            .ok_or(ParserError::UnexpectedToken {
                expected: ttype,
                context,
            })
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = TokenType>,
{
    #[inline(always)] // Rule tables are only used in parse_precedence
    fn unary_rule(token: TokenType) -> Option<(PrefixRule<I>, Precedence)> {
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

    fn literal(&mut self) -> ParseResult<Expr> {
        let val = match self.advance().expect("Literal token") {
            TokenType::IntegerLiteral(i) => Value::Integer(i),
            t => panic!("Invalid literal {t:?}"),
        };
        Ok(Expr::Literal(val))
    }

    fn ident(&mut self) -> ParseResult<Expr> {
        Ok(Expr::Identifier { handle: 0 })
    }

    fn grouping(&mut self) -> ParseResult<Expr> {
        // Propagate errors early
        let expr = self.parse_expr()?;
        self.force_consume(TokenType::RParen, "after expression")?;
        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        let op = match self.advance().expect("Unary operator") {
            TokenType::Minus => UnaryOp::Neg,
            TokenType::Not => UnaryOp::Not,
            t => panic!("Invalid unary operator {t:?}"),
        };
        let right = Box::new(self.parse_precedence(self.cur_prec.next(), "after unary operator")?);
        Ok(Expr::Unary { op, right })
    }

    fn binary(&mut self, left: Box<Expr>) -> ParseResult<Expr> {
        let op = match self.advance().expect("Binary operator") {
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
        let right = Box::new(self.parse_precedence(self.cur_prec.next(), "after binary operator")?);
        Ok(Expr::Binary { left, op, right })
    }

    fn parse_precedence(&mut self, prec: Precedence, context: &'static str) -> ParseResult<Expr> {
        let unary_func: PrefixRule<I>;
        match self.peek().and_then(Self::unary_rule) {
            Some(t) => (unary_func, self.cur_prec) = t,
            None => return Err(ParserError::ExpectedExpression { context }),
        };
        let mut res = unary_func(self)?;

        loop {
            let binary_func: InfixRule<I>;
            match self.peek().and_then(Self::binary_rule) {
                Some((_, p)) if prec > p => break,
                Some(rule) => (binary_func, self.cur_prec) = rule,
                None => break,
            };
            res = binary_func(self, Box::new(res))?;
        }
        Ok(res)
    }

    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_precedence(Precedence::LogicOr, "")
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = TokenType>,
{
    fn arguments(&mut self) -> ParseResult<Vec<Expr>> {
        let mut items = vec![self.parse_expr()?];
        while self.consume(TokenType::Comma).is_some() {
            items.push(self.parse_expr()?);
        }
        Ok(items)
    }

    #[inline]
    fn if_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::If).expect("IF");
        let condition = self.parse_expr()?;
        self.force_consume(TokenType::Then, "after IF condition")?;
        let then_branch = self.parse_block()?;
        let else_branch = if self.consume(TokenType::Else).is_some() {
            self.parse_block()?
        } else {
            Block::default()
        };
        self.force_consume(TokenType::EndIf, "at the end of the IF statement")?;
        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    #[inline]
    fn repeat_until(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::Repeat).expect("REPEAT");
        let body = self.parse_block()?;
        self.force_consume(TokenType::Until, "after REPEAT-UNTIL condition")?;
        let condition = self.parse_expr()?;
        Ok(Stmt::RepeatUntil { body, condition })
    }

    pub fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        match self.peek().ok_or(ParserError::ExpectedStatement)? {
            TokenType::If => self.if_stmt(),
            TokenType::While => {
                self.advance();
                let condition = self.parse_expr()?;
                self.force_consume(TokenType::Do, "after WHILE condition")?;
                let body = self.parse_block()?;
                self.force_consume(TokenType::EndWhile, "after while loop")?;
                Ok(Stmt::While { condition, body })
            }
            TokenType::Output => {
                self.advance();
                Ok(Stmt::Output(self.arguments()?))
            }
            TokenType::Input => {
                self.advance();
                Ok(Stmt::Input(self.arguments()?))
            }
            TokenType::EndIf
            | TokenType::EndCase
            | TokenType::EndWhile
            | TokenType::EndFunction
            | TokenType::EndProcedure => Err(ParserError::ExpectedStatement),
            t => todo!("{:?}", t),
        }
    }

    pub fn parse_block(&mut self) -> ParseResult<Block> {
        let mut items = Vec::new();
        while let Ok(stmt) = self.parse_stmt() {
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
