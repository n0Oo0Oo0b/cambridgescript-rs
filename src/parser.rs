use crate::ast::*;
use crate::scanner::{Token, TokenType};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token),
    UnexpectedEOF,
}

struct TokenBuffer {
    items: Box<[Token]>,
    current: usize,
}

impl TokenBuffer {
    fn current_token(&self) -> Option<&Token> {
        if self.current < self.items.len() {
            Some(&self.items[self.current])
        } else {
            None
        }
    }

    fn peek(&self) -> Option<TokenType> {
        self.current_token().map(|t| t.type_.clone())
    }

    fn next(&mut self) -> Option<TokenType> {
        let res = self.peek();
        if res.is_some() {
            self.current += 1;
        };
        res
    }

    fn next_if_equal(&mut self, other: &TokenType) -> Option<TokenType> {
        if &self.peek()? == other {
            self.next()
        } else {
            None
        }
    }
}

impl FromIterator<Token> for TokenBuffer {
    fn from_iter<T: IntoIterator<Item=Token>>(iter: T) -> Self {
        TokenBuffer {
            items: iter.into_iter().collect(),
            current: 0,
        }
    }
}

macro_rules! binary_op {
    ($name:ident : $parent:ident { $( $token:ident => $op:expr ),+ $(,)? } ) => {
        fn $name(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
            let mut left = self.$parent(tokens)?;
            loop {
                let op = match tokens.peek() {
                    $(
                        Some(TokenType::$token) => $op,
                    )+
                    _ => break,
                };
                tokens.next();
                let right = self.$parent(tokens)?;
                left = Expr::Binary {
                    left: Box::new(left),
                    operator: op,
                    right: Box::new(right),
                }
            };
            Ok(left)
        }
    }
}

struct Parser {
    identifier_map: HashMap<Rc<str>, usize>,
}

impl Parser {
    fn new() -> Self {
        Parser {
            identifier_map: HashMap::new(),
        }
    }

    fn consume(&mut self, tokens: &mut TokenBuffer, type_: &TokenType) -> Result<(), ParserError> {
        let next_token = &match tokens.next() {
            Some(t) => t,
            None => return Err(ParserError::UnexpectedEOF),
        };
        if next_token == type_ {
            Ok(())
        } else {
            Err(ParserError::UnexpectedToken(
                tokens.current_token().unwrap().clone(),
            ))
        }
    }

    fn parse_block(&mut self, tokens: &mut TokenBuffer) -> Vec<Stmt> {
        unimplemented!()
    }

    fn parse_stmt(&mut self, tokens: &mut TokenBuffer) -> Result<Stmt, ParserError> {
        unimplemented!()
    }

    fn parse_expression(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        self.parse_logic_or(tokens)
    }

    binary_op! {
        parse_logic_or: parse_logic_and {Or => BinaryOperator::LogicOr}
    }

    binary_op! {
        parse_logic_and: parse_logic_not {And => BinaryOperator::LogicAnd}
    }

    fn parse_logic_not(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        if tokens.next_if_equal(&TokenType::Not).is_some() {
            Ok(Expr::Unary {
                operator: UnaryOperator::LogicNot,
                right: Box::new(self.parse_logic_not(tokens)?),
            })
        } else {
            self.parse_comparison(tokens)
        }
    }

    binary_op! {
        parse_comparison: parse_term {
            Equal => BinaryOperator::Equal,
            NotEqual => BinaryOperator::NotEqual,
            Less => BinaryOperator::Less,
            LessEqual => BinaryOperator::LessEqual,
            Greater => BinaryOperator::Greater,
            GreaterEqual => BinaryOperator::GreaterEqual,
        }
    }

    binary_op! {
        parse_term: parse_factor {
            Plus => BinaryOperator::Plus,
            Minus => BinaryOperator::Minus,
        }
    }

    binary_op! {
        parse_factor: parse_call {
            Star => BinaryOperator::Star,
            Slash => BinaryOperator::Slash,
            Div => BinaryOperator::Div,
            Mod => BinaryOperator::Mod,
        }
    }

    fn parse_call(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        let mut left = self.parse_primary(tokens)?;
        while let Some(TokenType::LParen) = tokens.peek() {
            tokens.next();
            left = Expr::FunctionCall {
                function: Box::new(left),
                args: vec![],
            };
            self.consume(tokens, TokenType::RParen)?;
        }
        Ok(left)
    }

    fn parse_primary(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        let next_token = match tokens.next() {
            Some(t) => t,
            None => return Err(ParserError::UnexpectedEOF),
        };
        let expr = match next_token {
            TokenType::Identifier(ident) => Expr::Identifier {
                handle: self.get_ident_handle(ident),
            },
            TokenType::CharLiteral(c) => Expr::Literal(Literal::Char(c)),
            TokenType::StringLiteral(s) => Expr::Literal(Literal::String(s)),
            TokenType::IntegerLiteral(i) => Expr::Literal(Literal::Integer(i)),
            TokenType::RealLiteral(r) => Expr::Literal(Literal::Real(r)),
            TokenType::BooleanLiteral(b) => Expr::Literal(Literal::Boolean(b)),
            TokenType::LParen => {
                let inner = self.parse_expression(tokens)?;
                self.consume(tokens, &TokenType::RParen)?;
                inner
            }
            _ => {
                return Err(ParserError::UnexpectedToken(
                    tokens.current_token().unwrap().clone(),
                ))
            }
        };
        Ok(expr)
    }

    fn get_ident_handle(&mut self, ident: Rc<str>) -> usize {
        if let Some(&handle) = self.identifier_map.get(&ident) {
            return handle;
        }
        let new_handle = self.identifier_map.len();
        let _ = self.identifier_map.insert(ident, new_handle);
        new_handle
    }
}

pub fn parse_expression(tokens: impl IntoIterator<Item=Token>) -> Result<Expr, ParserError> {
    let mut buf = TokenBuffer::from_iter(tokens);
    let mut parser = Parser::new();
    parser.parse_expression(&mut buf)
}
