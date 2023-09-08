use crate::ast::*;
use crate::scanner::{Token, TokenType};
use std::collections::HashMap;
use std::rc::Rc;

pub enum ParserError {
    UnexpectedToken(Token),
    UnexpectedEOF,
}

pub struct TokenBuffer {
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

pub struct Parser {
    identifier_map: HashMap<Rc<str>, usize>,
}

impl Parser {
    fn consume(&mut self, tokens: &mut TokenBuffer, type_: TokenType) -> Result<(), ParserError> {
        let next_token: TokenType = match tokens.next() {
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

    fn parse_logic_or(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    fn parse_logic_and(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    fn parse_logic_not(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    fn parse_comparison(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    fn parse_term(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    binary_op! {
        parse_factor: parse_term {
            Star => BinaryOperator::Star,
            Slash => BinaryOperator::Slash,
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
        let next_token: TokenType = match tokens.next() {
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
                self.consume(tokens, TokenType::RParen)?;
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
