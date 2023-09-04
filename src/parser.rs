use std::collections::HashMap;
use std::iter::Peekable;
use crate::ast::*;
use crate::scanner::{Token,TokenType};

pub enum ParserError {
    UnexpectedToken(Token),
    UnexpectedEOF,
}

pub struct Parser<T: IntoIterator<Item=Token>> {
    tokens: Peekable<T::IntoIter>,
    identifier_map: HashMap<Box<str>, usize>
}

impl<T: IntoIterator<Item=Token>> From<T> for Parser<T> {
    fn from(value: T) -> Self {
        Parser {
            tokens: value.into_iter().peekable(),
            identifier_map: HashMap::new(),
        }
    }
}

impl<T: IntoIterator<Item=Token>> Parser<T> {
    fn consume(&mut self, type_: TokenType) -> Result<(), ParserError> {
        let next_token: Token = match self.tokens.next() {
            Some(t) => t,
            None => return Err(ParserError::UnexpectedEOF),
        };
        if next_token.type_ == type_ {
            Ok(())
        } else {
            Err(ParserError::UnexpectedToken(next_token))
        }
    }

    fn parse_block(&mut self) -> Vec<Stmt> {
        unimplemented!()
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParserError> {
        unimplemented!()
    }

    fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        self.parse_logic_or()
    }

    fn parse_logic_or(&mut self) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    fn parse_logic_and(&mut self) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    fn parse_logic_not(&mut self) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    fn parse_call(&mut self) -> Result<Expr, ParserError> {
        unimplemented!()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        let next_token: Token = match self.tokens.next() {
            Some(t) => t,
            None => return Err(ParserError::UnexpectedEOF)
        };
        let expr = match next_token.type_ {
            TokenType::Identifier(ident) =>
                Expr::Identifier { handle: self.get_ident_handle(ident) },
            TokenType::CharLiteral(c) => Expr::Literal(Literal::Char(c)),
            TokenType::StringLiteral(s) => Expr::Literal(Literal::String(s)),
            TokenType::IntegerLiteral(i) => Expr::Literal(Literal::Integer(i)),
            TokenType::RealLiteral(r) => Expr::Literal(Literal::Real(r)),
            TokenType::BooleanLiteral(b) => Expr::Literal(Literal::Boolean(b)),
            TokenType::LParen => {
                let inner = self.parse_expression()?;
                self.consume(TokenType::RParen)?;
                inner
            },
            _ => return Err(ParserError::UnexpectedToken(next_token)),
        };
        Ok(expr)
    }

    fn get_ident_handle(&mut self, ident: Box<str>) -> usize {
        if let Some(&handle) = self.identifier_map.get(&ident) {
            return handle
        }
        let new_handle = self.identifier_map.len();
        let _ = self.identifier_map.insert(ident, new_handle);
        new_handle
    }
}
