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
        todo!()
    }

    fn parse_stmt(&mut self) -> Vec<Stmt> {
        todo!()
    }

    fn parse_expression(&mut self) -> Expr {
        self.parse_logic_or()
    }

    fn parse_logic_or(&mut self) -> Expr {
        todo!()
    }

    fn parse_logic_and(&mut self) -> Expr {
        todo!()
    }

    fn parse_logic_not(&mut self) -> Expr {
        todo!()
    }

    fn parse_comparison(&mut self) -> Expr {
        todo!()
    }

    fn parse_term(&mut self) -> Expr {
        todo!()
    }

    fn parse_factor(&mut self) -> Expr {
        todo!()
    }

    fn parse_call(&mut self) -> Expr {
        todo!()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
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
