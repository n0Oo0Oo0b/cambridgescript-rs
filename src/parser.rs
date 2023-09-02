use std::iter::Peekable;
use crate::ast::*;
use crate::scanner::Token;

pub enum ParserError {
    UnexpectedToken(Token),
}

pub struct Parser<T: IntoIterator<Item=Token>> {
    tokens: Peekable<T::IntoIter>
}

impl<T: IntoIterator<Item=Token>> From<T> for Parser<T> {
    fn from(value: T) -> Self {
        Parser {
            tokens: value.into_iter().peekable(),
        }
    }
}

impl<T: IntoIterator<Item=Token>> Parser<T> {
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

    fn parse_primary(&mut self) -> Expr {
        todo!()
    }
}
