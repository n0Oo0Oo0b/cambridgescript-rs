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

    fn consume(&mut self, type_: &TokenType) -> Result<(), ParserError> {
        let next_token = &match self.peek() {
            Some(t) => t,
            None => return Err(ParserError::UnexpectedEOF),
        };
        if next_token == type_ {
            self.next();
            Ok(())
        } else {
            Err(ParserError::UnexpectedToken(
                self.current_token().unwrap().clone(),
            ))
        }
    }

    fn next_if_equal(&mut self, other: &TokenType) -> Option<TokenType> {
        if &self.peek()? == other {
            self.next()
        } else {
            None
        }
    }

    fn backtrack(&mut self) {
        if self.current > 0 {
            self.current -= 1;
        }
    }
}

impl FromIterator<Token> for TokenBuffer {
    fn from_iter<T: IntoIterator<Item = Token>>(iter: T) -> Self {
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

macro_rules! comma_separated {
    ($getter:expr, $tokens:expr) => {{
        let mut right = Vec::new();
        loop {
            right.push($getter?);
            match $tokens.peek() {
                Some(TokenType::Comma) => {
                    $tokens.next();
                }
                Some(_) => break Ok(right),
                None => break Err(ParserError::UnexpectedEOF),
            }
        }
    }};
    ($getter:expr, $tokens:expr; $end:ident) => {
        'comma_sep: {
            if $tokens.next_if_equal(&TokenType::$end).is_some() {
                break 'comma_sep Ok(Vec::new());
            }
            let mut right = Vec::new();
            loop {
                right.push($getter?);
                match $tokens.next() {
                    Some(TokenType::$end) => {
                        break Ok(right);
                    }
                    Some(TokenType::Comma) => {}
                    Some(_) => {
                        $tokens.backtrack();
                        break Err(ParserError::UnexpectedToken(
                            $tokens.current_token().unwrap().clone(),
                        ));
                    }
                    None => break Err(ParserError::UnexpectedEOF),
                }
            }
        }
    };
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

    fn parse_block(&mut self, tokens: &mut TokenBuffer) -> Block {
        let mut contents = Vec::new();
        while let Ok(stmt) = self.parse_stmt(tokens) {
            contents.push(stmt);
        }
        Block { contents }
    }

    fn parse_stmt(&mut self, tokens: &mut TokenBuffer) -> Result<Stmt, ParserError> {
        let next_token = match tokens.next() {
            Some(t) => t,
            None => return Err(ParserError::UnexpectedEOF),
        };
        let res = match next_token {
            TokenType::Procedure => unimplemented!(),
            TokenType::Function => unimplemented!(),
            TokenType::If => unimplemented!(),
            TokenType::Return => Stmt::Return(self.parse_expression(tokens)?),
            TokenType::Case => unimplemented!(),
            TokenType::For => unimplemented!(),
            TokenType::Repeat => unimplemented!(),
            TokenType::While => unimplemented!(),
            TokenType::Declare => {
                let name = self.parse_primary(tokens)?;
                tokens.consume(&TokenType::Colon)?;
                let type_ = self.parse_type(tokens)?;
                Stmt::VariableDecl { name, type_ }
            }
            TokenType::Constant => unimplemented!(),
            TokenType::Input => {
                Stmt::Input(comma_separated!(self.parse_expression(tokens), tokens)?)
            }
            TokenType::Output => {
                Stmt::Output(comma_separated!(self.parse_expression(tokens), tokens)?)
            }
            TokenType::Call => unimplemented!(),
            TokenType::OpenFile => unimplemented!(),
            TokenType::ReadFile => unimplemented!(),
            TokenType::WriteFile => unimplemented!(),
            TokenType::CloseFile => unimplemented!(),
            TokenType::Read => unimplemented!(),
            TokenType::Write => unimplemented!(),
            _ => {
                tokens.backtrack();
                let target = self.parse_assignable(tokens)?;
                tokens.consume(&TokenType::LArrow)?;
                let value = self.parse_expression(tokens)?;
                Stmt::Assignment { target, value }
            }
        };
        Ok(res)
    }

    fn parse_type(&mut self, tokens: &mut TokenBuffer) -> Result<Type, ParserError> {
        match tokens.next() {
            Some(TokenType::Integer) => Ok(Type::Primitive(PrimitiveType::Integer)),
            Some(TokenType::Real) => Ok(Type::Primitive(PrimitiveType::Real)),
            Some(TokenType::String) => Ok(Type::Primitive(PrimitiveType::String)),
            Some(TokenType::Char) => Ok(Type::Primitive(PrimitiveType::Char)),
            Some(TokenType::Boolean) => Ok(Type::Primitive(PrimitiveType::Boolean)),
            Some(TokenType::Array) => { unimplemented!() }
            Some(_) => {
                tokens.backtrack();
                Err(ParserError::UnexpectedToken(
                    tokens.current_token().unwrap().clone(),
                ))
            }
            None => Err(ParserError::UnexpectedEOF),
        }
    }

    fn parse_expression(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        self.parse_logic_or(tokens)
    }

    fn parse_assignable(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        self.parse_call(tokens)
    }

    binary_op! {
        parse_logic_or: parse_logic_and {Or => BinaryOperator::LogicOr}
    }

    binary_op! {
        parse_logic_and: parse_logic_not {And => BinaryOperator::LogicAnd}
    }

    fn parse_logic_not(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        if tokens.consume(&TokenType::Not).is_ok() {
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
        }
    }

    fn parse_call(&mut self, tokens: &mut TokenBuffer) -> Result<Expr, ParserError> {
        let mut left = self.parse_primary(tokens)?;
        loop {
            left = match tokens.next() {
                Some(TokenType::LParen) => {
                    let right = comma_separated!(self.parse_expression(tokens), tokens; RParen)?;
                    Expr::FunctionCall {
                        function: Box::new(left),
                        args: right,
                    }
                }
                Some(TokenType::LBracket) => {
                    let right = comma_separated!(self.parse_expression(tokens), tokens; RBracket)?;
                    Expr::ArrayIndex {
                        array: Box::new(left),
                        indexes: right,
                    }
                }
                _ => {
                    tokens.backtrack();
                    break
                },
            };
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
                tokens.consume(&TokenType::RParen)?;
                inner
            }
            _ => {
                tokens.backtrack();
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

pub fn parse_expression(tokens: impl IntoIterator<Item = Token>) -> Result<Expr, ParserError> {
    let mut buf = TokenBuffer::from_iter(tokens);
    let mut parser = Parser::new();
    parser.parse_expression(&mut buf)
}

pub fn parse_statement(tokens: impl IntoIterator<Item = Token>) -> Result<Stmt, ParserError> {
    let mut buf = TokenBuffer::from_iter(tokens);
    let mut parser = Parser::new();
    parser.parse_stmt(&mut buf)
}

pub fn parse_block(tokens: impl IntoIterator<Item = Token>) -> Block {
    let mut buf = TokenBuffer::from_iter(tokens);
    let mut parser = Parser::new();
    parser.parse_block(&mut buf)
}
