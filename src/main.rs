#![allow(dead_code)]

use std::io;
use std::io::prelude::*;

use parser::{Parser, TokenTypeExtractor};

mod ast;
mod interpreter;
mod parser;
mod scanner;
mod token;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let s = scanner::iter_tokens(input.as_str()); //.map(|t| dbg!(t));
    let tokens = TokenTypeExtractor::new(s);
    let res = Parser::new(tokens).parse_stmt();
    dbg!(&res);

    // let tokens = vec![
    //     TokenType::Not,
    //     TokenType::IntegerLiteral(1),
    //     TokenType::Equal,
    //     TokenType::IntegerLiteral(2),
    //     TokenType::Star,
    //     TokenType::IntegerLiteral(2),
    // ];
    // let res = Parser::new(tokens.into_iter()).parse_expr();
    // dbg!(&res);

    // let (tokens, errors) = scanner::scan(input.as_str());
    // for token in &tokens {
    //     dbg!(token);
    // }
    // for error in &errors {
    //     dbg!(error);
    // }
    //
    // let block = parser::parse_block(tokens);
    // dbg!(&block);
}
