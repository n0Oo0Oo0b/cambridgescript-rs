#![allow(dead_code)]

use std::io;
use std::io::prelude::*;

// mod ast;
// mod parser;
mod scanner;
mod token;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let s = scanner::iter_tokens(input.as_str());
    for t in s {
        let _ = dbg!(t);
    }

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
