use std::io;
use std::io::prelude::*;

mod scanner;
mod ast;
mod parser;

fn main() {
    let mut input = String::new();
    let _ = io::stdin().read_to_string(&mut input);

    let (tokens, errors) = scanner::scan(input.as_str());
    for token in tokens {
        dbg!(token);
    }
    for error in errors {
        dbg!(error);
    }
}
