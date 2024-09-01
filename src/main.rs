#![allow(dead_code, unused)]

use std::io;
use std::io::prelude::*;

use interpreter::Interpreter;
use parser::Parser;
use token::TokenType;

mod ast;
mod interpreter;
mod parser;
mod scanner;
mod token;

fn main() {
    let mut i = Interpreter::new();
    let a = i.eval("1 + 1.");
    dbg!(a);
}
