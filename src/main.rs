use std::io;
use std::io::prelude::*;

mod scanner;

fn main() {
    let mut input = String::new();
    let _ = io::stdin().read_to_string(&mut input);

    for token in scanner::iter_tokens(input.as_str()) {
        dbg!(&token);
    }
}
