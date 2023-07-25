use std::io;
use std::io::prelude::*;

mod scanner;

fn main() {
    let mut input = String::new();
    let _ = io::stdin().read_to_string(&mut input);

    let scanner = scanner::Scanner::from_source(input.as_str());
    for token in scanner.iter_tokens(false) {
        println!("{token:?}");
    }
}
