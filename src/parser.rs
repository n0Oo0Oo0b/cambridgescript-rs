use std::iter::Peekable;
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
