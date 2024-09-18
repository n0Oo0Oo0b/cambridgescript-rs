// TODO: remove once everything is implemented
#![allow(unused)]

mod expression;
pub mod parser;
mod spans;
mod statement;
mod types;

macro_rules! enum_derive {
    ($name:ident$(<$gen:ident>)? { $( $variant:ident($wrapped:ty) ),* $(,)? }) => {
        #[derive(Debug)]
        pub enum $name$(<$gen>)? {
            $( $variant($wrapped), )*
        }

        impl MaybeSpanned for $name {
            fn get_span(&self) -> Option<Span> {
                match self {
                    $( Self::$variant(x) => x.get_span(), )*
                }
            }
        }
    };
}
use enum_derive;

pub use expression::*;
pub use spans::*;
pub use statement::*;
pub use types::*;
