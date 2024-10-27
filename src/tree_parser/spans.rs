use super::{expr, stmt};
use codespan::{ByteIndex, Span};
use std::fmt::Debug;

#[inline]
pub(super) fn join_span(a: Option<Span>, b: Option<Span>) -> Option<Span> {
    match (a, b) {
        (Some(a), Some(b)) => Some(a.merge(b)),
        _ => None,
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T: Debug + Clone> {
    pub inner: T,
    pub span: Option<Span>,
}

impl<T: Debug + Clone> Spanned<T> {
    pub fn new(item: T, span: Option<Span>) -> Self {
        Self { inner: item, span }
    }

    pub fn with_span(item: T, span: Span) -> Self {
        Self::new(item, Some(span))
    }

    pub fn without_span(item: T) -> Self {
        Self::new(item, None)
    }
}

impl<T: Debug + Clone> From<T> for Spanned<T> {
    fn from(value: T) -> Self {
        Self::without_span(value)
    }
}

impl<T: Debug + Clone> MaybeSpanned for Spanned<T> {
    fn get_span(&self) -> Option<Span> {
        self.span
    }
}

pub trait MaybeSpanned {
    fn get_span(&self) -> Option<Span>;

    fn get_span_start(&self) -> Option<ByteIndex> {
        self.get_span().map(|s| s.start())
    }

    fn get_span_end(&self) -> Option<ByteIndex> {
        self.get_span().map(|s| s.end())
    }
}

impl MaybeSpanned for Option<Span> {
    fn get_span(&self) -> Option<Span> {
        *self
    }
}

macro_rules! impl_spanned {
    ($struct:path => $($t:tt)+) => {
        impl MaybeSpanned for $struct {
            impl_spanned!(@inner $($t)+);
        }
    };
    (@inner $a:ident) => {
        fn get_span(&self) -> Option<Span> {
            self.$a.get_span()
        }
    };
    (@inner $a:ident + $b:ident) => {
        fn get_span(&self) -> Option<Span> {
            join_span(self.$a.get_span(), self.$b.get_span())
        }
        fn get_span_start(&self) -> Option<ByteIndex> {
            self.$a.get_span_start()
        }
        fn get_span_end(&self) -> Option<ByteIndex> {
            self.$b.get_span_end()
        }
    };
}

impl_spanned!(expr::BinaryExpr => left + right);
impl_spanned!(expr::UnaryExpr => op + right);
impl_spanned!(expr::Identifier => span);
impl_spanned!(expr::Literal => span);

impl_spanned!(stmt::AssignStmt => target + value);
