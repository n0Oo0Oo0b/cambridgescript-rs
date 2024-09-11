use std::ops::Range;

use codespan::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{parser::ParseError, scanner::ScannerError, token::Token};

use super::runtime::{InterpretError, RuntimeError};

#[inline(always)]
fn range_after(span: Span) -> Range<usize> {
    let end = span.end().to_usize();
    end..end
}

impl From<ScannerError> for Diagnostic<()> {
    fn from(value: ScannerError) -> Self {
        match value {
            ScannerError::InvalidCharLiteral(span) => Diagnostic::error()
                .with_message("Invalid character")
                .with_labels(vec![
                    Label::primary((), span).with_message("Expected a ' to end the char")
                ]),
            ScannerError::UnterminatedString(span) => Diagnostic::error()
                .with_message("Unterminated string")
                .with_labels(vec![Label::primary((), span)
                    .with_message("Another `\"` was expected to close the string")]),
            ScannerError::InvalidRealLiteral(span, message) => Diagnostic::error()
                .with_message("Invalid real value")
                .with_labels(vec![Label::primary((), span).with_message(message)]),
            ScannerError::UnexpectedCharacter(c, index) => Diagnostic::error()
                .with_message("Unexpected character")
                .with_labels(vec![Label::primary((), index.to_usize()..index.to_usize())
                    .with_message(format!("Unexpected character '{c}'"))]),
        }
    }
}

fn error_labels(
    expected: &str,
    token: Option<Token>,
    context: (&'static str, Option<Span>),
) -> Vec<Label<()>> {
    let (ctx_msg, ctx_span) = (context.0, context.1.unwrap());
    let (found_span, found_name) = match &token {
        Some(t) => (t.span.unwrap().into(), t.type_.to_string()),
        None => (range_after(ctx_span), "nothing".into()),
    };
    vec![
        Label::primary((), found_span)
            .with_message(format!("Expected {expected}, found {found_name}")),
        Label::secondary((), ctx_span).with_message(format!("Required {}", ctx_msg)),
    ]
}

impl From<ParseError> for Diagnostic<()> {
    fn from(value: ParseError) -> Self {
        match value {
            ParseError::UnexpectedToken {
                expected,
                actual,
                context,
            } => Diagnostic::error()
                .with_message(
                    #[rustfmt::skip]
                        if actual.is_some() { "Unexpected token" }
                        else { "Unexpected EOF (end of file)" },
                )
                .with_labels(error_labels(&expected.to_string(), actual, context)),
            ParseError::ExpectedExpression { context, actual } => Diagnostic::error()
                .with_message("Unexpected expression")
                .with_labels(error_labels("expression", actual, context)),
            _ => todo!(),
        }
    }
}

impl From<RuntimeError> for Diagnostic<()> {
    fn from(value: RuntimeError) -> Self {
        match value {
            RuntimeError::UndeclaredVariable(_) => todo!(),
            RuntimeError::UndefinedVariable(_) => todo!(),
            RuntimeError::IncompatibleTypes(_, _) => todo!(),
            RuntimeError::InvalidBool(_) => todo!(),
        }
    }
}

impl From<InterpretError> for Diagnostic<()> {
    fn from(value: InterpretError) -> Self {
        match value {
            InterpretError::Scanner(err) => err.into(),
            InterpretError::Parser(err) => err.into(),
            InterpretError::Runtime(_) => todo!(),
        }
    }
}
