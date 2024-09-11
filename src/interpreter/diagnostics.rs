use codespan::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle, Severity};

use crate::{parser::ParserError, scanner::ScannerError};

use super::runtime::{InterpretError, RuntimeError};

macro_rules! make_diagnostic {
    ($message:literal, $span:expr, $($t:tt)*) => {{
        Diagnostic::new(Severity::Error)
            .with_message($message)
            .with_labels(vec![
                Label::new(LabelStyle::Primary, (), $span).with_message(format!($($t)*))
            ])
    }};
}

impl From<ScannerError> for Diagnostic<()> {
    fn from(value: ScannerError) -> Self {
        match value {
            ScannerError::InvalidCharLiteral(span) => make_diagnostic!(
                "Invalid character literal",
                span,
                "Expected a ' to end the char",
            ),
            ScannerError::UnterminatedString(span) => make_diagnostic!(
                "Unterminated string literal",
                span,
                "Another \" is needed to end the string",
            ),
            ScannerError::InvalidRealLiteral(span) => {
                make_diagnostic!("Invalid real literal", span, "Invalid real message")
            }
            ScannerError::UnexpectedCharacter(c, index) => make_diagnostic!(
                "Unexpected character",
                Span::new(index, index),
                "Unexpected character '{c}'"
            ),
        }
    }
}

impl ParserError {
    fn make_diagnostic(self, span: Span) -> Diagnostic<()> {
        match self {
            ParserError::UnexpectedEOF { expected, context } => {
                make_diagnostic!("Unexpected EOF", span, "Expected '{expected}' {context}",)
            }
            ParserError::UnexpectedToken {
                expected,
                actual,
                context,
            } => make_diagnostic!(
                "Unexpected EOF",
                span,
                "Expected '{expected}' {context}, found '{actual}'",
            ),
            ParserError::ExpectedExpression { context } => {
                make_diagnostic!("Expected expression", span, "Expected expression {context}")
            }
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
            InterpretError::Parser(err, span) => err.make_diagnostic(span),
            InterpretError::Runtime(_) => todo!(),
        }
    }
}
