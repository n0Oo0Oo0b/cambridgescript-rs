use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    scanner::ScannerError,
    tree_parser::parser::{ParseError, ParseErrorKind},
    tree_parser::MaybeSpanned,
};

use super::runtime::{InterpretError, RuntimeError};

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
                    .with_message("Another `\"` is needed to close the string")]),
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

impl From<ParseError> for Diagnostic<()> {
    fn from(value: ParseError) -> Self {
        let ParseError {
            context,
            location,
            kind,
        } = value;
        let (message, expected) = match kind {
            ParseErrorKind::UnexpectedToken(expected) => {
                ("Unexpected token or EOF", expected.to_string())
            }
            ParseErrorKind::ExpectedExpression => ("Expected expression", "expression".to_string()),
            ParseErrorKind::ExpectedStatement => ("Expected statement", "statement".to_string()),
            ParseErrorKind::ExpectedType => ("Expected statement", "statement".to_string()),
        };

        let mut labels = vec![Label::primary((), location.get_span().unwrap())
            .with_message(format!("Expected {expected}, found {location}"))];
        if let Some(span) = context.1 {
            labels.push(Label::secondary((), span).with_message(context.0));
        }
        Diagnostic::error()
            .with_message(message)
            .with_labels(labels)
    }
}

impl From<RuntimeError> for Diagnostic<()> {
    fn from(value: RuntimeError) -> Self {
        match value {
            RuntimeError::InvalidAssignmentType(_, _) => todo!("Invalid assignment type"),
            RuntimeError::UndeclaredVariable(_) => todo!("Undeclared ident"),
            RuntimeError::UndefinedVariable(_) => todo!("Undefined ident"),
            RuntimeError::IncompatibleTypes(_, _) => todo!("Incompatible type"),
            RuntimeError::InvalidBool(_) => todo!("Implicit cast to bool"),
        }
    }
}

impl From<InterpretError> for Diagnostic<()> {
    fn from(value: InterpretError) -> Self {
        match value {
            InterpretError::Scanner(e) => e.into(),
            InterpretError::Parser(e) => e.into(),
            InterpretError::Runtime(e) => e.into(),
        }
    }
}
