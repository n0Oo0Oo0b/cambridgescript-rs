use codespan::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};

use crate::{
    scanner::ScannerError,
    tree_parser::parser::{ParseError, ParseErrorKind},
    tree_parser::MaybeSpanned,
};

use super::runtime::{InterpretError, RuntimeError};

impl From<ScannerError> for Diagnostic<()> {
    fn from(value: ScannerError) -> Diagnostic<()> {
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
    fn from(value: ParseError) -> Diagnostic<()> {
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
            ParseErrorKind::ExpectedType => ("Expected type", "type".to_string()),
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

fn label_or_note(
    maybe_span: Option<Span>,
    style: LabelStyle,
    message: impl Into<String>,
    labels: &mut Vec<Label<()>>,
    notes: &mut Vec<String>,
) {
    match maybe_span {
        Some(span) => labels.push(Label::new(style, (), span).with_message(message)),
        None => notes.push(message.into()),
    }
}

macro_rules! val_and_type {
    ($val:expr) => {{
        let v = $val;
        format!("{} ({} datatype)", v, v.get_type())
    }};
}

impl From<RuntimeError<'_>> for Diagnostic<()> {
    fn from(value: RuntimeError<'_>) -> Diagnostic<()> {
        let mut labels = Vec::new();
        let mut notes = Vec::new();

        let message = match value {
            RuntimeError::UndeclaredVariable { var, required_type } => {
                label_or_note(
                    var.get_span(),
                    LabelStyle::Primary,
                    format!("Variable {} not declared", var.get_name()),
                    &mut labels,
                    &mut notes,
                );
                notes.push(format!(
                    "Consider declaring it with `DECLARE {}: {}`",
                    var.get_name(),
                    required_type.map_or("[datatype]".to_string(), |t| t.to_string())
                ));
                "Undeclared variable"
            }

            RuntimeError::UndefinedVariable { var } => {
                label_or_note(
                    var.get_span(),
                    LabelStyle::Primary,
                    format!("Variable {} doesn't have a value", var.get_name()),
                    &mut labels,
                    &mut notes,
                );
                "Undefined variable"
            }

            RuntimeError::InvalidAssignType {
                target,
                expected_type,
                value,
            } => {
                let msg = format!(
                    "Cannot assign {} to variable {} ({} datatype)",
                    val_and_type!(value),
                    target.get_name(),
                    expected_type,
                );
                label_or_note(
                    target.get_span(),
                    LabelStyle::Primary,
                    msg,
                    &mut labels,
                    &mut notes,
                );
                "Incompatible assignment type"
            }

            RuntimeError::InvalidBinOpTypes { tree, lhs, rhs } => {
                label_or_note(
                    tree.op.span,
                    LabelStyle::Primary,
                    format!("Incompatible types for '{}'", tree.op.inner),
                    &mut labels,
                    &mut notes,
                );
                label_or_note(
                    tree.left.get_span(),
                    LabelStyle::Secondary,
                    format!("Left side evaluates to {}", val_and_type!(lhs)),
                    &mut labels,
                    &mut notes,
                );
                label_or_note(
                    tree.right.get_span(),
                    LabelStyle::Secondary,
                    format!("Right side evaluates to {}", val_and_type!(rhs)),
                    &mut labels,
                    &mut notes,
                );
                "Incompatible types for binary operator"
            }

            RuntimeError::InvalidUnOpType { tree, rhs } => {
                label_or_note(
                    tree.op.span,
                    LabelStyle::Primary,
                    format!("Incompatible type for '{}'", tree.op.inner),
                    &mut labels,
                    &mut notes,
                );
                label_or_note(
                    tree.right.get_span(),
                    LabelStyle::Secondary,
                    format!("Argument evaluates to {}", val_and_type!(rhs)),
                    &mut labels,
                    &mut notes,
                );
                "Incompatible type for unary operator"
            }

            RuntimeError::InvalidBool { tree, value } => {
                label_or_note(
                    tree.get_span(),
                    LabelStyle::Secondary,
                    format!("Evaluates to {}", val_and_type!(value)),
                    &mut labels,
                    &mut notes,
                );
                notes.push("All conditions must be the BOOLEAN datatype".to_string());
                "Invalid type for condition"
            }
        };

        Diagnostic::error()
            .with_message(message)
            .with_labels(labels)
            .with_notes(notes)
    }
}

impl From<InterpretError<'_>> for Diagnostic<()> {
    fn from(value: InterpretError<'_>) -> Diagnostic<()> {
        match value {
            InterpretError::Scanner(e) => e.into(),
            InterpretError::Parser(e) => e.into(),
            InterpretError::Runtime(e) => e.into(),
        }
    }
}
