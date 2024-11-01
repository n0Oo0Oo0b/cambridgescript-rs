use codespan::{ByteIndex, Span};

use crate::token::{Token, TokenType};
use std::iter::Peekable;
use std::mem;
use std::str::{self, Chars};

#[derive(Debug, Clone)]
pub enum ScannerError {
    InvalidCharLiteral(Span),
    UnterminatedString(Span),
    InvalidRealLiteral(Span, &'static str),
    UnexpectedCharacter(char, ByteIndex),
}

pub type ScanResult = Result<Token, ScannerError>;

/// Convert
#[derive(Debug)]
pub struct Scanner<'src, 'a>
where
    'src: 'a,
{
    pub source: &'src str,
    iter: Peekable<Chars<'a>>,
    start: u32,
    current: u32,
    pub errors: Vec<ScannerError>,
}

impl<'a, 'src: 'a> Scanner<'src, 'a> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            iter: source.chars().peekable(),
            start: 0,
            current: 0,
            errors: Vec::new(),
        }
    }

    #[inline]
    fn check_next<F>(&mut self, condition: F) -> bool
    where
        F: Fn(&char) -> bool,
    {
        self.iter.peek().is_some_and(condition)
    }

    #[inline]
    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.iter.next()
    }

    #[inline]
    fn current_span(&self) -> Span {
        Span::new(self.start, self.current)
    }

    #[inline]
    fn current_lexeme(&self) -> &str {
        &self.source[self.start as usize..self.current as usize]
    }

    #[inline]
    fn make_token(&self, r#type: TokenType) -> Token {
        Token::with_span(r#type, self.current_span())
    }

    fn advance_if_match(&mut self, target: char) -> bool {
        let result = self.check_next(|&c| c == target);
        if result {
            self.advance();
        }
        result
    }

    fn advance_while<F>(&mut self, condition: F)
    where
        F: Fn(&char) -> bool,
    {
        while self.check_next(&condition) {
            self.advance();
        }
    }

    fn comment(&mut self) -> TokenType {
        self.advance_while(|&c| c != '\n');
        TokenType::Comment
    }

    fn char(&mut self) -> ScanResult {
        let c = self
            .advance()
            .ok_or_else(|| ScannerError::InvalidCharLiteral(self.current_span()))?;
        self.advance_if_match('\'')
            .then(|| self.make_token(TokenType::CharLiteral(c)))
            .ok_or_else(|| ScannerError::InvalidCharLiteral(self.current_span()))
    }

    fn string(&mut self) -> ScanResult {
        self.advance_while(|&c| c != '"' && c != '\n');
        self.advance_if_match('"')
            .then(|| {
                let quoted = self.current_lexeme();
                let trimmed = &quoted[1..quoted.len() - 1];
                self.make_token(TokenType::StringLiteral(trimmed.into()))
            })
            .ok_or_else(|| ScannerError::UnterminatedString(self.current_span()))
    }

    fn identifier(&mut self) -> TokenType {
        self.advance_while(char::is_ascii_alphabetic);
        use TokenType as T;
        match self.current_lexeme() {
            "PROCEDURE" => T::Procedure,
            "ENDPROCEDURE" => T::EndProcedure,
            "FUNCTION" => T::Function,
            "RETURNS" => T::Returns,
            "ENDFUNCTION" => T::EndFunction,
            "RETURN" => T::Return,
            "IF" => T::If,
            "THEN" => T::Then,
            "ELSE" => T::Else,
            "ENDIF" => T::EndIf,
            "CASE" => T::Case,
            "OTHERWISE" => T::Otherwise,
            "ENDCASE" => T::EndCase,
            "FOR" => T::For,
            "TO" => T::To,
            "STEP" => T::Step,
            "NEXT" => T::Next,
            "REPEAT" => T::Repeat,
            "UNTIL" => T::Until,
            "WHILE" => T::While,
            "DO" => T::Do,
            "ENDWHILE" => T::EndWhile,
            "DECLARE" => T::Declare,
            "CONSTANT" => T::Constant,
            "INPUT" => T::Input,
            "OUTPUT" => T::Output,
            "CALL" => T::Call,
            "OPENFILE" => T::OpenFile,
            "READFILE" => T::ReadFile,
            "WRITEFILE" => T::WriteFile,
            "CLOSEFILE" => T::CloseFile,
            "READ" => T::Read,
            "WRITE" => T::Write,
            "INTEGER" => T::Integer,
            "REAL" => T::Real,
            "CHAR" => T::Char,
            "STRING" => T::String,
            "BOOLEAN" => T::Boolean,
            "ARRAY" => T::Array,
            "OF" => T::Of,
            "TRUE" => T::BooleanLiteral(true),
            "FALSE" => T::BooleanLiteral(false),
            "AND" => T::And,
            "OR" => T::Or,
            "NOT" => T::Not,
            ident => T::Identifier(ident.into()),
        }
    }

    fn number(&mut self) -> ScanResult {
        self.advance_while(char::is_ascii_digit);
        if self.advance_if_match('.') {
            if !self.check_next(char::is_ascii_digit) {
                Err(ScannerError::InvalidRealLiteral(
                    self.current_span(),
                    "A decimal point must be followed by at least one digit",
                ))
            } else {
                self.advance_while(char::is_ascii_digit);
                let value = self.current_lexeme().parse().unwrap();
                Ok(self.make_token(TokenType::RealLiteral(value)))
            }
        } else {
            let value = self.current_lexeme().parse().unwrap();
            Ok(self.make_token(TokenType::IntegerLiteral(value)))
        }
    }

    fn whitespace(&mut self) -> TokenType {
        self.advance_while(|a| a.is_whitespace());
        TokenType::Whitespace
    }

    fn scan_next(&mut self) -> Option<ScanResult> {
        self.start = self.current;
        let next_char = self.advance()?;

        #[rustfmt::skip]
        let token_type = match next_char {
            '(' => TokenType::LParen, ')' => TokenType::RParen,
            '[' => TokenType::LBracket, ']' => TokenType::RBracket,
            '+' => TokenType::Plus, '-' => TokenType::Minus,
            '*' => TokenType::Star,
            '/' => {
                if self.advance_if_match('/') { self.comment() }
                else { TokenType::Slash }
            }
            '^' => TokenType::Caret,
            '=' => TokenType::Equal,
            '>' => {
                if self.advance_if_match('=') { TokenType::GreaterEqual }
                else { TokenType::Greater }
            }
            '<' => {
                if self.advance_if_match('=') { TokenType::LessEqual }
                else if self.advance_if_match('>') { TokenType::NotEqual }
                else if self.advance_if_match('-') { TokenType::LArrow }
                else { TokenType::Less }
            }
            ',' => TokenType::Comma,
            ':' => TokenType::Colon,
            '\'' => return Some(self.char()),
            '"' => return Some(self.string()),
            c if c.is_ascii_digit() => return Some(self.number()),
            c if c.is_ascii_alphabetic() => self.identifier(),
            c if c.is_ascii_whitespace() => self.whitespace(),
            // Special case for nicer errors
            '.' if matches!(self.iter.peek(), Some(c) if c.is_ascii_digit()) => {
                return Some(Err(ScannerError::InvalidRealLiteral(
                    self.current_span(),
                    "Real values must start with at least 1 digit",
                )))
            }
            // Something else happened
            other => {
                let e = ScannerError::UnexpectedCharacter(other, ByteIndex(self.current));
                return Some(Err(e));
            }
        };
        Some(Ok(self.make_token(token_type)))
    }

    pub fn take_errors(&mut self) -> Vec<ScannerError> {
        let mut errors = Vec::new();
        mem::swap(&mut self.errors, &mut errors);
        errors
    }
}

impl<'s> Iterator for Scanner<'s, 's> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.scan_next() {
                Some(Ok(token)) => break Some(token),
                None => break None,
                Some(Err(e)) => self.errors.push(e),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scan_single_token(source: &str) -> ScanResult {
        let mut scanner = Scanner::new(source);
        scanner.scan_next().unwrap()
    }

    fn assert_token_type(source: &str, r#type: TokenType) {
        let token = match scan_single_token(source) {
            Ok(Token { inner: t, .. }) => t,
            Err(e) => panic!("Scan failed on {source:?}\nError: {e:?}"),
        };
        assert!(
            token == r#type,
            "Expected TokenType {type:?} from {source:?}, found {token:?}"
        );
    }

    #[test]
    fn keyword_token() {
        assert_token_type("DECLARE", TokenType::Declare);
        assert_token_type("ENDIF", TokenType::EndIf);
    }

    #[test]
    fn symbol_tokens() {
        assert_token_type("(", TokenType::LParen);
        assert_token_type("<", TokenType::Less);
        assert_token_type("<=", TokenType::LessEqual);
        assert_token_type("<>", TokenType::NotEqual);
        assert_token_type("-", TokenType::Minus);
    }

    #[test]
    fn identifier_token() {
        assert_token_type("foo", TokenType::Identifier("foo".into()));
    }

    #[test]
    fn char_literal_token() {
        assert_token_type("'c'", TokenType::CharLiteral('c'));
        assert_token_type(r#"'"'"#, TokenType::CharLiteral('"'));
        assert_token_type(r"'\'", TokenType::CharLiteral('\\'));
    }

    #[test]
    fn invalid_char_literal() {
        assert!(matches!(
            scan_single_token("''"),
            Err(ScannerError::InvalidCharLiteral(_))
        ));
        assert!(matches!(
            scan_single_token("'abc'"),
            Err(ScannerError::InvalidCharLiteral(_))
        ));
    }

    #[test]
    fn string_literal_token() {
        assert_token_type(
            r#""hello world""#,
            TokenType::StringLiteral("hello world".into()),
        );
        assert_token_type(r#""\n\r\b""#, TokenType::StringLiteral(r"\n\r\b".into()));
        assert_token_type(r#""\""#, TokenType::StringLiteral(r"\".into()));
    }

    #[test]
    fn unterminated_string_literal() {
        assert!(matches!(
            scan_single_token(r#""hello"#),
            Err(ScannerError::UnterminatedString(_))
        ))
    }

    #[test]
    fn integer_literal_token() {
        assert_token_type("42", TokenType::IntegerLiteral(42));
        assert_token_type("-5", TokenType::IntegerLiteral(-5));
        assert_token_type("0", TokenType::IntegerLiteral(0));
    }

    #[test]
    fn real_literal_token() {
        assert_token_type("0.6", TokenType::RealLiteral(0.6));
        assert_token_type("13.0", TokenType::RealLiteral(13.0));
        assert_token_type("-2.5", TokenType::RealLiteral(-2.5));
    }

    #[test]
    fn invalid_real_literal() {
        assert!(matches!(
            scan_single_token("2."),
            Err(ScannerError::InvalidRealLiteral(..))
        ));
        assert!(scan_single_token(".5").is_err());
    }

    #[test]
    fn boolean_literal_token() {
        assert_token_type("TRUE", TokenType::BooleanLiteral(true));
        assert_token_type("FALSE", TokenType::BooleanLiteral(false));
    }
}
