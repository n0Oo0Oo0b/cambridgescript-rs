use crate::token::{Token, TokenType};
use std::iter::Peekable;
use std::str::{self, Chars};

#[derive(Debug)]
pub enum ScannerError<'a> {
    InvalidCharLiteral(&'a str),
    UnterminatedString(&'a str),
    InvalidRealLiteral(&'a str),
    UnexpectedCharacter(char, &'a str),
}

pub type ScanResult<'a> = Result<Token<'a>, ScannerError<'a>>;

/// Convert
struct Scanner<'src, 'a>
where
    'src: 'a,
{
    source: &'src str,
    iter: Peekable<Chars<'a>>,
    start: usize,
    current: usize,
}

impl<'a, 'src: 'a> Scanner<'src, 'a> {
    fn from_source(source: &'src str) -> Self {
        Self {
            source,
            iter: source.chars().peekable(),
            start: 0,
            current: 0,
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
    fn lexeme_contents(&self) -> &'src str {
        &self.source[self.start..self.current]
    }

    #[inline]
    fn make_token(&self, type_: TokenType) -> Token<'src> {
        Token::with_lexeme(type_, self.lexeme_contents())
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

    fn char(&mut self) -> ScanResult<'src> {
        let c = self
            .advance()
            .ok_or(ScannerError::InvalidCharLiteral("hi"))?;
        self.advance_if_match('\'')
            .then(|| self.make_token(TokenType::CharLiteral(c)))
            .ok_or_else(|| ScannerError::InvalidCharLiteral(self.lexeme_contents()))
    }

    fn string(&mut self) -> ScanResult<'src> {
        self.advance_while(|&c| c != '"' && c != '\n');
        self.advance_if_match('"')
            .then(|| {
                let quoted = self.lexeme_contents();
                let trimmed = &quoted[1..quoted.len() - 1];
                self.make_token(TokenType::StringLiteral(trimmed.into()))
            })
            .ok_or_else(|| ScannerError::UnterminatedString(self.lexeme_contents()))
    }

    fn identifier(&mut self) -> TokenType {
        self.advance_while(char::is_ascii_alphabetic);
        use TokenType as T;
        match self.lexeme_contents() {
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

    fn number(&mut self) -> ScanResult<'src> {
        self.advance_while(char::is_ascii_digit);
        if self.advance_if_match('.') {
            if !self.check_next(char::is_ascii_digit) {
                Err(ScannerError::InvalidRealLiteral(self.lexeme_contents()))
            } else {
                self.advance_while(char::is_ascii_digit);
                let value = self.lexeme_contents().parse().unwrap();
                Ok(self.make_token(TokenType::RealLiteral(value)))
            }
        } else {
            let value = self.lexeme_contents().parse().unwrap();
            Ok(self.make_token(TokenType::IntegerLiteral(value)))
        }
    }

    fn whitespace(&mut self) -> TokenType {
        self.advance_while(|a| a.is_whitespace());
        TokenType::Whitespace
    }

    fn scan_next(&mut self) -> Option<ScanResult<'src>> {
        self.start = self.current;
        let next_char = self.advance()?;

        let token_type = match next_char {
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '[' => TokenType::LBracket,
            ']' => TokenType::RBracket,
            '+' => TokenType::Plus,
            '-' => {
                // TODO: replace with unary negation operator
                if self.check_next(char::is_ascii_digit) {
                    return Some(self.number());
                } else {
                    TokenType::Minus
                }
            }
            '*' => TokenType::Star,
            '/' => {
                if self.advance_if_match('/') {
                    self.comment()
                } else {
                    TokenType::Slash
                }
            }
            '^' => TokenType::Caret,
            '=' => TokenType::Equal,
            '>' => {
                if self.advance_if_match('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                }
            }
            '<' => {
                if self.advance_if_match('=') {
                    TokenType::LessEqual
                } else if self.advance_if_match('>') {
                    TokenType::NotEqual
                } else if self.advance_if_match('-') {
                    TokenType::LArrow
                } else {
                    TokenType::Less
                }
            }
            ',' => TokenType::Comma,
            ':' => TokenType::Colon,
            '\'' => return Some(self.char()),
            '"' => return Some(self.string()),
            c if c.is_ascii_alphabetic() => self.identifier(),
            c if c.is_ascii_digit() => return Some(self.number()),
            c if c.is_ascii_whitespace() => self.whitespace(),
            c => {
                let e = ScannerError::UnexpectedCharacter(c, self.lexeme_contents());
                return Some(Err(e));
            }
        };
        Some(Ok(self.make_token(token_type)))
    }
}

pub struct TokenStream<'s> {
    scanner: Scanner<'s, 's>,
}

impl<'s> Iterator for TokenStream<'s> {
    type Item = ScanResult<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        self.scanner.scan_next()
    }
}

impl<'s> TokenStream<'s> {
    fn new(s: &'s str) -> Self {
        Self {
            scanner: Scanner::from_source(s),
        }
    }
}

pub fn iter_tokens(source: &str) -> TokenStream {
    TokenStream::new(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scan_single_token(source: &str) -> ScanResult {
        let mut scanner = Scanner::from_source(source);
        scanner.scan_next().unwrap()
    }

    fn assert_token_type(source: &str, type_: TokenType) {
        let token = match scan_single_token(source) {
            Ok(Token { type_: t, .. }) => t,
            Err(e) => panic!("Scan failed on {source:?}\nError: {e:?}"),
        };
        assert!(
            token == type_,
            "Expected TokenType {type_:?} from {source:?}, found {token:?}"
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
            Err(ScannerError::InvalidRealLiteral(_))
        ));
        assert!(scan_single_token(".5").is_err());
    }

    #[test]
    fn boolean_literal_token() {
        assert_token_type("TRUE", TokenType::BooleanLiteral(true));
        assert_token_type("FALSE", TokenType::BooleanLiteral(false));
    }
}
