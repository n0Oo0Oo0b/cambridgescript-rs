use std::iter;
use std::str;

#[rustfmt::skip]
#[derive(Debug)]
pub enum TokenType {
    // Reserved words

    Procedure, EndProcedure,
    Function, Returns, EndFunction, Return,

    If, Then, Else,
    Case, Otherwise, EndCase,
    For, To, Step, Next,
    Repeat, Until,
    While, Do, EndWhile,

    Declare, Constant,
    Input, Output, Call,

    OpenFile, ReadFile, WriteFile, CloseFile,
    Read, Write,

    Integer, Real, Char, String, Boolean,
    Array, Of,

    And, Or, Not,

    // Symbols

    LParen, RParen, LBracket, RBracket,
    Plus, Minus, Star, Slash, Caret,
    Equal, NotEqual, LessEqual, GreaterEqual, Less, Greater,
    Comma, Colon, LArrow,

    // Others

    Identifier(String),

    CharLiteral(char),
    StringLiteral(String),
    IntegerLiteral(i64),
    RealLiteral(f64),
    BooleanLiteral(bool),

    Whitespace, Comment,
}

#[derive(Copy, Clone, Debug)]
pub struct Location {
    line: usize,
    column: usize,
}

impl Location {
    fn new() -> Self {
        Self { line: 1, column: 1 }
    }

    fn increment_column(&mut self) {
        self.column += 1;
    }

    fn increment_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }
}

#[derive(Debug)]
pub enum ScannerError {
    UnterminatedString(Location),
    UnexpectedCharacter(char, Location),
}

#[derive(Debug)]
pub struct Token {
    pub type_: TokenType,
    pub lexeme: String,
    pub location: Location,
}

struct Scanner<'a> {
    source: iter::Peekable<str::Chars<'a>>,
    cur_lexeme: String,
    cur_location: Location,
}

impl<'a> Scanner<'a> {
    fn from_source(source: &'a str) -> Self {
        Self {
            source: source.chars().peekable(),
            cur_lexeme: String::new(),
            cur_location: Location::new(),
        }
    }

    fn check_next(&mut self, condition: &dyn Fn(&char) -> bool) -> bool {
        match self.source.peek() {
            Some(&c) => condition(&c),
            None => false,
        }
    }

    fn advance(&mut self) -> Option<char> {
        let next = self.source.next();
        if let Some(c) = next {
            self.cur_lexeme.push(c);
            if c == '\n' {
                self.cur_location.increment_line();
            } else {
                self.cur_location.increment_column();
            }
        };
        next
    }

    fn advance_if_match(&mut self, target: char) -> bool {
        let result = self.check_next(&|&c| c == target);
        if result {
            self.advance();
        };
        result
    }

    fn advance_while(&mut self, condition: &dyn Fn(&char) -> bool) {
        while self.check_next(condition) {
            self.advance();
        }
    }

    fn create_token(&mut self, type_: TokenType, location: Location) -> Token {
        Token {
            type_,
            lexeme: self.cur_lexeme.clone(),
            location,
        }
    }

    fn comment(&mut self) -> TokenType {
        self.advance_while(&|&c| c != '\n');
        TokenType::Comment
    }

    fn char(&mut self) -> Result<TokenType, ScannerError> {
        let c = match self.advance() {
            Some(c) => c,
            None => return Err(ScannerError::UnterminatedString(self.cur_location)),
        };
        if !self.advance_if_match('\'') {
            return Err(ScannerError::UnterminatedString(self.cur_location));
        }
        Ok(TokenType::CharLiteral(c))
    }

    fn string(&mut self) -> Result<TokenType, ScannerError> {
        self.advance_while(&|&c| c != '"' && c != '\n');
        if !self.advance_if_match('"') {
            return Err(ScannerError::UnterminatedString(self.cur_location));
        };
        let content = self.cur_lexeme[1..self.cur_lexeme.len() - 1].to_string();
        Ok(TokenType::StringLiteral(content))
    }

    fn identifier(&mut self) -> TokenType {
        self.advance_while(&char::is_ascii_alphabetic);
        match self.cur_lexeme.as_str() {
            "PROCEDURE" => TokenType::Procedure,
            "ENDPROCEDURE" => TokenType::EndProcedure,
            "FUNCTION" => TokenType::Function,
            "RETURNS" => TokenType::Returns,
            "ENDFUNCTION" => TokenType::EndFunction,
            "RETURN" => TokenType::Return,
            "IF" => TokenType::If,
            "THEN" => TokenType::Then,
            "ELSE" => TokenType::Else,
            "CASE" => TokenType::Case,
            "OTHERWISE" => TokenType::Otherwise,
            "ENDCASE" => TokenType::EndCase,
            "FOR" => TokenType::For,
            "TO" => TokenType::To,
            "STEP" => TokenType::Step,
            "NEXT" => TokenType::Next,
            "REPEAT" => TokenType::Repeat,
            "UNTIL" => TokenType::Until,
            "WHILE" => TokenType::While,
            "DO" => TokenType::Do,
            "ENDWHILE" => TokenType::EndWhile,
            "DECLARE" => TokenType::Declare,
            "CONSTANT" => TokenType::Constant,
            "INPUT" => TokenType::Input,
            "OUTPUT" => TokenType::Output,
            "CALL" => TokenType::Call,
            "OPENFILE" => TokenType::OpenFile,
            "READFILE" => TokenType::ReadFile,
            "WRITEFILE" => TokenType::WriteFile,
            "CLOSEFILE" => TokenType::CloseFile,
            "READ" => TokenType::Read,
            "WRITE" => TokenType::Write,
            "INTEGER" => TokenType::Integer,
            "REAL" => TokenType::Real,
            "CHAR" => TokenType::Char,
            "STRING" => TokenType::String,
            "BOOLEAN" => TokenType::Boolean,
            "ARRAY" => TokenType::Array,
            "OF" => TokenType::Of,
            "TRUE" => TokenType::BooleanLiteral(true),
            "FALSE" => TokenType::BooleanLiteral(false),
            "AND" => TokenType::And,
            "OR" => TokenType::Or,
            "NOT" => TokenType::Not,
            identifier => TokenType::Identifier(identifier.into()),
        }
    }

    fn number(&mut self) -> Result<TokenType, ScannerError> {
        self.advance_while(&char::is_ascii_digit);
        if self.advance_if_match('.') {
            self.advance_while(&char::is_ascii_digit);
            Ok(TokenType::RealLiteral(0.0))
        } else {
            Ok(TokenType::IntegerLiteral(self.cur_lexeme.parse().unwrap()))
        }
    }

    fn whitespace(&mut self) -> TokenType {
        self.advance_while(&char::is_ascii_whitespace);
        TokenType::Whitespace
    }

    fn scan_next(&mut self) -> Option<Result<Token, ScannerError>> {
        let location = self.cur_location;
        self.cur_lexeme.clear();

        let next_char = match self.advance() {
            Some(c) => c,
            None => return None, // Already at end
        };
        let result = match next_char {
            '(' => Ok(TokenType::LParen),
            ')' => Ok(TokenType::RParen),
            '[' => Ok(TokenType::LBracket),
            ']' => Ok(TokenType::RBracket),
            '+' => Ok(TokenType::Plus),
            '-' => {
                if self.check_next(&char::is_ascii_digit) {
                    self.number()
                } else {
                    Ok(TokenType::Minus)
                }
            },
            '*' => Ok(TokenType::Star),
            '/' => {
                if self.advance_if_match('/') {
                    Ok(self.comment())
                } else {
                    Ok(TokenType::Slash)
                }
            }
            '^' => Ok(TokenType::Caret),
            '=' => Ok(TokenType::Equal),
            '>' => {
                if self.advance_if_match('=') {
                    Ok(TokenType::GreaterEqual)
                } else {
                    Ok(TokenType::Greater)
                }
            }
            '<' => {
                if self.advance_if_match('=') {
                    Ok(TokenType::LessEqual)
                } else if self.advance_if_match('>') {
                    Ok(TokenType::NotEqual)
                } else if self.advance_if_match('-') {
                    Ok(TokenType::LArrow)
                } else {
                    Ok(TokenType::Less)
                }
            }
            ',' => Ok(TokenType::Comma),
            ':' => Ok(TokenType::Colon),
            '\'' => self.char(),
            '"' => self.string(),
            c if c.is_ascii_alphabetic() => Ok(self.identifier()),
            c if c.is_ascii_digit() => self.number(),
            c if c.is_ascii_whitespace() => Ok(self.whitespace()),
            c => Err(ScannerError::UnexpectedCharacter(c, self.cur_location)),
        };
        Some(result.map(|t| self.create_token(t, location)))
    }
}

pub struct ScannerIterator<'a> {
    scanner: Scanner<'a>,
    ignore_irrelevant: bool,
}

impl<'a> Iterator for ScannerIterator<'a> {
    type Item = Result<Token, ScannerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut token = self.scanner.scan_next();
        if self.ignore_irrelevant {
            // Skip whitespace and comment tokens
            while let Some(Ok(Token {
                type_: TokenType::Comment | TokenType::Whitespace,
                ..
            })) = token
            {
                token = self.scanner.scan_next();
            }
        };
        token
    }
}

pub fn iter_tokens(source: &str) -> ScannerIterator {
    ScannerIterator {
        scanner: Scanner::from_source(source),
        ignore_irrelevant: true,
    }
}

pub fn scan(source: &str) -> (Vec<Token>, Vec<ScannerError>) {
    let mut tokens: Vec<Token> = Vec::new();
    let mut errors: Vec<ScannerError> = Vec::new();
    for item in iter_tokens(source) {
        match item {
            Ok(token) => tokens.push(token),
            Err(error) => errors.push(error),
        }
    }
    (tokens, errors)
}
