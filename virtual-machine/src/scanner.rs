use crate::debugln;
use core::iter::Enumerate;
use std::borrow::BorrowMut;
use std::fmt;
use std::str::Chars;

pub struct Scanner<'a> {
    start: Enumerate<Chars<'a>>,
    current: Enumerate<Chars<'a>>,
    line: usize,
}

#[derive(PartialEq, Debug)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Slash,
    Star,

    // one or two-char tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error(String), // Description of error
    Eof,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub line: usize,
    pub start: Enumerate<Chars<'a>>,
    pub length: usize,
    pub kind: TokenKind,
}

const EMPTY_STR: &str = "";

impl<'a> Token<'_> {
    pub fn new(line: usize, start: Enumerate<Chars<'a>>, length: usize, kind: TokenKind) -> Token {
        Token {
            line,
            start,
            length,
            kind,
        }
    }

    /// Creates an error token which is different in that it doesn't iterator over the source input
    /// and instead is a string whose lifetime it owns on its own
    pub fn new_error(line: usize, description: String) -> Token<'a> {
        Token {
            line,
            // Eugh, this is hacky
            start: EMPTY_STR.chars().enumerate(),
            length: 0,
            kind: TokenKind::Error(description),
        }
    }
}

impl ToString for Token<'_> {
    fn to_string(&self) -> String {
        self.start
            .clone()
            .map(|(_, c)| c)
            .take(self.length)
            .collect::<String>()
    }
}

#[derive(Debug, Clone)]
pub enum ScannerError {
    UnexpectedEndOfInput(usize), // Cur line
}
impl std::error::Error for ScannerError {}

impl fmt::Display for ScannerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ScannerError::UnexpectedEndOfInput(line) => {
                write!(f, "Unexpected end of input on line {}", line)
            }
        }
    }
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            start: source.chars().into_iter().enumerate(),
            current: source.chars().into_iter().enumerate(),
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Result<Token, ScannerError> {
        self.skip_whitespace()?;
        self.start = self.current.clone();
        if self.is_at_end()? {
            return self.make_token(TokenKind::Eof);
        }
        let (_, c) = self
            .current
            .next()
            .ok_or(ScannerError::UnexpectedEndOfInput(self.line))?;

        if c.is_ascii_digit() {
            return self.make_number_token();
        }

        // TODO dedup
        match c {
            // Single char tokens
            '(' => self.make_token(TokenKind::LeftParen),
            ')' => self.make_token(TokenKind::RightParen),
            '{' => self.make_token(TokenKind::LeftBrace),
            '}' => self.make_token(TokenKind::RightBrace),
            ';' => self.make_token(TokenKind::SemiColon),
            ',' => self.make_token(TokenKind::Comma),
            '.' => self.make_token(TokenKind::Dot),
            '-' => self.make_token(TokenKind::Minus),
            '+' => self.make_token(TokenKind::Plus),
            '/' => self.make_token(TokenKind::Slash),
            '*' => self.make_token(TokenKind::Star),
            // Double char tokens
            '!' => {
                let next_char_matches = self.consume_char_if_eq('=')?;
                self.make_token(if next_char_matches {
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                })
            }
            '=' => {
                let next_char_matches = self.consume_char_if_eq('=')?;
                self.make_token(if next_char_matches {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                })
            }
            '<' => {
                let next_char_matches = self.consume_char_if_eq('=')?;
                self.make_token(if next_char_matches {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                })
            }
            '>' => {
                let next_char_matches = self.consume_char_if_eq('=')?;
                self.make_token(if next_char_matches {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                })
            }
            '"' => self.make_string_token(),
            // Unknown
            _ => Ok(self.error_token(format!("Unexpected character: {}", c))),
        }
    }

    fn peek_current_char(&self) -> Result<char, ScannerError> {
        self.current
            .clone()
            .nth(0)
            .map(|(_, c)| c)
            .ok_or(ScannerError::UnexpectedEndOfInput(self.line))
    }

    fn peek_next_char(&self) -> Result<char, ScannerError> {
        self.current
            .clone()
            .nth(1)
            .map(|(_, c)| c)
            .ok_or(ScannerError::UnexpectedEndOfInput(self.line))
    }

    fn skip_whitespace(&mut self) -> Result<(), ScannerError> {
        loop {
            match self.peek_current_char()? {
                ' ' | '\t' | '\r' => {
                    self.current.next();
                }
                '\n' => {
                    self.line = self.line + 1;
                    self.current.next();
                }
                '/' => {
                    if self.peek_next_char()? == '/' {
                        // Consume comment
                        while self.peek_current_char()? != '\n' && !self.is_at_end()? {
                            self.current.next();
                        }
                    } else {
                        // This is a non-whitespace character, we're done skipping
                        return Ok(());
                    }
                }
                _ => break,
            }
        }
        Ok(())
    }

    fn consume_char_if_eq(&mut self, expected: char) -> Result<bool, ScannerError> {
        if self.is_at_end()? {
            return Ok(false);
        }
        let matches = self
            .current
            .clone()
            .peekable()
            .peek()
            .map(|(_, c)| *c == expected)
            .ok_or(ScannerError::UnexpectedEndOfInput(self.line))?;
        if matches {
            self.current.next();
        };
        Ok(matches)
    }

    /// Returns whether or not we're at the end of the file
    pub fn is_at_end(&self) -> Result<bool, ScannerError> {
        if let Some((_, c)) = self.current.clone().peekable().peek() {
            Ok(*c == '\0')
        } else {
            Err(ScannerError::UnexpectedEndOfInput(self.line))
        }
    }

    fn length_of_current_token(&self) -> Result<usize, ScannerError> {
        let (start_idx, _) = self
            .start
            .clone()
            .nth(0)
            .ok_or_else(|| ScannerError::UnexpectedEndOfInput(self.line))?;
        let (current_idx, _) = self
            .current
            .clone()
            .nth(0)
            .ok_or_else(|| ScannerError::UnexpectedEndOfInput(self.line))?;
        Ok(current_idx - start_idx)
    }

    /// Token builders

    fn make_token(&self, kind: TokenKind) -> Result<Token, ScannerError> {
        Ok(Token::new(
            self.line,
            self.start.clone(),
            self.length_of_current_token()?,
            kind,
        ))
    }

    fn error_token(&self, description: String) -> Token {
        Token::new_error(self.line, description)
    }

    fn make_string_token(&mut self) -> Result<Token, ScannerError> {
        while self.peek_current_char()? != '"' && self.is_at_end()? {
            if self.peek_current_char()? == '\n' {
                self.line = self.line + 1;
            }
            self.current.next();
        }
        if self.is_at_end()? {
            return Ok(self.error_token(format!("Unterminated string on line {}", self.line)));
        }
        // Consume closing quote
        self.current.next();

        self.make_token(TokenKind::String)
    }

    fn make_number_token(&mut self) -> Result<Token, ScannerError> {
        while self.peek_current_char()?.is_ascii_digit() {
            self.current.next();
            debugln!(
                "Consumed char. Current token len={}",
                self.length_of_current_token()?
            );
        }

        // Check for fractions
        if self.peek_current_char()? == '.' && self.peek_next_char()?.is_ascii_digit() {
            // consume dot
            self.current.next();

            while self.peek_current_char()?.is_ascii_digit() {
                self.current.next();
                debugln!(
                    "Consumed post-decimal digit. Current token len={}",
                    self.length_of_current_token()?
                );
            }
        }

        self.make_token(TokenKind::Number)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn scan_until_end() {
        let mut scanner = Scanner::new("(!=}\0");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::LeftParen, "Expected (");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::BangEqual, "Expected !=");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::RightBrace, "Expected }}");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::Eof, "Expected Eof");
        let is_at_end = scanner.is_at_end();
        assert!(is_at_end.is_ok(), "{}", is_at_end.unwrap_err());
        assert!(is_at_end.unwrap());
    }

    #[test]
    fn skip_whitespace() {
        let mut scanner = Scanner::new("(\n ) \0");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::LeftParen, "Expected (");
        assert_eq!(token.line, 1);

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::RightParen, "Expected )");
        assert_eq!(token.line, 2);

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::Eof, "Expected Eof");
    }

    #[test]
    fn start_with_whitespace() {
        let mut scanner = Scanner::new(" (\0");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::LeftParen, "Expected (");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::Eof, "Expected Eof");

        let is_at_end = scanner.is_at_end();
        assert!(is_at_end.is_ok(), "{}", is_at_end.unwrap_err());
        assert!(is_at_end.unwrap());
    }

    #[test]
    fn scan_integral() {
        let mut scanner = Scanner::new("123\0");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::Number, "Expected number");
        assert_eq!(token.to_string(), "123");
    }

    #[test]
    fn scan_float() {
        let mut scanner = Scanner::new("42.123\0");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::Number, "Expected number");
        assert_eq!(token.to_string(), "42.123", "{:?}", token);
    }
}
