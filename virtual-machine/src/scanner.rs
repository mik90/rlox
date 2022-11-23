use core::iter::Enumerate;
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

    Error,
    Eof,
}

pub struct Token<'a> {
    pub line: usize,
    pub start: Enumerate<Chars<'a>>,
    pub current: Enumerate<Chars<'a>>,
    pub length: usize,
    pub kind: TokenKind,
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
        self.start = self.current.clone();
        if self.is_at_end()? {
            return Ok(self.make_token(TokenKind::Eof));
        }
        Ok(self.error_token("Unexpected character".to_owned()))
    }

    pub fn is_at_end(&self) -> Result<bool, ScannerError> {
        if let Some((_, c)) = self.current.nth(0) {
            Ok(c == '\0')
        } else {
            Err(ScannerError::UnexpectedEndOfInput(self.line))
        }
    }

    pub fn make_token(&self, kind: TokenKind) -> Token {
        todo!()
    }

    pub fn error_token(&self, description: String) -> Token {
        todo!()
    }
}
