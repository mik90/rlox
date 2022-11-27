use crate::debugln;
use core::iter::Enumerate;
use std::fmt::{self, format};
use std::str::Chars;

pub struct Scanner<'a> {
    start: Enumerate<Chars<'a>>,
    current: Enumerate<Chars<'a>>,
    line: usize,
}

#[derive(PartialEq, Eq, Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub line: usize,
    pub start: Enumerate<Chars<'a>>,
    pub length: usize,
    pub kind: TokenKind,
}

const EMPTY_STR: &str = "";

impl<'a> Token<'_> {
    pub fn new_uninit() -> Token<'a> {
        Token {
            line: 0,
            start: EMPTY_STR.chars().enumerate(),
            length: 0,
            kind: TokenKind::Error(String::from("Uninitialized token")),
        }
    }

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

    pub fn scan_token(&mut self) -> Result<Token<'a>, ScannerError> {
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
        } else if c.is_alphabetic() || c == '_' {
            return self.make_identifier_token();
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

    fn peek_char_after_start(&self) -> Result<char, ScannerError> {
        self.start
            .clone()
            .nth(1)
            .map(|(_, c)| c)
            .ok_or(ScannerError::UnexpectedEndOfInput(self.line))
    }

    fn peek_starting_char(&self) -> Result<char, ScannerError> {
        self.start
            .clone()
            .nth(0)
            .map(|(_, c)| c)
            .ok_or(ScannerError::UnexpectedEndOfInput(self.line))
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

    fn string_from_start_idx(
        &self,
        start_idx: usize,
        length: usize,
    ) -> Result<String, ScannerError> {
        let token_str = self
            .start
            .clone()
            .skip(start_idx)
            .take(length)
            .map(|(_, c)| c)
            .collect::<String>();
        Ok(token_str)
    }

    // TODO can i have a &str that is made from Enumerate<Char>?
    fn current_token_as_string(&self) -> Result<String, ScannerError> {
        let (current_idx, _) = self
            .current
            .clone()
            .nth(0)
            .ok_or_else(|| ScannerError::UnexpectedEndOfInput(self.line))?;
        let token_str = self
            .start
            .clone()
            .take_while(|(idx, _)| *idx < current_idx)
            .map(|(_, c)| c)
            .collect::<String>();
        Ok(token_str)
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

    fn make_token(&self, kind: TokenKind) -> Result<Token<'a>, ScannerError> {
        Ok(Token::new(
            self.line,
            self.start.clone(),
            self.length_of_current_token()?,
            kind,
        ))
    }

    fn error_token(&self, description: String) -> Token<'a> {
        Token::new_error(self.line, description)
    }

    fn make_string_token(&mut self) -> Result<Token<'a>, ScannerError> {
        while self.peek_current_char()? != '"' && !self.is_at_end()? {
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

    fn make_number_token(&mut self) -> Result<Token<'a>, ScannerError> {
        while self.peek_current_char()?.is_ascii_digit() {
            self.current.next();
        }

        // Check for fractions
        if self.peek_current_char()? == '.' && self.peek_next_char()?.is_ascii_digit() {
            // consume dot
            self.current.next();

            while self.peek_current_char()?.is_ascii_digit() {
                self.current.next();
            }
        }

        self.make_token(TokenKind::Number)
    }

    fn make_identifier_token(&mut self) -> Result<Token<'a>, ScannerError> {
        while self.peek_current_char()?.is_alphanumeric() || self.peek_current_char()? == '_' {
            self.current.next();
        }
        let token_kind = self.identifier_type()?;
        self.make_token(token_kind)
    }

    // Returns the TokenKind for the proposed keyword or a general identifier TokenKind
    fn check_keyword(
        &self,
        start_idx: usize,
        rest_of_keyword: &str,
        proposed_kind: TokenKind,
    ) -> Result<TokenKind, ScannerError> {
        let tokens_are_same_length =
            self.length_of_current_token()? == start_idx + rest_of_keyword.len();
        let suspected_keyword_substr =
            self.string_from_start_idx(start_idx, rest_of_keyword.len())?;

        if tokens_are_same_length && suspected_keyword_substr == rest_of_keyword {
            return Ok(proposed_kind);
        }

        return Ok(TokenKind::Identifier);
    }

    fn identifier_type(&self) -> Result<TokenKind, ScannerError> {
        let c = self
            .start
            .clone()
            .peekable()
            .peek()
            .map(|(_, c)| *c)
            .ok_or_else(|| ScannerError::UnexpectedEndOfInput(self.line))?;
        match c {
            'a' => self.check_keyword(1, "nd", TokenKind::And),
            'c' => self.check_keyword(1, "lass", TokenKind::Class),
            'e' => self.check_keyword(1, "lse", TokenKind::Else),
            'f' => {
                if self.length_of_current_token()? > 1 {
                    match self.peek_char_after_start()? {
                        'a' => self.check_keyword(2, "lse", TokenKind::False),
                        'o' => self.check_keyword(2, "r", TokenKind::For),
                        'u' => self.check_keyword(2, "n", TokenKind::Fun),
                        _ => Ok(TokenKind::Identifier),
                    }
                } else {
                    Ok(TokenKind::Identifier)
                }
            }
            'i' => self.check_keyword(1, "f", TokenKind::If),
            'n' => self.check_keyword(1, "il", TokenKind::Nil),
            'o' => self.check_keyword(1, "r", TokenKind::Or),
            'p' => self.check_keyword(1, "rint", TokenKind::Print),
            'r' => self.check_keyword(1, "eturn", TokenKind::Return),
            's' => self.check_keyword(1, "uper", TokenKind::Super),
            't' => {
                if self.length_of_current_token()? > 1 {
                    match self.peek_char_after_start()? {
                        'h' => self.check_keyword(2, "is", TokenKind::This),
                        'r' => self.check_keyword(2, "ue", TokenKind::True),
                        _ => Ok(TokenKind::Identifier),
                    }
                } else {
                    Ok(TokenKind::Identifier)
                }
            }
            'v' => self.check_keyword(1, "ar", TokenKind::Var),
            'w' => self.check_keyword(1, "hile", TokenKind::While),
            // Default to identifier since it's definitely not a keyword
            _ => Ok(TokenKind::Identifier),
        }
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

    #[test]
    fn scan_string_literal() {
        let mut scanner = Scanner::new("\"hello world\"\0");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::String, "Expected string");
        assert_eq!(token.to_string(), "\"hello world\"", "{:?}", token);
    }

    #[test]
    fn scan_string_literal_multi_line() {
        let mut scanner = Scanner::new("\"hello \n world\"\0");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::String, "Expected string");
        assert_eq!(token.to_string(), "\"hello \n world\"", "{:?}", token);
        assert_eq!(token.line, 2);
    }

    #[test]
    fn scan_unterminated_string_literal() {
        let mut scanner = Scanner::new("\"hello\0");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        if let TokenKind::Error(msg) = token.kind {
            assert!(msg.contains("Unterminated string"));
        } else {
            assert!(false, "Expected kind to be Error");
        }
    }

    #[test]
    fn scan_one_layer_trie() {
        let mut scanner = Scanner::new("if\0");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::If, "{:?}", token);
    }

    #[test]
    fn scan_two_layer_trie() {
        let mut scanner = Scanner::new("this\0");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::This, "{:?}", token);
    }

    #[test]
    fn scan_two_layer_trie_identifier() {
        let mut scanner = Scanner::new("thisnt\0");

        let token = scanner.scan_token();
        assert!(token.is_ok(), "{}", token.unwrap_err());
        let token = token.unwrap();
        assert_eq!(token.kind, TokenKind::Identifier, "{:?}", token);
    }
}
