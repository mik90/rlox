use crate::{
    error::LoxError,
    token::{self, LiteralToken, Token},
    token_type::TokenType,
};

pub struct Scanner {
    source: String,
    /// raw source code
    tokens: Vec<Token>,

    // These two fields offset into self.source
    start_idx: usize,
    cur_idx: usize,

    line: usize,
}

impl Scanner {
    fn is_at_end(&self) -> bool {
        self.cur_idx >= self.source.len()
    }

    fn advance(&mut self) -> Result<char, LoxError> {
        let c = self
            .source
            .chars()
            .nth(self.cur_idx)
            .ok_or(self.make_parser_error(format!(
                "Tried to index too far into souce code (cur_idx:{}, source code len:{})",
                self.cur_idx,
                self.source.len()
            )))?;
        self.cur_idx += 1;
        Ok(c)
    }
    fn add_token(&mut self, kind: TokenType) -> Result<(), LoxError> {
        self.add_token_with_literal(kind, LiteralToken::None)
    }

    fn add_token_with_literal(
        &mut self,
        kind: TokenType,
        literal: LiteralToken,
    ) -> Result<(), LoxError> {
        // TODO clean up this error handling/printout
        if self.start_idx > self.source.len() || self.cur_idx > self.source.len() {
            return Err(self.make_parser_error(format!(
                "Tried to index too far into souce code (start_idx:{}, cur_idx:{}, source code len:{})",
                self.start_idx,
                self.cur_idx,
                self.source.len())));
        }
        if self.start_idx > self.cur_idx {
            return Err(self.make_parser_error(format!(
                "start_idx is greater than cur_idx! (start_idx:{}, cur_idx:{})",
                self.start_idx, self.cur_idx,
            )));
        }
        let lexeme = self.source[self.start_idx..self.cur_idx].to_string();
        self.tokens.push(Token {
            kind,
            lexeme,
            literal,
            line: self.line,
        });

        Ok(())
    }

    fn make_parser_error(&self, msg: String) -> LoxError {
        return LoxError::ParseError(format!("{} at line {}", msg, self.line));
    }

    // allows us to handle two-character lexemes
    fn has_extra_char(&mut self, expected: char) -> bool {
        if !self.is_at_end() {
            return false;
        }

        if let Some(c) = self.source.chars().nth(self.cur_idx) {
            if c != expected {
                return false;
            }
        } else {
            // TODO if the cur idx is greater than source, this should return some type of error
            eprintln!(
                "cur_idx ({}) is greater than source.len ({})",
                self.cur_idx,
                self.source.len()
            );
        }
        self.cur_idx += 1;
        true
    }

    fn scan_token(&mut self) -> Result<(), LoxError> {
        let c = self.advance()?;
        let token_type = match c {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            ';' => TokenType::SemiColon,
            '*' => TokenType::Star,
            // TODO Simplify this multi-char lexeme handling
            '!' => {
                // Handle multi-char lexemes that share a common first char
                if self.has_extra_char('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                }
            }
            '=' => {
                if self.has_extra_char('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                }
            }
            '<' => {
                if self.has_extra_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                }
            }
            '>' => {
                if self.has_extra_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                }
            }
            _ => return Err(self.make_parser_error(format!("Found invalid character '{}'", c))),
        };
        self.add_token(token_type)
    }

    pub fn new(source: String) -> Self {
        Scanner {
            source: source,
            tokens: Vec::new(),
            start_idx: 0,
            cur_idx: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<(), LoxError> {
        let mut res: Result<(), LoxError> = Ok(());
        while !self.is_at_end() {
            self.start_idx = self.cur_idx;
            // As per chapter 4.5.1, keep scanning even if there's an error during parsing
            if let Err(e) = self.scan_token() {
                // Store the first error we saw in the scanning
                if !res.is_err() {
                    res = Err(e);
                }
            }
        }

        self.tokens.push(Token {
            kind: TokenType::Eof,
            lexeme: "".to_string(),
            literal: LiteralToken::None,
            line: self.line,
        });
        res
    }
}
