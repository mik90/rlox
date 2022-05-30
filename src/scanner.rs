use crate::{
    error::LoxError,
    token::{LiteralToken, Token},
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
        self.add_token_literal(kind, LiteralToken::None)
    }

    fn add_token_literal(
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

    fn scan_token(&mut self) -> Result<(), LoxError> {
        let c = self.advance()?;
        match c {
            '(' => self.add_token(TokenType::LeftParen)?,
            ')' => self.add_token(TokenType::RightParen)?,
            '{' => self.add_token(TokenType::LeftBrace)?,
            '}' => self.add_token(TokenType::RightBrace)?,
            ',' => self.add_token(TokenType::Comma)?,
            '.' => self.add_token(TokenType::Dot)?,
            '-' => self.add_token(TokenType::Minus)?,
            '+' => self.add_token(TokenType::Plus)?,
            ';' => self.add_token(TokenType::SemiColon)?,
            '*' => self.add_token(TokenType::Star)?,
            _ => return Err(self.make_parser_error(format!("Found invalid character '{}'", c))),
        }
        todo!()
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
        while !self.is_at_end() {
            self.start_idx = self.cur_idx;
            self.scan_token()?;
        }

        self.tokens.push(Token {
            kind: TokenType::Eof,
            lexeme: "".to_string(),
            literal: LiteralToken::None,
            line: self.line,
        });
        Ok(())
    }
}
