use crate::{
    error::LoxError,
    token::{self, LiteralKind, Token, TokenKind},
};
use std::{collections::HashMap, hash::Hash};

pub struct Scanner {
    source: String,
    /// raw source code
    tokens: Vec<Token>,

    // These two fields offset into self.source
    start_idx: usize,
    cur_idx: usize,

    line: usize,
    keywords: HashMap<&'static str, TokenKind>,
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
    fn add_token(&mut self, kind: TokenKind) -> Result<(), LoxError> {
        self.add_token_with_literal(kind, LiteralKind::None)
    }

    fn add_token_with_literal(
        &mut self,
        kind: TokenKind,
        literal: LiteralKind,
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
        return LoxError::ScanError(format!(
            "{} at line {}. cur_idx={}",
            msg, self.line, self.cur_idx
        ));
    }

    // allows us to handle two-character lexemes
    fn has_extra_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
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
    fn peek(&self) -> char {
        self.source.chars().nth(self.cur_idx).unwrap_or('\0')
    }
    fn peek_next(&self) -> char {
        self.source.chars().nth(self.cur_idx + 1).unwrap_or('\0')
    }

    fn scan_number(&mut self) -> Result<(), LoxError> {
        // consume all the digits before the .
        while self.peek().is_digit(10) {
            self.advance()?;
        }

        if self.peek() == '.' && self.peek_next().is_digit(10) {
            // consume the "."
            self.advance()?;

            // consume all the digits _after_ the dot
            while self.peek().is_digit(10) {
                self.advance()?;
            }
        }
        let len = self.cur_idx - self.start_idx;
        let substr = self
            .source
            .chars()
            .skip(self.start_idx)
            .take(len)
            .collect::<String>();
        let number = substr
            .parse::<f64>()
            .map_err(|_| LoxError::ScanError(format!("Could not parse '{}' as f64", substr)))?;
        self.add_token_with_literal(TokenKind::Number, LiteralKind::Number(number))
    }
    fn scan_identifier(&mut self) -> Result<(), LoxError> {
        while self.peek().is_alphanumeric() {
            self.advance()?;
        }
        let len = self.cur_idx - self.start_idx;
        let lexeme = self
            .source
            .chars()
            .skip(self.start_idx)
            .take(len)
            .collect::<String>();
        let token_type = self
            .keywords
            .get(lexeme.as_str())
            .unwrap_or(&TokenKind::Identifier)
            .clone();
        match token_type {
            TokenKind::Identifier => {
                self.add_token_with_literal(token_type, LiteralKind::Identifier(lexeme))
            }
            _ => self.add_token_with_literal(token_type, LiteralKind::None),
        }
    }

    fn scan_string(&mut self) -> Result<(), LoxError> {
        while self.peek() != '"' && !self.is_at_end() {
            // keep consuming until the string ends
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance()?;
        }
        if !self.is_at_end() && self.peek() != '"' {
            return Err(self.make_parser_error(format!(
                "Unterminated string. Current char '{}'",
                self.peek()
            )));
        }
        // consume the closing ""
        self.advance()?;

        // Dont include the closing quotes, and don't include the starting quotes
        let substr_len = (self.cur_idx - 1) - (self.start_idx + 1);
        let value = self
            .source
            .chars()
            .skip(self.start_idx + 1)
            .take(substr_len)
            .collect::<String>();
        self.add_token_with_literal(TokenKind::String, LiteralKind::String(value))
    }

    fn scan_token(&mut self) -> Result<(), LoxError> {
        let c = self.advance()?;
        let token_type = match c {
            ' ' | '\r' | '\t' => return Ok(()), // ignore whitespace
            '\n' => {
                self.line += 1;
                return Ok(());
            }
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            ',' => TokenKind::Comma,
            '"' => return self.scan_string(),
            '.' => TokenKind::Dot,
            '-' => TokenKind::Minus,
            '+' => TokenKind::Plus,
            ';' => TokenKind::SemiColon,
            '*' => TokenKind::Star,
            '!' if self.has_extra_char('=') => TokenKind::BangEqual,
            '!' => TokenKind::Bang,
            '=' if self.has_extra_char('=') => TokenKind::EqualEqual,
            '=' => TokenKind::Equal,
            '<' if self.has_extra_char('=') => TokenKind::LessEqual,
            '<' => TokenKind::Less,
            '>' if self.has_extra_char('=') => TokenKind::GreaterEqual,
            '>' => TokenKind::Greater,
            '/' => {
                if self.has_extra_char('/') {
                    // if we hit a comment, consume until we hit the end of the line
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance()?;
                    }
                    // early exit after getting rid of all the comment chars
                    return Ok(());
                } else {
                    // not a comment, it's a division operator
                    TokenKind::Slash
                }
            }
            '0'..='9' => {
                return self.scan_number();
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                return self.scan_identifier();
            }
            _ => return Err(self.make_parser_error(format!("Found invalid character '{}'", c))),
        };
        // TODO add some type of logging
        // println!("Adding token type {:?}", token_type);
        self.add_token(token_type)
    }

    pub fn new(source: String) -> Self {
        Scanner {
            source: source,
            tokens: Vec::new(),
            start_idx: 0,
            cur_idx: 0,
            line: 1,
            keywords: HashMap::from([
                ("and", TokenKind::And),
                ("class", TokenKind::Class),
                ("else", TokenKind::Else),
                ("false", TokenKind::False),
                ("for", TokenKind::For),
                ("fun", TokenKind::Fun),
                ("if", TokenKind::If),
                ("nil", TokenKind::Nil),
                ("or", TokenKind::Or),
                ("print", TokenKind::Print),
                ("return", TokenKind::Return),
                ("super", TokenKind::Super),
                ("this", TokenKind::This),
                ("true", TokenKind::True),
                ("var", TokenKind::Var),
                ("while", TokenKind::While),
            ]),
        }
    }

    // TODO maybe have the tokens returned as part of here instead of being a member var
    // Why can this just be a static function that creates a scanner and runs it?
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
            kind: TokenKind::Eof,
            lexeme: "".to_string(),
            literal: LiteralKind::None,
            line: self.line,
        });
        res
    }

    pub fn copy_tokens(&self) -> Vec<Token> {
        self.tokens.clone()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn scan_comment() {
        let input = "// this is a comment\n //\t this is another comment".to_string();

        let mut scan = Scanner::new(input);
        let res = scan.scan_tokens();
        assert!(res.is_ok(), "{}", res.unwrap_err().to_string());

        let tokens = scan.copy_tokens();
        assert_eq!(tokens.len(), 1); // the EOF token is the only one we expect
        assert_eq!(tokens[tokens.len() - 1].kind, TokenKind::Eof);
    }

    #[test]
    fn scan_symbols() {
        let input = "+-,".to_string();

        let mut scan = Scanner::new(input);
        let res = scan.scan_tokens();
        assert!(res.is_ok(), "{}", res.unwrap_err().to_string());

        let tokens = scan.copy_tokens();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].kind, TokenKind::Plus);
        assert_eq!(tokens[1].kind, TokenKind::Minus);
        assert_eq!(tokens[2].kind, TokenKind::Comma);
        assert_eq!(tokens[tokens.len() - 1].kind, TokenKind::Eof);
    }

    #[test]
    fn scan_string() {
        let input = r#""Hello world""#.to_string();

        let mut scan = Scanner::new(input);
        let res = scan.scan_tokens();
        assert!(res.is_ok(), "{}", res.unwrap_err().to_string());

        let tokens = scan.copy_tokens();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].kind, TokenKind::String);
        assert_eq!(
            tokens[0].literal,
            LiteralKind::String("Hello world".to_string())
        );

        assert_eq!(tokens[tokens.len() - 1].kind, TokenKind::Eof);
    }

    #[test]
    fn scan_unterminated_string() {
        let input = r#""Hello unterminated world"#.to_string();

        let mut scan = Scanner::new(input);
        let res = scan.scan_tokens();
        assert!(res.is_err());
    }

    #[test]
    fn scan_number_literal() {
        let input = r#"1.2"#.to_string();

        let mut scan = Scanner::new(input);
        let res = scan.scan_tokens();
        assert!(res.is_ok(), "{}", res.unwrap_err().to_string());

        let tokens = scan.copy_tokens();
        assert_eq!(tokens.len(), 2, "\ntokens: {:?}", tokens);
        assert_eq!(tokens[0].kind, TokenKind::Number);
        assert_eq!(tokens[0].lexeme, "1.2".to_string());
        assert_eq!(tokens[0].literal, LiteralKind::Number(1.2));

        assert_eq!(tokens[tokens.len() - 1].kind, TokenKind::Eof);
    }

    #[test]
    fn scan_literal() {
        let input = "and or foobar printfoo".to_string();

        let mut scan = Scanner::new(input);
        let res = scan.scan_tokens();
        assert!(res.is_ok(), "{}", res.unwrap_err().to_string());

        let tokens = scan.copy_tokens();
        assert_eq!(tokens.len(), 5, "\ntokens: {:?}", tokens);

        assert_eq!(tokens[0].kind, TokenKind::And);
        assert_eq!(tokens[0].lexeme, "and".to_string());
        assert_eq!(tokens[0].literal, LiteralKind::None);

        assert_eq!(tokens[1].kind, TokenKind::Or);
        assert_eq!(tokens[1].lexeme, "or".to_string());
        assert_eq!(tokens[1].literal, LiteralKind::None);

        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].lexeme, "foobar".to_string());
        assert_eq!(
            tokens[2].literal,
            LiteralKind::Identifier("foobar".to_string())
        );

        assert_eq!(tokens[3].kind, TokenKind::Identifier);
        assert_eq!(tokens[3].lexeme, "printfoo".to_string());
        assert_eq!(
            tokens[3].literal,
            LiteralKind::Identifier("printfoo".to_string())
        );

        assert_eq!(tokens[tokens.len() - 1].kind, TokenKind::Eof);
    }

    #[test]
    fn scan_expr() {
        let input = "(* (- 123) (group 45.67))".to_string();

        let mut scan = Scanner::new(input);
        let res = scan.scan_tokens();
        assert!(res.is_ok(), "{}", res.unwrap_err().to_string());

        let tokens = scan.copy_tokens();
        assert_eq!(tokens.len(), 12, "\ntokens: {:?}", tokens);
    }
}
