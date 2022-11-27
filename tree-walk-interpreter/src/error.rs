use crate::token::TokenKind;
use crate::Token;
use std::error;
use std::fmt;

pub type ErrorMessage = String;
#[derive(Debug, Clone)]
pub enum LoxError {
    Scanner(ErrorMessage), // Not necessarily a fatal error. Returned in scanner
    Parser(ErrorMessage), // Also not necessarily a fatal error, maybe this should be split into fatal/non-fatal
}

impl error::Error for LoxError {}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            LoxError::Scanner(e) => write!(f, "Scanner - message: {}", e),
            LoxError::Parser(e) => write!(f, "Parser - message: {}", e),
        }
    }
}

impl LoxError {
    pub fn new_parser_err(token: Token, err_desc: &str) -> LoxError {
        let formatted_msg = match token.kind {
            TokenKind::Eof => {
                format!("On line {} at end: {}", token.line, err_desc)
            }
            _ => {
                format!("On line {} at '{}': {}", token.line, token.lexeme, err_desc)
            }
        };
        LoxError::Parser(formatted_msg)
    }
}
