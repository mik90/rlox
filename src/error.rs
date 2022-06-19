use crate::token::TokenKind;
use crate::Token;
use std::error;
use std::fmt;
use std::io;

type ErrorMessage = String;
#[derive(Debug)]
pub enum LoxError {
    ScanError(ErrorMessage), // Not necessarily a fatal error. Returned in scanner
    ParserError(ErrorMessage), // Also not necessarily a fatal error, maybe this should be split into fatal/non-fatal
    IoError(io::Error),
}

impl error::Error for LoxError {}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            LoxError::ScanError(e) => write!(f, "ScanError - message: {}", e),
            LoxError::ParserError(e) => write!(f, "ParserError - message: {}", e),
            LoxError::IoError(e) => write!(f, "IoError: {}", e.to_string()),
        }
    }
}

impl From<io::Error> for LoxError {
    fn from(error: io::Error) -> Self {
        LoxError::IoError(error)
    }
}

impl LoxError {
    pub fn new_syntax_err(token: Token, err_desc: &str) -> LoxError {
        LoxError::ParserError(match token.kind {
            TokenKind::Eof => {
                format!("{} at end: {}", token.line, err_desc)
            }
            _ => {
                format!("{} at '{}': {}", token.line, token.text, err_desc)
            }
        })
    }
}
