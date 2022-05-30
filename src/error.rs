use std::error;
use std::fmt;
use std::io;

type ErrorMessage = String;
#[derive(Debug)]
pub enum LoxError {
    ParseError(ErrorMessage),
    LexicalError(ErrorMessage), // i.e. Syntax errors
    IoError(io::Error),
}

impl error::Error for LoxError {}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            LoxError::ParseError(e) => write!(f, "ParseError - message: {}", e),
            LoxError::IoError(e) => write!(f, "IoError: {}", e.to_string()),
        }
    }
}

impl From<io::Error> for LoxError {
    fn from(error: io::Error) -> Self {
        LoxError::IoError(error)
    }
}
