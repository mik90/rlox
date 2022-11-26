use crate::{
    chunk::Chunk,
    scanner::{Scanner, ScannerError, TokenKind},
};
use std::fmt;

#[derive(Debug, Clone)]
pub enum CompilerError {
    Scanner(ScannerError),
}
impl std::error::Error for CompilerError {}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            CompilerError::Scanner(err) => {
                write!(f, "Unable to compile due to error during scan: {}", err)
            }
        }
    }
}

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {}
    }

    fn advance(&mut self) {
        todo!()
    }

    fn consume(&mut self, kind: TokenKind, error_msg: &str) {
        todo!()
    }

    fn expression(&mut self) {
        todo!()
    }

    pub fn compile(&mut self, source: &str, chunk: &mut Chunk) -> Result<(), CompilerError> {
        let mut scanner = Scanner::new(source);
        self.advance();
        self.expression();
        self.consume(TokenKind::Eof, "Expect end of expression");

        todo!()
    }
}
