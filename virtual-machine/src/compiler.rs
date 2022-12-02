use crate::{
    chunk::{Chunk, OpCode},
    scanner::{Scanner, ScannerError, Token, TokenKind},
};
use std::fmt;

#[derive(Debug, Clone)]
pub enum CompilerError {
    Scanner(ScannerError),
    Parse(Vec<String>),  // multiple errors can be stored internally
    Bytecode(String),    // error when building bytecode
    Unreachable(String), // error when the parse tree reaches something previously thought impossible
}
impl std::error::Error for CompilerError {}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            CompilerError::Scanner(err) => {
                write!(f, "Unable to compile due to error during scan: {}", err)
            }
            CompilerError::Parse(errors) => {
                write!(
                    f,
                    "Unable to compile due parse errors: {}",
                    errors.join("\n")
                )
            }
            CompilerError::Bytecode(error) => {
                write!(f, "Hit error while emitting bytecode: {}", error)
            }
            CompilerError::Unreachable(error) => {
                write!(f, "Hit unreachable path during compilation: {}", error)
            }
        }
    }
}

struct Parser<'source_lifetime> {
    current: Token<'source_lifetime>,
    previous: Token<'source_lifetime>,
    panic_mode: bool,
    errors: Vec<String>,
}

impl<'source_lifetime> Parser<'source_lifetime> {
    pub fn new() -> Parser<'source_lifetime> {
        Parser {
            current: Token::new_uninit(),
            previous: Token::new_uninit(),
            panic_mode: false,
            errors: vec![],
        }
    }
}

/// Pecedence from lowest to highest
enum Precedence {
    None = 0,
    Assignment, //< =
    Or,         //< or
    And,        //< and
    Equality,   //< == !=
    Comparison, //< < > <= >=
    Term,       //< + -
    Factor,     //< * /
    Unary,      //< ! -
    Call,       //< . ()
    Primary,
}

pub struct Compiler<'source_lifetime> {
    // TODO, these may not need to be members since their init logic is so odd
    parser: Parser<'source_lifetime>,
    scanner: Scanner<'source_lifetime>,
}

enum ErrorAtKind {
    Previous,
    Current,
}

impl<'source_lifetime> Compiler<'source_lifetime> {
    pub fn new() -> Compiler<'source_lifetime> {
        Compiler {
            parser: Parser::new(),
            scanner: Scanner::new(""),
        }
    }

    /// TODO this function call needs to be cleaned up
    fn error_at(&mut self, error_at_kind: ErrorAtKind, msg: Option<&'static str>) {
        if self.parser.panic_mode {
            return;
        }
        self.parser.panic_mode = true;

        let token = match error_at_kind {
            ErrorAtKind::Previous => &self.parser.previous,
            ErrorAtKind::Current => &self.parser.current,
        };
        let mut current_error: Vec<String> = vec![];
        current_error.push(format!("[line {}] Error", token.line));

        match &token.kind {
            TokenKind::Eof => {
                current_error.push(format!(" at end: {}", msg.unwrap_or_default()));
            }
            TokenKind::Error(error_msg) => {
                // The error message itself isn't stored in the source, it's stored as part of the enum
                current_error.push(format!(": {}", error_msg));
            }
            _ => {
                current_error.push(format!(
                    "at '{}': {}",
                    token.to_string(),
                    msg.unwrap_or_default()
                ));
            }
        }
        self.parser.errors.push(current_error.join(""));
    }

    fn advance(&mut self) -> Result<(), ScannerError> {
        self.parser.previous = self.parser.current.clone();

        loop {
            self.parser.current = self.scanner.scan_token()?;
            if !matches!(self.parser.current.kind, TokenKind::Error(_)) {
                // Break on non-error types
                break;
            }
            self.error_at(ErrorAtKind::Current, None);
        }
        Ok(())
    }

    fn consume(
        &mut self,
        expected_kind: TokenKind,
        error_msg: &'static str,
    ) -> Result<(), CompilerError> {
        if matches!(&self.parser.current.kind, kind) {
            return self.advance().map_err(CompilerError::Scanner);
        }

        self.error_at(ErrorAtKind::Current, Some(error_msg));
        Ok(())
    }

    /// The book stores the current chunk as a static variable, ill just pass it in
    /// This may not be sufficient for later
    fn emit_byte(&self, byte: u8, mut current_chunk: Chunk) -> Chunk {
        current_chunk.write_byte(byte, self.parser.previous.line);
        current_chunk
    }

    fn emit_opcode_and_byte(&self, opcode: OpCode, byte: u8, mut current_chunk: Chunk) -> Chunk {
        current_chunk.write_opcode(opcode, self.parser.previous.line);
        current_chunk.write_byte(byte, self.parser.previous.line);
        current_chunk
    }

    fn emit_constant(&self, value: f64, mut current_chunk: Chunk) -> Result<Chunk, CompilerError> {
        current_chunk
            .write_constant(value, self.parser.previous.line)
            .map_err(|e| {
                CompilerError::Bytecode(format!("On line {}, {}", self.parser.previous.line, e))
            })?;
        Ok(current_chunk)
    }

    /// Temporary measure as per the book to print hte value of our single expression
    fn end_compiler(&mut self, mut current_chunk: Chunk) -> Chunk {
        current_chunk.write_opcode(OpCode::Return, self.parser.previous.line);
        current_chunk
    }

    // parses and generates bytecode for an expression
    fn expression(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        // Parse lowest precedence level
        self.parse_precedence(Precedence::Assignment);
        todo!()
    }

    fn number(&self, mut current_chunk: Chunk) -> Result<Chunk, CompilerError> {
        let value = self
            .parser
            .previous
            .to_string()
            .parse::<f64>()
            .map_err(|e| {
                CompilerError::Parse(vec![format!(
                    "Could not parse float from {:?}, {}",
                    self.parser.previous, e
                )])
            })?;
        current_chunk = self.emit_constant(value, current_chunk)?;
        Ok(current_chunk)
    }

    fn unary(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        let operator_kind = self.parser.previous.kind.clone();

        // parse only the operand
        self.parse_precedence(Precedence::Unary);

        if let TokenKind::Minus = operator_kind {
            Ok(self.emit_byte(OpCode::Negate as u8, chunk))
        } else {
            Err(CompilerError::Unreachable(format!(
                "Expected unary negation on line {} but instead saw {:?}",
                self.parser.previous.line, operator_kind
            )))
        }
    }

    /// Starts at current token and parses any expressin at the current precedence or higher
    fn parse_precedence(&mut self, precedence: Precedence) {
        todo!()
    }

    fn grouping(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        let chunk = self.expression(chunk)?;
        self.consume(TokenKind::RightParen, "Expect ')' after expression")?;
        Ok(chunk)
    }

    pub fn compile(&mut self, source: &'source_lifetime str) -> Result<Chunk, CompilerError> {
        self.scanner = Scanner::new(source);
        let mut chunk = Chunk::new();
        self.advance().map_err(CompilerError::Scanner)?;

        chunk = self.expression(chunk)?;

        self.consume(TokenKind::Eof, "Expect end of expression")?;

        if self.parser.errors.is_empty() {
            Ok(chunk)
        } else {
            let errors: Vec<String> = self.parser.errors.drain(0..).collect();
            Err(CompilerError::Parse(errors))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn advance_parser() {
        let mut compiler = Compiler::new();
        compiler.scanner = Scanner::new("1 + 2\0");

        let expected_tokens = vec![
            TokenKind::Number,
            TokenKind::Plus,
            TokenKind::Number,
            TokenKind::Eof,
        ];

        for _token_kind in expected_tokens {
            let res = compiler.advance();
            assert!(res.is_ok(), "{}", res.unwrap_err());
            assert!(
                matches!(&compiler.parser.current.kind, _token_kind),
                "{:?}",
                compiler.parser.current.kind
            );
        }
    }
}
