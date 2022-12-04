use crate::{
    chunk::{debug::dissassemble_chunk, Chunk, OpCode},
    debugln,
    scanner::{Scanner, ScannerError, Token, TokenKind},
};
use std::{clone, collections::HashMap, fmt, hash::Hash};

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

/// Precedence from lowest to highest
#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
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

impl Precedence {
    fn next(&self) -> Precedence {
        let next_enum_integer = *self as u8 + 1;
        Precedence::from(next_enum_integer)
    }
}

impl From<u8> for Precedence {
    fn from(v: u8) -> Self {
        match v {
            x if x == Precedence::None as u8 => Precedence::None,
            x if x == Precedence::Assignment as u8 => Precedence::Assignment,
            x if x == Precedence::Or as u8 => Precedence::Or,
            x if x == Precedence::And as u8 => Precedence::And,
            x if x == Precedence::Equality as u8 => Precedence::Equality,
            x if x == Precedence::Comparison as u8 => Precedence::Comparison,
            x if x == Precedence::Term as u8 => Precedence::Term,
            x if x == Precedence::Factor as u8 => Precedence::Factor,
            x if x == Precedence::Unary as u8 => Precedence::Unary,
            x if x == Precedence::Call as u8 => Precedence::Call,
            x if x == Precedence::Primary as u8 => Precedence::Primary,
            _ => Precedence::Primary,
        }
    }
}

type ParseFn = dyn Fn(&mut Compiler, Chunk) -> Result<Chunk, CompilerError>;

struct ParserRule {
    prefix: Option<Box<ParseFn>>,
    infix: Option<Box<ParseFn>>,
    precedence: Precedence,
}

enum ParserCallType {
    Prefix,
    Infix,
}

struct ParserRuleResult {
    /// Always returns the parsed chunk, even if parsing failed
    chunk: Chunk,
    error: Option<CompilerError>,
    precedence: Precedence,
}

impl ParserRule {
    fn new(
        prefix: Option<Box<ParseFn>>,
        infix: Option<Box<ParseFn>>,
        precedence: Precedence,
    ) -> ParserRule {
        ParserRule {
            prefix,
            infix,
            precedence,
        }
    }
    fn get_next_precedence(&self) -> Precedence {
        todo!()
    }
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

    /// This is a hacky workaround to the parse table in clox. A better approach would be to have a big match expression
    /// that matches a TokenKind to a ParserRule. It would either return a reference to an already-constructed ParserRule or, if given the appropriate args,
    /// it would return the request information from the ParserRule such as the precedence or the result of the applied infix or prefix operation
    /// TODO use a better approach

    fn get_rule(
        &mut self,
        operator_kind: &TokenKind,
        call_type: ParserCallType,
    ) -> (Option<Box<ParseFn>>, Precedence) {
        match operator_kind {
            TokenKind::LeftParen => (
                match call_type {
                    ParserCallType::Prefix => {
                        Some(Box::new(|compiler, chunk| compiler.grouping(chunk)))
                    }
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::RightParen => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::LeftBrace => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::RightBrace => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Comma => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Dot => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Minus => (
                match call_type {
                    ParserCallType::Prefix => {
                        Some(Box::new(|compiler, chunk| compiler.unary(chunk)))
                    }
                    ParserCallType::Infix => {
                        Some(Box::new(|compiler, chunk| compiler.binary(chunk)))
                    }
                },
                Precedence::Term,
            ),
            TokenKind::Plus => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => {
                        Some(Box::new(|compiler, chunk| compiler.binary(chunk)))
                    }
                },
                Precedence::Term,
            ),
            TokenKind::SemiColon => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Slash => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => {
                        Some(Box::new(|compiler, chunk| compiler.binary(chunk)))
                    }
                },
                Precedence::Factor,
            ),
            TokenKind::Star => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => {
                        Some(Box::new(|compiler, chunk| compiler.binary(chunk)))
                    }
                },
                Precedence::Factor,
            ),
            TokenKind::Bang => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::BangEqual => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Equal => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::EqualEqual => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Greater => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::GreaterEqual => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Less => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::LessEqual => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Identifier => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::String => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Number => (
                match call_type {
                    ParserCallType::Prefix => {
                        Some(Box::new(|compiler, chunk| compiler.number(chunk)))
                    }
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::And => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Class => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Else => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::False => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::For => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Fun => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::If => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Nil => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Or => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Print => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Return => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Super => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::This => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::True => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Var => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::While => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Error(_) => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
            TokenKind::Eof => (
                match call_type {
                    ParserCallType::Prefix => None,
                    ParserCallType::Infix => None,
                },
                Precedence::None,
            ),
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

    fn advance(&mut self) -> Result<(), CompilerError> {
        self.parser.previous = self.parser.current.clone();

        loop {
            self.parser.current = self.scanner.scan_token().map_err(CompilerError::Scanner)?;
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
            return self.advance();
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

    fn emit_opcode(&self, opcode: OpCode, mut current_chunk: Chunk) -> Chunk {
        current_chunk.write_opcode(opcode, self.parser.previous.line);
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
        if self.parser.errors.is_empty() {
            debugln!("{}", dissassemble_chunk(&current_chunk, "code"));
        }

        current_chunk
    }

    fn binary(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        let operator_kind = self.parser.previous.kind.clone();
        // Technically either infix/prefix works since the precedence doesnt care
        let (_, precedence) = self.get_rule(&operator_kind, ParserCallType::Infix);
        chunk = self.parse_precedence(precedence.next(), chunk)?;

        match operator_kind {
            TokenKind::Plus => Ok(self.emit_opcode(OpCode::Add, chunk)),
            TokenKind::Minus => Ok(self.emit_opcode(OpCode::Subtract, chunk)),
            TokenKind::Star => Ok(self.emit_opcode(OpCode::Multiply, chunk)),
            TokenKind::Slash => Ok(self.emit_opcode(OpCode::Divide, chunk)),
            _ => Err(CompilerError::Unreachable(format!(
                "Did not expect operator {:?} in binary expression on line {}",
                operator_kind, self.parser.previous.line
            ))),
        }
    }

    // parses and generates bytecode for an expression
    fn expression(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        // Parse lowest precedence level
        self.parse_precedence(Precedence::Assignment, chunk)
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

    fn unary(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        let operator_kind = self.parser.previous.kind.clone();

        // parse only the operand
        chunk = self.parse_precedence(Precedence::Unary, chunk)?;

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
    fn parse_precedence(
        &mut self,
        precedence: Precedence,
        mut chunk: Chunk,
    ) -> Result<Chunk, CompilerError> {
        self.advance()?;
        let prev_kind = self.parser.previous.kind.clone();
        let (prefix_callable, _) = self.get_rule(&prev_kind, ParserCallType::Prefix);

        if let Some(prefix_fn) = prefix_callable {
            chunk = prefix_fn(self, chunk)?;
        } else {
            self.error_at(ErrorAtKind::Previous, Some("Expect expression"));
            // Non-fatal error, evidently?
            return Ok(chunk);
        };

        // Handle infix
        loop {
            let current_kind = self.parser.current.kind.clone();
            let (_, infix_precedence) = self.get_rule(&current_kind, ParserCallType::Infix);
            if precedence > infix_precedence {
                break;
            }
            self.advance()?;
            let prev_kind = self.parser.previous.kind.clone();
            let (infix_rule, _) = self.get_rule(&prev_kind, ParserCallType::Infix);
            match infix_rule {
                Some(r) => chunk = r(self, chunk)?,
                None => {
                    return Err(CompilerError::Unreachable(format!(
                        "No infix parser rule provided for {:?} on line {}",
                        &self.parser.previous.kind, self.parser.previous.line
                    )))
                }
            }
        }

        Ok(chunk)
    }

    fn grouping(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        let chunk = self.expression(chunk)?;
        self.consume(TokenKind::RightParen, "Expect ')' after expression")?;
        Ok(chunk)
    }

    pub fn compile(&mut self, source: &'source_lifetime str) -> Result<Chunk, CompilerError> {
        self.scanner = Scanner::new(source);
        let mut chunk = Chunk::new();
        self.advance()?;

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
