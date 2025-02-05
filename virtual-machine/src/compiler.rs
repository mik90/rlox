use crate::{
    chunk::{debug::dissassemble_chunk, Chunk, OpCode},
    debugln, herefmt,
    scanner::{Scanner, ScannerError, Token, TokenKind},
    value::{Obj, ObjFunction, Value},
};
use std::{collections::HashMap, fmt, rc::Rc};

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

type ParseFn = dyn Fn(&mut Compiler, Chunk, bool) -> Result<Chunk, CompilerError>;

// The functions are stored in an Rc so that they can be cloned from the map and called
// We cannot call the function if we're borrowing it since it uses the mutable Compiler
// Another option would be to only call it in-place but that isn't very ergonomic
#[derive(Clone)]
struct ParserRule {
    prefix: Option<Rc<ParseFn>>,
    infix: Option<Rc<ParseFn>>,
    precedence: Precedence,
}

impl ParserRule {
    fn new(
        prefix: Option<Rc<ParseFn>>,
        infix: Option<Rc<ParseFn>>,
        precedence: Precedence,
    ) -> ParserRule {
        ParserRule {
            prefix,
            infix,
            precedence,
        }
    }
}

type ParseRuleMap = HashMap<
    std::mem::Discriminant<TokenKind>, //< Kind of token
    ParserRule,
>;

struct Local<'source_lifetime> {
    // It's possible that I only need the lexeme and not the whole name
    name: Token<'source_lifetime>,
    lexeme: String, //< stringified token since i cant figure out how to get a non-allocating str for the token
    depth: i32,     //< I'd like this to be unsigned but the book makes use of a -1 here
}

enum FunctionKind {
    Function,
    Script,
}

pub struct Compiler<'source_lifetime> {
    // TODO, these may not need to be members since their init logic is so odd
    parser: Parser<'source_lifetime>,
    scanner: Scanner<'source_lifetime>,

    rules: ParseRuleMap,

    locals: Vec<Local<'source_lifetime>>,
    scope_depth: i32, //< number of blocks surrounding current part of code being compiled. 0 is global scope
    current_function: ObjFunction, //< The compiler always has a function in mind
    // I could use a sum type here but this is closer to the book
    current_function_kind: FunctionKind,
}

enum ErrorAtKind {
    Previous,
    Current,
}

impl<'source_lifetime> Local<'source_lifetime> {
    pub fn new(name: Token<'source_lifetime>, depth: i32) -> Self {
        let lexeme = name.to_string();
        Local {
            name,
            lexeme,
            depth,
        }
    }
}

impl<'source_lifetime> Compiler<'source_lifetime> {
    /// max length of a single byte, which is all of the locals we can track
    const MAX_LOCAL_COUNT: usize = u8::MAX as usize;

    pub fn new() -> Compiler<'source_lifetime> {
        // Used for internal VM purposes
        let internal_local = Local::new(Token::new_uninit(), 0);

        Compiler {
            parser: Parser::new(),
            scanner: Scanner::new(""),
            rules: Compiler::make_rules(),
            locals: vec![internal_local],
            scope_depth: 0,
            current_function: ObjFunction::new("<script>".to_owned()),
            current_function_kind: FunctionKind::Script,
        }
    }

    /// Number of locals in scope
    fn local_count(&self) -> usize {
        self.locals.len()
    }

    fn make_rules() -> ParseRuleMap {
        // Closures that can be used for any of these parser rules
        let grouping =
            Rc::new(|compiler: &mut Compiler, chunk: Chunk, _: bool| compiler.grouping(chunk));
        let literal =
            Rc::new(|compiler: &mut Compiler, chunk: Chunk, _: bool| compiler.literal(chunk));
        let number =
            Rc::new(|compiler: &mut Compiler, chunk: Chunk, _: bool| compiler.number(chunk));
        let string =
            Rc::new(|compiler: &mut Compiler, chunk: Chunk, _: bool| compiler.string(chunk));
        let binary =
            Rc::new(|compiler: &mut Compiler, chunk: Chunk, _: bool| compiler.binary(chunk));
        let unary = Rc::new(|compiler: &mut Compiler, chunk: Chunk, _: bool| compiler.unary(chunk));
        let variable = Rc::new(|compiler: &mut Compiler, chunk: Chunk, can_assign: bool| {
            compiler.variable(chunk, can_assign)
        });
        let and = Rc::new(|compiler: &mut Compiler, chunk: Chunk, _: bool| compiler.and(chunk));
        let or = Rc::new(|compiler: &mut Compiler, chunk: Chunk, _: bool| compiler.or(chunk));

        HashMap::from([
            (
                std::mem::discriminant(&TokenKind::LeftParen),
                ParserRule::new(Some(grouping.clone()), None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::RightParen),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::LeftBrace),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::RightBrace),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Comma),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Dot),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Minus),
                ParserRule::new(Some(unary.clone()), Some(binary.clone()), Precedence::Term),
            ),
            (
                std::mem::discriminant(&TokenKind::Plus),
                ParserRule::new(None, Some(binary.clone()), Precedence::Term),
            ),
            (
                std::mem::discriminant(&&TokenKind::SemiColon),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Slash),
                ParserRule::new(None, Some(binary.clone()), Precedence::Factor),
            ),
            (
                std::mem::discriminant(&TokenKind::Star),
                ParserRule::new(None, Some(binary.clone()), Precedence::Factor),
            ),
            (
                std::mem::discriminant(&TokenKind::Bang),
                ParserRule::new(Some(unary.clone()), None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::BangEqual),
                ParserRule::new(None, Some(binary.clone()), Precedence::Equality),
            ),
            (
                std::mem::discriminant(&TokenKind::Equal),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::EqualEqual),
                ParserRule::new(None, Some(binary.clone()), Precedence::Equality),
            ),
            (
                std::mem::discriminant(&TokenKind::Greater),
                ParserRule::new(None, Some(binary.clone()), Precedence::Comparison),
            ),
            (
                std::mem::discriminant(&TokenKind::GreaterEqual),
                ParserRule::new(None, Some(binary.clone()), Precedence::Comparison),
            ),
            (
                std::mem::discriminant(&TokenKind::Less),
                ParserRule::new(None, Some(binary.clone()), Precedence::Comparison),
            ),
            (
                std::mem::discriminant(&TokenKind::LessEqual),
                ParserRule::new(None, Some(binary.clone()), Precedence::Comparison),
            ),
            (
                std::mem::discriminant(&TokenKind::Identifier),
                ParserRule::new(Some(variable), None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::String),
                ParserRule::new(Some(string.clone()), None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Number),
                ParserRule::new(Some(number.clone()), None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::And),
                ParserRule::new(None, Some(and), Precedence::And),
            ),
            (
                std::mem::discriminant(&TokenKind::Class),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Else),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::False),
                ParserRule::new(Some(literal.clone()), None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::For),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Fun),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::If),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Nil),
                ParserRule::new(Some(literal.clone()), None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Or),
                ParserRule::new(None, Some(or), Precedence::Or),
            ),
            (
                std::mem::discriminant(&TokenKind::Print),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Return),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Super),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::This),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::True),
                ParserRule::new(Some(literal.clone()), None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Var),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::While),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Error("".to_owned())),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Eof),
                ParserRule::new(None, None, Precedence::None),
            ),
        ])
    }

    fn get_rule(&self, operator_kind: &TokenKind) -> ParserRule {
        self.rules
            .get(&std::mem::discriminant(operator_kind))
            .expect(&herefmt!(
                "Could not find parser rule for TokenKind '{:?}'",
                operator_kind
            ))
            .clone()
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
                    " at '{}': {}",
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
        if std::mem::discriminant(&self.parser.current.kind)
            == std::mem::discriminant(&expected_kind)
        {
            return self.advance();
        }

        self.error_at(ErrorAtKind::Current, Some(error_msg));
        Ok(())
    }

    fn check(&self, expected_kind: TokenKind) -> bool {
        self.parser.current.kind == expected_kind
    }

    /// Consumes token if it matches the expected kind
    fn token_matches(&mut self, expected_kind: TokenKind) -> Result<bool, CompilerError> {
        if !self.check(expected_kind) {
            return Ok(false);
        }
        self.advance()?;
        Ok(true)
    }

    /// The book stores the current chunk as a static variable, ill just pass it in
    /// This may not be sufficient for later
    fn emit_byte(&self, byte: u8, mut chunk: Chunk) -> Chunk {
        chunk.write_byte(byte, self.parser.previous.line);
        chunk
    }

    fn emit_opcode(&self, opcode: OpCode, mut chunk: Chunk) -> Chunk {
        chunk.write_opcode(opcode, self.parser.previous.line);
        chunk
    }

    fn emit_opcode_and_byte(&self, opcode: OpCode, byte: u8, mut chunk: Chunk) -> Chunk {
        chunk.write_opcode(opcode, self.parser.previous.line);
        chunk.write_byte(byte, self.parser.previous.line);
        chunk
    }

    fn emit_loop(&self, loop_start: u8, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        chunk.write_opcode(OpCode::Loop, self.parser.previous.line);

        // + 2 takes into account the size of the two byte operands to Loop
        let offset = chunk.code_len() - (loop_start as usize) + 2;
        if offset > i16::MAX as usize {
            return Err(CompilerError::Bytecode(herefmt!("Loop body is too large")));
        }
        let offset = offset as u16;
        let lower = (offset >> 8) as u8;
        let upper = offset as u8;
        // TODO do i really need the bitmasking here?
        chunk = self.emit_byte(lower & u8::MAX, chunk);
        chunk = self.emit_byte(upper & u8::MAX, chunk);
        Ok(chunk)
    }

    /// Emits new constant to chunk
    fn emit_constant(&self, value: Value, chunk: Chunk) -> Result<(u8, Chunk), CompilerError> {
        let (constant_index, mut chunk) = self.make_constant(value, chunk)?;
        if constant_index > u8::MAX.into() {
            // We can only store one bytes worth of indexes into a constant array
            return Err(CompilerError::Bytecode(herefmt!(
                "Too many constants in one chunk"
            )));
        }
        chunk.write_opcode(OpCode::Constant, self.parser.previous.line);
        chunk.write_byte(constant_index as u8, self.parser.previous.line);
        Ok((constant_index, chunk))
    }

    fn patch_jump(&self, offset: u8, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        let jump = chunk.code_len() - (offset as usize) - 2;

        if jump > u16::MAX as usize {
            return Err(CompilerError::Bytecode(herefmt!(
                "{} is too much code to jump over",
                jump
            )));
        }
        debugln!("patching jump with value of {}", jump);
        *chunk.byte_at_mut(offset as usize) = ((jump >> 8) & 0xff) as u8;
        *chunk.byte_at_mut((offset + 1).into()) = (jump & 0xff) as u8;
        Ok(chunk)
    }

    fn emit_jump(&self, instruction: OpCode, mut chunk: Chunk) -> (u8, Chunk) {
        chunk = self.emit_opcode(instruction, chunk);
        chunk = self.emit_byte(u8::MAX, chunk);
        chunk = self.emit_byte(u8::MAX, chunk);

        let instruction_offset = (chunk.code_len() - 2) as u8;
        (instruction_offset, chunk)
    }

    /// Adds constant to constant table although it doesn't emit bytecode
    fn make_constant(&self, value: Value, mut chunk: Chunk) -> Result<(u8, Chunk), CompilerError> {
        let idx = chunk.add_constant(value);
        if idx > u8::MAX.into() {
            // We can only store one bytes worth of indexes into a constant array
            return Err(CompilerError::Bytecode(herefmt!(
                "Too many constants in one chunk"
            )));
        }
        Ok((idx as u8, chunk))
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self, mut chunk: Chunk) -> Chunk {
        self.scope_depth -= 1;

        // Pop off all locals beyond our current depth
        while let Some(last) = self.locals.last() {
            if last.depth > self.scope_depth {
                chunk = self.emit_opcode(OpCode::Pop, chunk);
                self.locals.pop();
            } else {
                break;
            }
        }
        chunk
    }

    fn binary(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        let operator_kind = self.parser.previous.kind.clone();
        // Technically either infix/prefix works since the precedence doesnt care
        let rule = self.get_rule(&operator_kind);
        chunk = self.parse_precedence(rule.precedence.next(), chunk)?;

        match operator_kind {
            // Comparisons
            // Equality
            TokenKind::BangEqual => {
                let chunk = self.emit_opcode(OpCode::Equal, chunk);
                Ok(self.emit_opcode(OpCode::Not, chunk))
            }
            TokenKind::EqualEqual => Ok(self.emit_opcode(OpCode::Equal, chunk)),
            // Greater
            TokenKind::Greater => Ok(self.emit_opcode(OpCode::Greater, chunk)),
            TokenKind::GreaterEqual => {
                let chunk = self.emit_opcode(OpCode::Less, chunk);
                Ok(self.emit_opcode(OpCode::Not, chunk))
            }
            // Less
            TokenKind::Less => Ok(self.emit_opcode(OpCode::Less, chunk)),
            TokenKind::LessEqual => {
                let chunk = self.emit_opcode(OpCode::Greater, chunk);
                Ok(self.emit_opcode(OpCode::Not, chunk))
            }
            // Arithmetic
            TokenKind::Plus => Ok(self.emit_opcode(OpCode::Add, chunk)),
            TokenKind::Minus => Ok(self.emit_opcode(OpCode::Subtract, chunk)),
            TokenKind::Star => Ok(self.emit_opcode(OpCode::Multiply, chunk)),
            TokenKind::Slash => Ok(self.emit_opcode(OpCode::Divide, chunk)),
            // Other
            _ => Err(CompilerError::Unreachable(format!(
                "Did not expect operator {:?} in binary expression on line {}",
                operator_kind, self.parser.previous.line
            ))),
        }
    }

    fn literal(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        match &self.parser.previous.kind {
            TokenKind::False => Ok(self.emit_opcode(OpCode::False, chunk)),
            TokenKind::Nil => Ok(self.emit_opcode(OpCode::Nil, chunk)),
            TokenKind::True => Ok(self.emit_opcode(OpCode::True, chunk)),
            other => Err(CompilerError::Unreachable(herefmt!(
                "Expected literal but saw {:?}",
                other
            ))),
        }
    }

    // parses and generates bytecode for an expression
    fn expression(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        // Parse lowest precedence level
        self.parse_precedence(Precedence::Assignment, chunk)
    }

    fn block(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
            chunk = self.declaration(chunk)?;
        }

        self.consume(TokenKind::RightBrace, "Expect '}' after block")?;
        Ok(chunk)
    }

    fn var_declaration(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        let (global_idx, mut chunk) = self.parse_variable(chunk, "Expect variable name")?;

        chunk = if self.token_matches(TokenKind::Equal)? {
            self.expression(chunk)?
        } else {
            self.emit_opcode(OpCode::Nil, chunk)
        };
        self.consume(
            TokenKind::SemiColon,
            "Expect ';' after variable declaration",
        )?;

        self.define_variable(global_idx, chunk)
    }

    // parses and generates bytecode for an expression statement
    // This is an expression followed by a semicolon, and side effects are expected to occur in most cases
    // The result is discarded
    fn expression_statement(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        let chunk = self.expression(chunk)?;
        self.consume(TokenKind::SemiColon, "Expect ';' after expression")?;
        Ok(self.emit_opcode(OpCode::Pop, chunk))
    }

    fn for_statement(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        self.begin_scope();

        self.consume(TokenKind::LeftParen, "Expect '(' after 'for'.")?;

        if self.token_matches(TokenKind::SemiColon)? {
            // No initializer, this is okay
        } else if self.token_matches(TokenKind::Var)? {
            chunk = self.var_declaration(chunk)?;
        } else {
            chunk = self.expression_statement(chunk)?;
        }

        let mut loop_start = chunk.code_len();
        let mut exit_jump: i32 = -1;

        // Non-empty conditional clause
        if !self.token_matches(TokenKind::SemiColon)? {
            chunk = self.expression(chunk)?;
            self.consume(TokenKind::SemiColon, "Expect ';' after loop condition.")?;

            // emit jump if/when condition is false
            let (idx, new_chunk) = self.emit_jump(OpCode::JumpIfFalse, chunk);
            exit_jump = idx as i32;
            chunk = new_chunk;
            chunk = self.emit_opcode(OpCode::Pop, chunk);
        }

        // non-empty increment clause
        if !self.token_matches(TokenKind::RightParen)? {
            // instruction index post-increment
            let (body_jump, new_chunk) = self.emit_jump(OpCode::Jump, chunk);
            chunk = new_chunk;
            let increment_start = chunk.code_len();
            chunk = self.expression(chunk)?;
            // Discard value of increment expression
            chunk = self.emit_opcode(OpCode::Pop, chunk);
            self.consume(TokenKind::RightParen, "Expect ')' after for clauses.")?;

            chunk = self.emit_loop(loop_start as u8, chunk)?;
            // The loop should start by running the increment expression whenever it jumps back up
            loop_start = increment_start;
            chunk = self.patch_jump(body_jump, chunk)?;
        }

        chunk = self.statement(chunk)?;

        chunk = self.emit_loop(loop_start as u8, chunk)?;

        if exit_jump != -1 {
            chunk = self.patch_jump(exit_jump as u8, chunk)?;
            chunk = self.emit_opcode(OpCode::Pop, chunk);
        }

        Ok(self.end_scope(chunk))
    }

    fn if_statement(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        self.consume(TokenKind::LeftParen, "Expect '(' after 'if'.")?;
        chunk = self.expression(chunk)?;
        self.consume(TokenKind::RightParen, "Expect ')' after condition.")?;

        let (then_jump, mut chunk) = self.emit_jump(OpCode::JumpIfFalse, chunk);

        // Pop off value if the condition is truthy
        chunk = self.emit_opcode(OpCode::Pop, chunk);

        chunk = self.statement(chunk)?;

        let (else_jump, mut chunk) = self.emit_jump(OpCode::Jump, chunk);

        chunk = self.patch_jump(then_jump, chunk)?;

        // Pop off value if the condition is falsey
        chunk = self.emit_opcode(OpCode::Pop, chunk);

        if self.token_matches(TokenKind::Else)? {
            chunk = self.statement(chunk)?;
        }
        chunk = self.patch_jump(else_jump, chunk)?;
        Ok(chunk)
    }

    // parses and generates bytecode for a declaration statement
    fn declaration(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        let chunk = if self.token_matches(TokenKind::Var)? {
            self.var_declaration(chunk)
        } else {
            self.statement(chunk)
        };

        if self.parser.panic_mode {
            self.synchronize()?;
        }
        chunk
    }

    // parses and generates bytecode for a statement
    fn statement(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        if self.token_matches(TokenKind::Print)? {
            self.print_statement(chunk)
        } else if self.token_matches(TokenKind::For)? {
            self.for_statement(chunk)
        } else if self.token_matches(TokenKind::If)? {
            self.if_statement(chunk)
        } else if self.token_matches(TokenKind::While)? {
            self.while_statement(chunk)
        } else if self.token_matches(TokenKind::LeftBrace)? {
            self.begin_scope();
            let chunk = self.block(chunk)?;
            Ok(self.end_scope(chunk))
        } else {
            self.expression_statement(chunk)
        }
    }

    // parses and generates bytecode for a print statement
    fn print_statement(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        let chunk = self.expression(chunk)?;
        self.consume(TokenKind::SemiColon, "Expect ';' after value")?;
        Ok(self.emit_opcode(OpCode::Print, chunk))
    }

    fn while_statement(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        let loop_start = chunk.code_len();
        debugln!("loop_start={}", loop_start);
        self.consume(TokenKind::LeftParen, "Expect '(' after 'while'")?;
        let chunk = self.expression(chunk)?;
        self.consume(TokenKind::RightParen, "Expect ')' after condition")?;

        let (exit_jump, mut chunk) = self.emit_jump(OpCode::JumpIfFalse, chunk);
        chunk = self.emit_opcode(OpCode::Pop, chunk);

        chunk = self.statement(chunk)?;

        chunk = self.emit_loop(loop_start as u8, chunk)?;

        chunk = self.patch_jump(exit_jump, chunk)?;
        Ok(self.emit_opcode(OpCode::Pop, chunk))
    }

    fn synchronize(&mut self) -> Result<(), CompilerError> {
        self.parser.panic_mode = false;

        // Skip tokens until we hit something that's most likely a statement boundary

        while self.parser.current.kind != TokenKind::Eof {
            if self.parser.previous.kind == TokenKind::SemiColon {
                return Ok(());
            }
            match self.parser.current.kind {
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => {
                    return Ok(());
                }
                _ => (), // Continue searching for a statement boundary
            }
        }
        self.advance()
    }

    fn number(&self, chunk: Chunk) -> Result<Chunk, CompilerError> {
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
        let (_, chunk) = self.emit_constant(Value::Number(value), chunk)?;
        Ok(chunk)
    }

    fn or(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        // If the lhs condition is false, jump to the rhs
        let (else_jump, chunk) = self.emit_jump(OpCode::JumpIfFalse, chunk);
        let (end_jump, mut chunk) = self.emit_jump(OpCode::Jump, chunk);

        chunk = self.patch_jump(else_jump, chunk)?;
        // Pop on falsett
        chunk = self.emit_opcode(OpCode::Pop, chunk);

        chunk = self.parse_precedence(Precedence::Or, chunk)?;

        self.patch_jump(end_jump, chunk)
    }

    fn string(&self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        let length = self.parser.previous.length - 2;
        let copy: String = self
            .parser
            .previous
            .start
            .clone()
            .skip(1)
            .take(length)
            .map(|(_, c)| c)
            .collect();

        let (_, chunk) = self.emit_constant(Value::from(Obj::String(copy)), chunk)?;
        Ok(chunk)
    }

    fn named_variable(
        &mut self,
        name: String,
        mut chunk: Chunk,
        can_assign: bool,
    ) -> Result<Chunk, CompilerError> {
        let mut variable_index = self.resolve_local(&name)?;

        let (get_opcode, set_opcode) = if variable_index != -1 {
            // Local depth is known, so use local getter/setter
            (OpCode::GetLocal, OpCode::SetLocal)
        } else {
            // Local depth is not known, so resolve value as a global
            let (global_arg, updated_chunk) = self.identifier_constant(name, chunk)?;
            variable_index = global_arg as i32; // :(
            chunk = updated_chunk;
            (OpCode::GetGlobal, OpCode::SetGlobal)
        };

        if can_assign && self.token_matches(TokenKind::Equal)? {
            // Handle assignment expression
            let chunk = self.expression(chunk)?;
            Ok(self.emit_opcode_and_byte(set_opcode, variable_index as u8, chunk))
        } else {
            Ok(self.emit_opcode_and_byte(get_opcode, variable_index as u8, chunk))
        }
    }

    fn variable(&mut self, chunk: Chunk, can_assign: bool) -> Result<Chunk, CompilerError> {
        self.named_variable(self.parser.previous.to_string(), chunk, can_assign)
    }

    fn unary(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        let operator_kind = self.parser.previous.kind.clone();

        // parse only the operand
        chunk = self.parse_precedence(Precedence::Unary, chunk)?;

        match operator_kind {
            TokenKind::Minus => Ok(self.emit_opcode(OpCode::Negate, chunk)),
            TokenKind::Bang => Ok(self.emit_opcode(OpCode::Not, chunk)),
            other => Err(CompilerError::Unreachable(format!(
                "Expected unary negation on line {} but instead saw {:?}",
                self.parser.previous.line, other
            ))),
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
        let rule = self.get_rule(&prev_kind);

        let can_assign = precedence <= Precedence::Assignment;

        if let Some(prefix_fn) = rule.prefix {
            chunk = prefix_fn(self, chunk, can_assign)?;
        } else {
            self.error_at(ErrorAtKind::Previous, Some("Expect expression"));
            // Non-fatal error, evidently?
            return Ok(chunk);
        };

        // Handle infix
        loop {
            let current_kind = self.parser.current.kind.clone();
            let rule = self.get_rule(&current_kind);
            if precedence > rule.precedence {
                break;
            }
            self.advance()?;
            let prev_kind = self.parser.previous.kind.clone();
            let rule = self.get_rule(&prev_kind);
            match rule.infix {
                Some(r) => chunk = r(self, chunk, can_assign)?,
                None => {
                    return Err(CompilerError::Unreachable(format!(
                        "No infix parser rule provided for {:?} on line {}",
                        &self.parser.previous.kind, self.parser.previous.line
                    )))
                }
            }
        }

        if can_assign && self.token_matches(TokenKind::Equal)? {
            self.error_at(ErrorAtKind::Previous, Some("Invalid assignment target"));
        }

        Ok(chunk)
    }

    fn parse_variable(
        &mut self,
        chunk: Chunk,
        error_msg: &'static str,
    ) -> Result<(u8, Chunk), CompilerError> {
        self.consume(TokenKind::Identifier, error_msg)?;

        self.declare_variable()?;

        // Exit function early if we're in local scope
        if self.scope_depth > 0 {
            // Return a dummy index so we don't look up the variable name in the hash table of globals
            return Ok((0, chunk));
        }

        self.identifier_constant(self.parser.previous.to_string(), chunk)
    }

    fn mark_local_initialized(&mut self) -> Result<(), CompilerError> {
        match self.locals.last_mut() {
            Some(local) => {
                local.depth = self.scope_depth;
                debugln!(
                    "Defining local '{}' as initialized with depth {}",
                    local.lexeme,
                    local.depth
                );
                Ok(())
            }
            None => Err(CompilerError::Parse(vec![herefmt!(
                "Cannot mark local as initialized since there are no locals"
            )])),
        }
    }

    fn define_variable(&mut self, global: u8, chunk: Chunk) -> Result<Chunk, CompilerError> {
        if self.scope_depth > 0 {
            // early exit for local scopes
            self.mark_local_initialized()?;
            return Ok(chunk);
        }
        Ok(self.emit_opcode_and_byte(OpCode::DefineGlobal, global, chunk))
    }

    fn and(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        let (end_jump, mut chunk) = self.emit_jump(OpCode::JumpIfFalse, chunk);

        chunk = self.emit_opcode(OpCode::Pop, chunk);
        chunk = self.parse_precedence(Precedence::And, chunk)?;

        self.patch_jump(end_jump, chunk)
    }

    fn identifier_constant(
        &mut self,
        identifier: String,
        chunk: Chunk,
    ) -> Result<(u8, Chunk), CompilerError> {
        self.make_constant(Value::from(Obj::String(identifier)), chunk)
    }

    fn add_local(&mut self, name: Token<'source_lifetime>) {
        if self.local_count() == Compiler::MAX_LOCAL_COUNT {
            self.error_at(
                ErrorAtKind::Previous,
                Some("Too many local variables in function"),
            );
            return;
        }
        let local = Local::new(name, -1);
        self.locals.push(local);
    }

    fn resolve_local(&self, name: &str) -> Result<i32, CompilerError> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            /*debugln!(
                "({i} out locals.len()={}, local.name={}, local.depth={}",
                self.locals.len(),
                local.lexeme,
                local.depth
            );*/
            if local.lexeme == name {
                if local.depth == -1 {
                    return Err(CompilerError::Parse(vec![herefmt!(
                        "Cannot read variable in its own initializer. Line {}",
                        self.parser.previous.line
                    )]));
                }
                //debugln!("Resolved local '{}' with depth of '{}'", local.lexeme, i);
                return Ok(i as i32);
            }
        }
        // If we can't find it, it must be a global
        return Ok(-1);
    }

    fn declare_variable(&mut self) -> Result<(), CompilerError> {
        if self.scope_depth == 0 {
            // Bail out for globals which are late-bound
            return Ok(());
        }

        // Sadly a copy, but avoids aliasing
        let token = self.parser.previous.clone();
        let token_name = token.to_string();

        // Detect duplicate locals in current scope
        for local in self.locals.iter().rev() {
            if local.depth != -1 && local.depth < self.scope_depth {
                break;
            }
            if local.lexeme == token_name {
                return Err(CompilerError::Parse(vec![herefmt!(
                    "Already a variable named '{}' in this scope on line {}",
                    token_name,
                    token.line
                )]));
            }
        }
        self.add_local(token);
        Ok(())
    }

    fn grouping(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        let chunk = self.expression(chunk)?;
        self.consume(TokenKind::RightParen, "Expect ')' after expression")?;
        Ok(chunk)
    }

    pub fn compile(&mut self, source: &'source_lifetime str) -> Result<ObjFunction, CompilerError> {
        debugln!("compile()");
        self.scanner = Scanner::new(source);
        let mut chunk = Chunk::new();
        self.advance()?;

        while !self.token_matches(TokenKind::Eof)? {
            chunk = self.declaration(chunk)?;
        }

        self.consume(TokenKind::Eof, "Expect end of expression")?;

        // This section is the same as end_compiler()/endCompiler(). I found that keeping it was more confusing
        chunk.write_opcode(OpCode::Return, self.parser.previous.line);
        if self.parser.errors.is_empty() {
            // Dissassemble the compiled chunk in debug mode
            debugln!(
                "{}",
                dissassemble_chunk(&chunk, &self.current_function.name)
            );

            self.current_function.chunk = chunk;
            Ok(self.current_function.clone()) // TODO, prob shouldn't clone. Maybe swap?
        } else {
            // Collect all the errors
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

        for token_kind in expected_tokens {
            let res = compiler.advance();
            assert!(res.is_ok(), "{}", res.unwrap_err());
            assert_eq!(
                std::mem::discriminant(&compiler.parser.current.kind),
                std::mem::discriminant(&token_kind),
                "{:?}",
                compiler.parser.current.kind
            );
        }
    }

    #[test]
    fn compile_literal() {
        let mut compiler = Compiler::new();

        let source = "print true;\0";
        let function = compiler.compile(source);
        assert!(function.is_ok());
        let function = function.unwrap();

        let mut code = function.chunk.code_iter();
        assert!(code.clone().count() >= 1);
        let byte = code.next().unwrap();

        let maybe_op = OpCode::try_from(*byte);
        assert!(maybe_op.is_ok());
        assert_eq!(maybe_op.unwrap(), OpCode::True);
    }
    #[test]
    fn compile_comparisons() {
        let mut compiler = Compiler::new();
        let function = compiler.compile("print !(5 - 4 > 3 * 2 == !nil);\0");
        let instructions: Vec<u8> = function.unwrap().chunk.code_iter().copied().collect();

        // Instruction indices are skipped since we don't care about the indexes of the constants
        // This is really just using the offsets found in the debug output and checking that future changes don't break
        // how the opcodes are emitted
        assert_eq!(instructions[0], OpCode::Constant as u8);

        assert_eq!(instructions[2], OpCode::Constant as u8);

        assert_eq!(instructions[4], OpCode::Subtract as u8);
        assert_eq!(instructions[5], OpCode::Constant as u8);

        assert_eq!(instructions[7], OpCode::Constant as u8);

        assert_eq!(instructions[9], OpCode::Multiply as u8);
        assert_eq!(instructions[10], OpCode::Greater as u8);
        assert_eq!(instructions[11], OpCode::Nil as u8);
        assert_eq!(instructions[12], OpCode::Not as u8);
        assert_eq!(instructions[13], OpCode::Equal as u8);
        assert_eq!(instructions[14], OpCode::Not as u8);
    }

    #[test]
    fn compile_string_concat() {
        let mut compiler = Compiler::new();

        let source = r#"print "st" + "ri" + "ng"; "#;
        let function = compiler.compile(source).unwrap();
        let chunk = function.chunk;

        let instructions: Vec<u8> = chunk.code_iter().copied().collect();

        // Instruction indices are skipped since we don't care about the indexes of the constants
        // This is really just using the offsets found in the debug output and checking that future changes don't break
        // how the opcodes are emitted
        assert_eq!(instructions[0], OpCode::Constant as u8);
        let value = chunk.get_constant_value(instructions[1] as usize);
        assert!(value.is_some());
        assert_eq!(format!("{}", value.unwrap()), "st");

        assert_eq!(instructions[2], OpCode::Constant as u8);
        let value = chunk.get_constant_value(instructions[3] as usize);
        assert!(value.is_some());
        assert_eq!(format!("{}", value.unwrap()), "ri");

        assert_eq!(instructions[4], OpCode::Add as u8);

        assert_eq!(instructions[5], OpCode::Constant as u8);
        let value = chunk.get_constant_value(instructions[6] as usize);
        assert!(value.is_some());
        assert_eq!(format!("{}", value.unwrap()), "ng");

        assert_eq!(instructions[7], OpCode::Add as u8);
    }

    #[test]
    fn compile_local_set() {
        let mut compiler = Compiler::new();

        // First statement
        let res = compiler.compile(
            "var global = -1;
                    {
                        var a = 2; 
                        global = a; 
                    }\0",
        );
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let chunk = res.unwrap().chunk;
        println!("{}", crate::chunk::debug::dissassemble_chunk(&chunk, ""));
    }

    #[test]
    fn compile_if_else() {
        let mut compiler = Compiler::new();

        // First statement
        let res = compiler.compile(
            "if (1) {
                      // Nothing
                    } else {
                      // Nothing
                    }
                    \0",
        );
        assert!(res.is_ok(), "{}", res.unwrap_err());
    }
}
