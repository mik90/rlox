use crate::{debugln, value::Value};

#[repr(u8)]
#[derive(Debug, PartialEq)]
pub enum OpCode {
    Constant = 0, //< Loads constant for use.
    Nil,          //< literal
    True,         //< literal
    False,        //< literal
    Equal,        //< Comparison
    Pop,          //< Discards value off stack
    GetLocal,     //< Gets value of local
    SetLocal,     //< Sets value of local
    GetGlobal,    //< Retrieves global via name's index
    SetGlobal,    //< Sets global variable via name's index
    DefineGlobal, //< Declares global and its name's index
    Greater,      //< Comparison
    Less,         //< Comparison
    Add,          //< Binary operation
    Subtract,     //< Binary operation
    Multiply,     //< Binary operation
    Divide,       //< Binary operation
    Not,          //< Unary boolean negation
    Negate,       //< Unary negation
    Print,        //< Prints to stdout
    Jump,         //< Unconditionally jumps
    JumpIfFalse,  //< Jumps to the given 16-bit address if the top of the stack is falsey
    Loop,         //< Unconditionally jumps back to an instruction given a 2-byte offset
    Return,       //< Return from current function.
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "OpCode::{:?}", self)
    }
}

impl TryFrom<u8> for OpCode {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            // TODO ew, this isn't guaranteed to be exhaustive. There has to be a built-in way to do this
            x if x == OpCode::Constant as u8 => Ok(OpCode::Constant),
            x if x == OpCode::Nil as u8 => Ok(OpCode::Nil),
            x if x == OpCode::True as u8 => Ok(OpCode::True),
            x if x == OpCode::False as u8 => Ok(OpCode::False),
            x if x == OpCode::Equal as u8 => Ok(OpCode::Equal),
            x if x == OpCode::Pop as u8 => Ok(OpCode::Pop),
            x if x == OpCode::GetLocal as u8 => Ok(OpCode::GetLocal),
            x if x == OpCode::SetLocal as u8 => Ok(OpCode::SetLocal),
            x if x == OpCode::GetGlobal as u8 => Ok(OpCode::GetGlobal),
            x if x == OpCode::SetGlobal as u8 => Ok(OpCode::SetGlobal),
            x if x == OpCode::DefineGlobal as u8 => Ok(OpCode::DefineGlobal),
            x if x == OpCode::Greater as u8 => Ok(OpCode::Greater),
            x if x == OpCode::Less as u8 => Ok(OpCode::Less),
            x if x == OpCode::Add as u8 => Ok(OpCode::Add),
            x if x == OpCode::Subtract as u8 => Ok(OpCode::Subtract),
            x if x == OpCode::Multiply as u8 => Ok(OpCode::Multiply),
            x if x == OpCode::Divide as u8 => Ok(OpCode::Divide),
            x if x == OpCode::Not as u8 => Ok(OpCode::Not),
            x if x == OpCode::Negate as u8 => Ok(OpCode::Negate),
            x if x == OpCode::Print as u8 => Ok(OpCode::Print),
            x if x == OpCode::Jump as u8 => Ok(OpCode::Jump),
            x if x == OpCode::JumpIfFalse as u8 => Ok(OpCode::JumpIfFalse),
            x if x == OpCode::Loop as u8 => Ok(OpCode::Loop),
            x if x == OpCode::Return as u8 => Ok(OpCode::Return),
            x => Err(format!("Could not convert '{}' into an OpCode", x)),
        }
    }
}

// Chunks are sequences of bytecodes
#[derive(Debug, Clone)]
pub struct Chunk {
    // The book has this as a dynamic array of uint8_t. I could use a rust enum but ill just stick with this for now i guess
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

type ChunkError = &'static str;

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }
    pub fn code_iter<'a>(&'_ self) -> std::slice::Iter<'_, u8> {
        self.code.iter()
    }

    #[allow(dead_code)]
    pub fn constant_iter(&self) -> std::slice::Iter<Value> {
        self.constants.iter()
    }

    /// Similar to count()
    pub fn code_len(&self) -> usize {
        self.code.len()
    }

    pub fn byte_at(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    pub fn byte_at_mut(&mut self, offset: usize) -> &mut u8 {
        &mut self.code[offset]
    }

    /// Source code line for a given instruction byte
    pub fn line_at(&self, offset: usize) -> usize {
        self.lines[offset]
    }

    pub fn write_opcode(&mut self, op: OpCode, source_line: usize) {
        self.code.push(op as u8);
        self.lines.push(source_line);
    }

    pub fn write_constant(&mut self, value: Value, source_line: usize) -> Result<u8, ChunkError> {
        println!("Adding constant '{value}'");
        let constant_index = self.add_constant(value);
        if constant_index > std::u8::MAX.into() {
            // We can only store one bytes worth of indexes into a constant array
            return Err("Too many constants in one chunk");
        }
        self.write_opcode(OpCode::Constant, source_line);
        self.write_byte(constant_index as u8, source_line);
        Ok(constant_index as u8)
    }

    pub fn write_byte(&mut self, byte: u8, source_line: usize) {
        self.code.push(byte);
        self.lines.push(source_line);
    }

    pub fn get_constant_value(&self, offset: usize) -> Option<&Value> {
        self.constants.get(offset)
    }

    /// Returns index of the constant that was added
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}

pub mod debug {
    use crate::chunk::{Chunk, OpCode};

    /// Returns debug string for a chunk
    #[allow(dead_code)]
    pub fn dissassemble_chunk(chunk: &Chunk, name: &str) -> String {
        let mut debug_string = format!("== {} ==\n", name);

        let mut offset: usize = 0;
        while offset < chunk.code_len() {
            let (instruction_string, new_offset) = dissassemble_instruction(chunk, offset);
            debug_string.push_str(instruction_string.as_str());
            offset = new_offset;
        }
        debug_string
    }

    /// Sign just affects the debug output, shows whether or not instr is jumping forward or backward
    fn jump_instruction(name: &str, sign: i32, chunk: &Chunk, offset: usize) -> (String, usize) {
        let mut jump = (chunk.byte_at(offset + 1) as u16) << 8;
        jump |= chunk.byte_at(offset + 2) as u16;
        (
            format!(
                "{:<16} offset: {:04} conditionally jumping to {:04}\n",
                name,
                offset,
                (offset as i32) + 3 + sign + (jump as i32)
            ),
            offset + 3,
        )
    }

    fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> (String, usize) {
        let constant_idx = chunk.byte_at(offset + 1);
        let constant_value = chunk
            .get_constant_value(constant_idx as usize)
            .expect("Constant value out of range");
        (
            format!(
                "{:<14} constant offset: {:04} val='{}'\n",
                name, constant_idx, constant_value
            ),
            offset + 2,
        )
    }

    fn byte_instruction(name: &str, chunk: &Chunk, offset: usize) -> (String, usize) {
        let slot = chunk.byte_at(offset + 1);
        (format!("{:<14} slot: {:04}\n", name, slot), offset + 2)
    }

    fn simple_instruction(name: &str, offset: usize) -> (String, usize) {
        (format!("{}\n", name), offset + 1)
    }

    fn print_line_number(chunk: &Chunk, offset: usize) -> String {
        if offset > 0 && chunk.line_at(offset) == chunk.line_at(offset - 1) {
            // Line number is same as the previous line, don't print the number twice
            String::from("        | ")
        } else {
            // Write line number for the instruction
            format!("line:{:>4} ", chunk.line_at(offset))
        }
    }

    pub fn dissassemble_instruction(chunk: &Chunk, offset: usize) -> (String, usize) {
        // Left pad with 0s, 4 digits
        let instruction_meta = format!(
            "instr offset: {:04} {}",
            offset,
            print_line_number(chunk, offset)
        );

        let instruction = chunk.byte_at(offset);
        let (instruction_string, offset) = match OpCode::try_from(instruction) {
            Ok(op) => match op {
                OpCode::Constant => constant_instruction("OP_CONSTANT", chunk, offset),
                OpCode::Nil => simple_instruction("OP_NIL", offset),
                OpCode::True => simple_instruction("OP_TRUE", offset),
                OpCode::False => simple_instruction("OP_FALSE", offset),
                OpCode::Pop => simple_instruction("OP_POP", offset),
                OpCode::GetLocal => byte_instruction("OP_GET_LOCAL", chunk, offset),
                OpCode::SetLocal => byte_instruction("OP_SET_LOCAL", chunk, offset),
                OpCode::GetGlobal => constant_instruction("OP_GET_GLOBAL", chunk, offset),
                OpCode::SetGlobal => constant_instruction("OP_SET_GLOBAL", chunk, offset),
                OpCode::DefineGlobal => constant_instruction("OP_DEFINE_GLOBAL", chunk, offset),
                OpCode::Equal => simple_instruction("OP_EQUAL", offset),
                OpCode::Greater => simple_instruction("OP_GREATER", offset),
                OpCode::Less => simple_instruction("OP_LESS", offset),
                OpCode::Add => simple_instruction("OP_ADD", offset),
                OpCode::Subtract => simple_instruction("OP_SUBTRACT", offset),
                OpCode::Multiply => simple_instruction("OP_MULTIPLY", offset),
                OpCode::Divide => simple_instruction("OP_DIVIDE", offset),
                OpCode::Not => simple_instruction("OP_NOT", offset),
                OpCode::Negate => simple_instruction("OP_NEGATE", offset),
                OpCode::Print => simple_instruction("OP_PRINT", offset),
                OpCode::Loop => jump_instruction("OP_LOOP", -1, chunk, offset),
                OpCode::Jump => jump_instruction("OP_JUMP", 1, chunk, offset),
                OpCode::JumpIfFalse => jump_instruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
                OpCode::Return => simple_instruction("OP_RETURN", offset),
            },
            Err(err) => (err, offset + 1),
        };
        (
            format!("{}{}", instruction_meta, instruction_string),
            offset,
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_opcode_translation() {
        let raw: u8 = OpCode::Subtract as u8;
        let op = OpCode::try_from(raw);
        assert!(op.is_ok());
        match op.unwrap() {
            OpCode::Subtract => (),
            op => assert!(false, "Unexpected opcode {:?}", op),
        }
    }

    #[test]
    fn test_example_chunks() {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(Value::Number(1.2));

        chunk.write_opcode(OpCode::Constant, 123);
        chunk.write_byte(constant as u8, 123);

        assert_eq!(chunk.byte_at(0), OpCode::Constant as u8);
        let constant_idx = chunk.byte_at(1);
        assert_eq!(constant_idx, 0);

        let value = chunk.get_constant_value(constant_idx as usize);
        assert!(value.is_some());
        assert_eq!(*value.unwrap(), Value::Number(1.2));
        assert_eq!(chunk.line_at(0), 123);
        assert_eq!(chunk.line_at(1), 123);

        chunk.write_opcode(OpCode::Return, 124);

        assert_eq!(chunk.byte_at(2), OpCode::Return as u8);
        assert_eq!(chunk.line_at(2), 124);
    }
}
