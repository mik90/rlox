use crate::value::{Value, ValueArray};

#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    Constant = 0, // Loads constant for use.
    Add = 1,      // Binary operation
    Subtract = 2, // Binary operation
    Multiply = 3, // Binary operation
    Divide = 4,   // Binary operation
    Negate = 5,   // Unary negation
    Return = 6,   // Return from current function.
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            // TODO ew
            x if x == OpCode::Constant as u8 => Ok(OpCode::Constant),
            x if x == OpCode::Add as u8 => Ok(OpCode::Add),
            x if x == OpCode::Subtract as u8 => Ok(OpCode::Subtract),
            x if x == OpCode::Multiply as u8 => Ok(OpCode::Multiply),
            x if x == OpCode::Divide as u8 => Ok(OpCode::Divide),
            x if x == OpCode::Negate as u8 => Ok(OpCode::Negate),
            x if x == OpCode::Return as u8 => Ok(OpCode::Return),
            _ => Err(()),
        }
    }
}

// Chunks are sequences of bytecodes
#[derive(Debug, Clone)]
pub struct Chunk {
    // The book has this as a dynamic array of uint8_t. I could use a rust enum but ill just stick with this for now i guess
    code: Vec<u8>,
    constants: ValueArray,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: ValueArray::new(),
            lines: vec![],
        }
    }
    pub fn code_iter<'a>(&'a self) -> std::slice::Iter<'a, u8> {
        self.code.iter()
    }

    pub fn constant_iter<'a>(&'a self) -> std::slice::Iter<'a, Value> {
        self.constants.iter()
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn byte_at(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    pub fn line_at(&self, offset: usize) -> usize {
        self.lines[offset]
    }

    pub fn write_opcode(&mut self, op: OpCode, source_line: usize) {
        self.code.push(op as u8);
        self.lines.push(source_line);
    }

    pub fn write_constant(&mut self, value: Value, source_line: usize) {
        let constant_index = self.add_constant(value);
        self.write_opcode(OpCode::Constant, source_line);
        self.write_byte(constant_index as u8, source_line);
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

    /// Returns debug string
    pub fn dissassemble_chunk(chunk: &Chunk, name: &str) -> String {
        let mut debug_string = format!("== {} ==\n", name);

        let mut offset: usize = 0;
        while offset < chunk.len() {
            let (instruction_string, new_offset) = dissassemble_instruction(chunk, offset);
            debug_string.push_str(instruction_string.as_str());
            offset = new_offset;
        }
        debug_string
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
                OpCode::Add => simple_instruction("OP_ADD", offset),
                OpCode::Subtract => simple_instruction("OP_SUBTRACT", offset),
                OpCode::Multiply => simple_instruction("OP_MULTIPLY", offset),
                OpCode::Divide => simple_instruction("OP_DIVIDE", offset),
                OpCode::Negate => simple_instruction("OP_NEGATE", offset),
                OpCode::Return => simple_instruction("OP_RETURN", offset),
            },
            Err(()) => (
                format!("Could not create opcode from '{}'", instruction),
                offset + 1,
            ),
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
        let raw: u8 = 2;
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
        let constant = chunk.add_constant(1.2);

        chunk.write_opcode(OpCode::Constant, 123);
        chunk.write_byte(constant as u8, 123);

        assert_eq!(chunk.byte_at(0), OpCode::Constant as u8);
        let constant_idx = chunk.byte_at(1);
        assert_eq!(constant_idx, 0);

        let value = chunk.get_constant_value(constant_idx as usize);
        assert!(value.is_some());
        assert_eq!(*value.unwrap(), 1.2);
        assert_eq!(chunk.line_at(0), 123);
        assert_eq!(chunk.line_at(1), 123);

        chunk.write_opcode(OpCode::Return, 124);

        assert_eq!(chunk.byte_at(2), OpCode::Return as u8);
        assert_eq!(chunk.line_at(2), 124);
    }
}
