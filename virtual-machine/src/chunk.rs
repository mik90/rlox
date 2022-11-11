use crate::value::{Value, ValueArray};

#[derive(Debug)]
pub enum OpCode {
    Constant = 0, // Loads constant for use. (OpCode, ConstantIdx)
    Return = 1,   // Return from current function. (OpCode)
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(OpCode::Constant),
            1 => Ok(OpCode::Return),
            _ => Err(()),
        }
    }
}

// Chunks are sequences of bytecodes
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

    pub fn write_byte(&mut self, byte: u8, source_line: usize) {
        self.code.push(byte);
        self.lines.push(source_line);
    }

    pub fn get_constant_value(&self, offset: usize) -> Value {
        self.constants[offset]
    }

    /// Returns index of the constant that was added
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}

pub mod debug {
    use crate::chunk::{Chunk, OpCode};

    pub fn dissassemble_chunk(chunk: &Chunk, name: &str) {
        println!("== {} ==", name);

        let mut offset: usize = 0;
        while offset < chunk.len() {
            offset = dissassemble_instruction(chunk, offset);
        }
    }

    fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
        let constant_idx = chunk.byte_at(offset + 1);
        let constant_value = chunk.get_constant_value(constant_idx as usize);
        println!("{:<16} {:04} '{}'", name, constant_idx, constant_value);
        offset + 2
    }

    fn simple_instruction(name: &str, offset: usize) -> usize {
        println!("{}", name);
        offset + 1
    }

    fn print_line_number(chunk: &Chunk, offset: usize) {
        if offset > 0 && chunk.line_at(offset) == chunk.line_at(offset - 1) {
            // Line number is same as the previous line, don't print the number twice
            print!("   | ");
        } else {
            // Print hte line number for the instruction
            print!("{:>4} ", chunk.line_at(offset));
        }
    }

    fn dissassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
        // Left pad with 0s, 4 digits
        print!("{:04} ", offset);
        print_line_number(chunk, offset);

        let instruction = chunk.byte_at(offset);
        match OpCode::try_from(instruction) {
            Ok(op) => match op {
                OpCode::Constant => constant_instruction("OP_CONSTANT", chunk, offset),
                OpCode::Return => simple_instruction("OP_RETURN", offset),
            },
            Err(()) => {
                eprintln!("Could not create opcode from '{}'", instruction);
                offset + 1
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_left_pad_formatting() {
        let offset: usize = 1;
        let text = format!("{:04}", offset);
        assert_eq!(text, "0001");
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

        assert_eq!(chunk.get_constant_value(constant_idx as usize), 1.2);
        assert_eq!(chunk.line_at(0), 123);
        assert_eq!(chunk.line_at(1), 123);

        chunk.write_opcode(OpCode::Return, 124);

        assert_eq!(chunk.byte_at(2), OpCode::Return as u8);
        assert_eq!(chunk.line_at(2), 124);
    }
}
