use crate::value::{Value, ValueArray};

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
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: ValueArray::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn at_offset(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    pub fn write_opcode(&mut self, op: OpCode) {
        self.code.push(op as u8);
    }

    pub fn write_byte(&mut self, byte: u8) {
        self.code.push(byte);
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
        let constant_idx = chunk.at_offset(offset + 1);
        let constant_value = chunk.get_constant_value(constant_idx as usize);
        println!("{:<16} {:04} '{}'", name, constant_idx, constant_value);
        offset + 2
    }

    fn simple_instruction(name: &str, offset: usize) -> usize {
        println!("{}", name);
        offset + 1
    }

    fn dissassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
        // Left pad with 0s, 4 digits
        print!("{:04} ", offset);

        let instruction = chunk.at_offset(offset);
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
    #[test]
    fn test_left_pad_formatting() {
        let offset: usize = 1;
        let text = format!("{:04}", offset);
        assert_eq!(text, "0001");
    }
}
