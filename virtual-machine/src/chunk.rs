// Chunks are sequences of bytecodes

pub enum OpCode {
    Return, // Return from current function
}

pub struct Chunk {
    pub code: Vec<OpCode>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk { code: vec![] }
    }
}

pub mod debug {
    use crate::chunk::{Chunk, OpCode};

    pub fn dissassemble_chunk(chunk: &Chunk, name: &str) {
        println!("== {} ==", name);

        let mut offset: usize = 0;
        while offset < chunk.code.len() {
            offset = dissassemble_instruction(chunk, offset);
        }
    }
    fn simple_instruction(name: &str, offset: usize) -> usize {
        println!("{}", name);
        offset + 1
    }
    fn dissassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
        // Left pad with 0s, 4 digits
        print!("{:04} ", offset);

        let instruction = &chunk.code[offset];
        match instruction {
            OpCode::Return => simple_instruction("OP_RETURN", offset),
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
