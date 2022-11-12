use crate::{
    chunk::{Chunk, OpCode},
    debug,
    value::Value,
};
use std::fmt;

pub struct Vm<'a> {
    // TODO Use generics to store these iterators
    chunk_iter: std::slice::Iter<'a, Chunk>,
    instruction_iter: std::slice::Iter<'a, u8>,
}

pub type ErrorMessage = String;

#[derive(Debug, Clone)]
pub enum InterpretError {
    Compile(ErrorMessage),
    Runtime(ErrorMessage),
    InstructionOutOfRange(usize),  // Position in instruction iterator
    ConstantOutOfRange(usize, u8), // Chunk position, constant position
}

impl std::error::Error for InterpretError {}

impl fmt::Display for InterpretError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            InterpretError::Compile(err) => write!(f, "Compile error during interpret: {}", err),
            InterpretError::Runtime(err) => write!(f, "Runtime error during interpret: {}", err),
            InterpretError::InstructionOutOfRange(chunk_pos) => {
                write!(
                    f,
                    "Instruction pointer out of range with index {}",
                    chunk_pos
                )
            }
            InterpretError::ConstantOutOfRange(chunk_pos, constant_pos) => write!(
                f,
                "Constant offset out of range with index {} in chunk index {}",
                constant_pos, chunk_pos
            ),
        }
    }
}

impl<'a> Vm<'a> {
    pub fn new(
        chunk_iter: std::slice::Iter<'a, Chunk>,
        instruction_iter: std::slice::Iter<'a, u8>,
    ) -> Self {
        Self {
            chunk_iter,
            instruction_iter,
        }
    }

    fn read_byte(&mut self) -> Result<u8, InterpretError> {
        match self.instruction_iter.nth(0) {
            Some(v) => Ok(*v),
            None => Err(InterpretError::InstructionOutOfRange(
                self.instruction_iter.clone().count(),
            )),
        }
    }

    /// Reads another byte from the bytecode input and uses it as an index into the constants table for the current chunk
    fn read_constant(&mut self) -> Result<Value, InterpretError> {
        let constant_index = self.read_byte()?;

        match self.chunk_iter.nth(0) {
            Some(chunk) => Ok(chunk.get_constant_value(constant_index as usize)),
            None => Err(InterpretError::ConstantOutOfRange(
                self.chunk_iter.clone().count(),
                constant_index,
            )),
        }
    }

    /// instruction pointer index - current code index
    fn cur_instruction_offset(&self) -> usize {
        self.instruction_iter.clone().count() - self.chunk_iter.clone().count()
    }

    fn latest_chunk(&self) -> Option<&Chunk> {
        self.chunk_iter.clone().take(1).next()
    }

    fn disassemble_latest_instruction(&self) -> String {
        if let Some(chunk) = self.latest_chunk() {
            let (debug_instruction, _) =
                debug::dissassemble_instruction(chunk, self.cur_instruction_offset());
            debug_instruction
        } else {
            String::from("End of chunks")
        }
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        loop {
            debug!("{}", self.disassemble_latest_instruction());

            let byte = self.read_byte()?;
            match OpCode::try_from(byte) {
                Ok(opcode) => match opcode {
                    OpCode::Return => {
                        return Ok(());
                    }
                    OpCode::Constant => {
                        let constant = self.read_constant()?;
                        println!("{}", constant);
                    }
                },
                Err(_) => {
                    return Err(InterpretError::Runtime(format!(
                        "Could not convert upcode from instruction {}",
                        byte
                    )));
                }
            };
        }
    }

    pub fn interpret(
        &mut self,
        chunk_iter: std::slice::Iter<'a, Chunk>,
    ) -> Result<(), InterpretError> {
        self.chunk_iter = chunk_iter;
        self.run()
    }
}

#[cfg(test)]
mod test {}
