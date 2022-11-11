use crate::chunk::{Chunk, OpCode};
use std::fmt;

pub struct Vm<'a> {
    // TODO Use generics to store these iterators
    chunk_iter: std::slice::Iter<'a, Chunk>,
    instruction_iter: std::slice::Iter<'a, u8>,
}

pub type ErrorMessage = String;

#[derive(Debug, Clone)]
pub enum InterpretError {
    Compile,
    Runtime(ErrorMessage),
    EndOfProgram, // No more chunks to read
}

impl std::error::Error for InterpretError {}

impl fmt::Display for InterpretError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            InterpretError::Compile => write!(f, "Compile error during interpret"),
            InterpretError::Runtime(err) => write!(f, "Runtime error during interpret: {}", err),
            InterpretError::EndOfProgram => write!(f, "Hit end of program"),
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

    fn run(&mut self) -> Result<(), InterpretError> {
        loop {
            match self.instruction_iter.nth(0) {
                Some(instruction) => match OpCode::try_from(*instruction) {
                    Ok(opcode) => match opcode {
                        OpCode::Return => {
                            return Ok(());
                        }
                        OpCode::Constant => {
                            todo!();
                        }
                    },
                    Err(_) => {
                        return Err(InterpretError::Runtime(format!(
                            "Could not convert upcode from instruction {}",
                            instruction
                        )));
                    }
                },
                None => {
                    return Err(InterpretError::EndOfProgram);
                }
            }
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
