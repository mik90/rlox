use crate::{
    chunk::{Chunk, OpCode},
    debug,
    value::{Value, ValueArray},
};
use std::fmt;

pub struct Vm<'a> {
    chunk_iter: std::slice::Iter<'a, Chunk>,
    instruction_iter: std::slice::Iter<'a, u8>,
    stack: ValueArray,
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
            stack: Vec::new(),
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

        match self.latest_chunk() {
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

    fn pop_from_stack(&mut self) -> Result<Value, InterpretError> {
        self.stack.pop().ok_or_else(|| {
            InterpretError::Runtime(format!(
                "Could not pop value off stack since there were no values there. Chunk index: {}",
                self.chunk_iter.clone().count()
            ))
        })
    }

    fn pop_pair_from_stack(&mut self) -> Result<(Value, Value), InterpretError> {
        let rhs = self.pop_from_stack()?;
        let lhs = self.pop_from_stack()?;
        Ok((lhs, rhs))
    }

    fn dump_stack(&self) -> String {
        let mut out = String::from("        | ");
        for value in &self.stack {
            out.push_str(format!("[ {} ]", *value).as_str());
        }
        out.push('\n');
        out
    }

    fn disassemble_latest_instruction(&self) -> String {
        if let Some(chunk) = self.latest_chunk() {
            let (debug_instruction, _) =
                debug::dissassemble_instruction(chunk, self.cur_instruction_offset());
            debug_instruction
        } else {
            String::from("End of chunks\n")
        }
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        loop {
            debug!("stack: {}", self.dump_stack());
            debug!("instr: {}", self.disassemble_latest_instruction());

            let byte = self.read_byte()?;
            match OpCode::try_from(byte) {
                Ok(opcode) => match opcode {
                    OpCode::Constant => {
                        let constant = self.read_constant()?;
                        debug!("Read constant {}\n", constant);
                        self.stack.push(constant);
                    }
                    OpCode::Add => {
                        let (lhs, rhs) = self.pop_pair_from_stack()?;
                        self.stack.push(lhs + rhs);
                    }
                    OpCode::Subtract => {
                        let (lhs, rhs) = self.pop_pair_from_stack()?;
                        self.stack.push(lhs - rhs);
                    }
                    OpCode::Multiply => {
                        let (lhs, rhs) = self.pop_pair_from_stack()?;
                        self.stack.push(lhs * rhs);
                    }
                    OpCode::Divide => {
                        let (lhs, rhs) = self.pop_pair_from_stack()?;
                        self.stack.push(lhs / rhs);
                    }
                    OpCode::Negate => {
                        let value = self.pop_from_stack()?;
                        self.stack.push(-value);
                    }
                    OpCode::Return => {
                        if let Some(v) = self.stack.pop() {
                            println!("{}", v);
                        }
                        return Ok(());
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
mod test {
    use super::*;
    #[test]
    fn test_example_eval() {
        let mut chunks = vec![];
        {
            let mut chunk = Chunk::new();
            let constant = chunk.add_constant(1.2);
            chunk.write_opcode(OpCode::Constant, 123);
            chunk.write_byte(constant as u8, 123);
            chunk.write_opcode(OpCode::Negate, 123);

            chunk.write_opcode(OpCode::Return, 123);
            chunks.push(chunk);
        }
        let mut vm = Vm::new(chunks.iter(), chunks[0].code_iter());
        let res = vm.interpret(chunks.iter());
        assert!(res.is_ok(), "{}", res.unwrap_err());
    }

    #[test]
    fn test_negation_eval() {
        let mut chunks = vec![];
        {
            let mut chunk = Chunk::new();
            let constant = chunk.add_constant(1.2);
            chunk.write_opcode(OpCode::Constant, 123);
            chunk.write_byte(constant as u8, 123);
            chunk.write_opcode(OpCode::Negate, 123);

            chunks.push(chunk);
        }
        let mut vm = Vm::new(chunks.iter(), chunks[0].code_iter());
        let res = vm.interpret(chunks.iter());

        match res.unwrap_err() {
            // Without a return, we expect to hit the end of execution ungracefully and without cleaning up the stack
            InterpretError::InstructionOutOfRange(_) => (),
            err => assert!(false, "{}", err),
        }

        let value = vm.stack.last();
        assert!(value.is_some());
        assert_eq!(*value.unwrap(), -1.2);
    }

    #[test]
    fn test_subtraction_eval() {
        let mut chunks = vec![];
        {
            let mut chunk = Chunk::new();
            {
                let constant = chunk.add_constant(3.0);
                chunk.write_opcode(OpCode::Constant, 123);
                chunk.write_byte(constant as u8, 123);
                assert_eq!(constant, 0);
            }

            {
                let constant = chunk.add_constant(1.0);
                chunk.write_opcode(OpCode::Constant, 123);
                chunk.write_byte(constant as u8, 123);
                assert_eq!(constant, 1);
            }

            chunk.write_opcode(OpCode::Subtract, 123);

            chunks.push(chunk);
        }
        let mut vm = Vm::new(chunks.iter(), chunks[0].code_iter());
        let res = vm.interpret(chunks.iter());

        match res.unwrap_err() {
            // Without a return, we expect to hit the end of execution ungracefully and without cleaning up the stack
            InterpretError::InstructionOutOfRange(_) => (),
            err => assert!(false, "{}", err),
        }

        let value = vm.stack.last();
        assert!(value.is_some());
        assert_eq!(*value.unwrap(), 2.0);
    }
}
