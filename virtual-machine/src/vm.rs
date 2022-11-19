use crate::{
    chunk::{Chunk, OpCode},
    debug, debugln,
    value::{Value, ValueArray},
};
use std::{fmt, iter::Enumerate};

pub struct Vm<'a> {
    chunk_iter: Enumerate<std::slice::Iter<'a, Chunk>>,
    instruction_iter: Enumerate<std::slice::Iter<'a, u8>>,
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
        chunk_iter: Enumerate<std::slice::Iter<'a, Chunk>>,
        instruction_iter: Enumerate<std::slice::Iter<'a, u8>>,
    ) -> Self {
        Self {
            chunk_iter,
            instruction_iter,
            stack: Vec::new(),
        }
    }

    fn read_byte(&mut self) -> Result<u8, InterpretError> {
        match self.instruction_iter.nth(0) {
            Some((_, byte)) => Ok(*byte),
            None => Err(InterpretError::InstructionOutOfRange(
                self.instruction_index(),
            )),
        }
    }

    /// Reads another byte from the bytecode input and uses it as an index into the constants table for the current chunk
    fn read_constant(&mut self) -> Result<Value, InterpretError> {
        let constant_index = self.read_byte()?;

        match self.peek_latest_chunk() {
            Some(chunk) => match chunk.get_constant_value(constant_index as usize) {
                Some(v) => Ok(*v),
                None => Err(InterpretError::ConstantOutOfRange(
                    self.chunk_index(),
                    constant_index,
                )),
            },
            None => Err(InterpretError::ConstantOutOfRange(
                self.chunk_index(),
                constant_index,
            )),
        }
    }

    fn peek_latest_chunk(&self) -> Option<&Chunk> {
        self.chunk_iter
            .clone()
            .peekable()
            .peek()
            .map(|(_, chunk)| *chunk)
    }

    fn chunk_index(&self) -> usize {
        self.chunk_iter
            .clone()
            .peekable()
            .clone()
            .peek()
            .map_or(0, |(i, _)| *i)
    }

    fn instruction_index(&self) -> usize {
        self.instruction_iter
            .clone()
            .peekable()
            .peek()
            .map_or(0, |(i, _)| *i)
    }

    fn pop_from_stack(&mut self) -> Result<Value, InterpretError> {
        self.stack.pop().ok_or_else(|| {
            InterpretError::Runtime(format!(
                "Could not pop value off stack since there were no values there. Chunk index: {}",
                self.chunk_index()
            ))
        })
    }

    fn pop_pair_from_stack(&mut self) -> Result<(Value, Value), InterpretError> {
        let rhs = self.pop_from_stack()?;
        let lhs = self.pop_from_stack()?;
        Ok((lhs, rhs))
    }

    fn dump_stack(&self) -> String {
        let mut out = String::from(" ");
        for value in &self.stack {
            out.push_str(format!("[ {} ]", *value).as_str());
        }
        out
    }

    fn disassemble_latest_instruction(&self) -> Result<String, InterpretError> {
        if let Some(chunk) = self.peek_latest_chunk() {
            let (debug_instruction, _) =
                debug::dissassemble_instruction(chunk, self.instruction_index());
            Ok(debug_instruction)
        } else {
            Ok(String::from("No chunks left\n"))
        }
    }

    /// Disassembles a single instructions and returns whether or not it should continue running
    fn run_once(&mut self) -> Result<bool, InterpretError> {
        debugln!("---------------------------------");
        //debugln!("stack data  : {}", self.dump_stack());
        debug!("{}", self.disassemble_latest_instruction()?);

        let byte = self.read_byte()?;
        match OpCode::try_from(byte) {
            Ok(opcode) => match opcode {
                OpCode::Constant => {
                    let constant = self.read_constant()?;
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
                    return Ok(false);
                }
            },
            Err(_) => {
                return Err(InterpretError::Runtime(format!(
                    "Could not convert upcode from instruction {}",
                    byte
                )));
            }
        };
        Ok(true)
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        while self.run_once()? {
            // Continue running while we're able to
        }
        // Non-error exit is fine
        Ok(())
    }

    pub fn interpret(
        &mut self,
        chunk_iter: Enumerate<std::slice::Iter<'a, Chunk>>,
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
        println!(
            "OpCodes (u8): {:?}",
            chunks[0].code_iter().collect::<Vec<&u8>>()
        );
        println!(
            "Constants : {:?}",
            chunks[0].constant_iter().collect::<Vec<&Value>>()
        );
        let mut vm = Vm::new(chunks.iter().enumerate(), chunks[0].code_iter().enumerate());

        // Interprets add constant
        let res = vm.run_once();
        assert!(res.is_ok());
        assert!(res.unwrap());

        // Interprets negate
        let res = vm.run_once();
        assert!(res.is_ok());
        assert!(res.unwrap());

        // Interprets return
        let res = vm.run_once();
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), false);
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
        let mut vm = Vm::new(chunks.iter().enumerate(), chunks[0].code_iter().enumerate());
        let res = vm.interpret(chunks.iter().enumerate());

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
        let mut vm = Vm::new(chunks.iter().enumerate(), chunks[0].code_iter().enumerate());

        // Interprets add constant
        let res = vm.run_once();
        assert!(res.is_ok());
        assert!(res.unwrap());

        // Interprets add constant
        let res = vm.run_once();
        assert!(res.is_ok());
        assert!(res.unwrap());

        // Interprets substract
        let res = vm.run_once();
        assert!(res.is_ok());
        assert!(res.unwrap());

        let value = vm.stack.last();
        assert!(value.is_some());
        assert_eq!(*value.unwrap(), 2.0);
    }

    #[test]
    fn test_operation_order() {
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

            chunks.push(chunk);
        }
        let mut vm = Vm::new(chunks.iter().enumerate(), chunks[0].code_iter().enumerate());

        let res = vm.run_once();
        assert!(res.is_ok());
        assert!(res.unwrap());
        let res = vm.run_once();
        assert!(res.is_ok());
        assert!(res.unwrap());

        let chunk = vm.peek_latest_chunk();
        assert!(chunk.is_some());
        let chunk = chunk.unwrap();
        let value = chunk.get_constant_value(0);
        assert!(value.is_some());
        assert_eq!(*value.unwrap(), 3.0);

        let value = chunk.get_constant_value(1);
        assert!(value.is_some());
        assert_eq!(*value.unwrap(), 1.0);
    }
}
