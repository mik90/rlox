use crate::{
    chunk::{debug, Chunk, OpCode},
    compiler::{Compiler, CompilerError},
    debug, debugln,
    value::{Value, ValueArray},
};
use std::{fmt, iter::Enumerate};

pub struct Vm {}

#[derive(Debug)]
pub struct VmState<'a> {
    pub source: String,
    chunks: Vec<Chunk>,
    chunk_iter: Enumerate<std::slice::Iter<'a, Chunk>>,
    instruction_iter: Enumerate<std::slice::Iter<'a, u8>>,
    stack: ValueArray,
}

pub type ErrorMessage = String;

#[derive(Debug, Clone)]
pub enum InterpretError {
    Compile(CompilerError),
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
// Only used to allow the Vm to init. Easier than having an optional iterator
const DEFAULT_INSTRUCTION_SLICE: &'static [u8] = &[];
const DEFAULT_CHUNK_SLICE: &'static [Chunk] = &[];

impl<'a> VmState<'a> {
    pub fn new_uninit() -> VmState<'a> {
        VmState {
            source: "".to_owned(),
            chunks: vec![],
            chunk_iter: DEFAULT_CHUNK_SLICE.iter().enumerate(),
            instruction_iter: DEFAULT_INSTRUCTION_SLICE.iter().enumerate(),
            stack: vec![],
        }
    }

    fn new(source: String) -> VmState<'a> {
        let mut s = VmState {
            source,
            chunks: vec![],
            chunk_iter: DEFAULT_CHUNK_SLICE.iter().enumerate(),
            instruction_iter: DEFAULT_INSTRUCTION_SLICE.iter().enumerate(),
            stack: vec![],
        };
        s.chunk_iter = s.chunks.iter().enumerate();
        s
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
}

impl Vm {
    pub fn new() -> Self {
        Self {}
    }

    /// Disassembles a single instructions and returns whether or not it should continue running
    fn run_once<'a>(
        &'a self,
        mut state: VmState<'a>,
    ) -> Result<(bool, VmState<'a>), InterpretError> {
        debugln!("---------------------------------");
        //debugln!("stack data  : {}", self.dump_stack());
        debug!("{}", state.disassemble_latest_instruction()?);

        let byte = state.read_byte()?;
        match OpCode::try_from(byte) {
            Ok(opcode) => match opcode {
                OpCode::Constant => {
                    let constant = state.read_constant()?;
                    state.stack.push(constant);
                }
                OpCode::Add => {
                    let (lhs, rhs) = state.pop_pair_from_stack()?;
                    state.stack.push(lhs + rhs);
                }
                OpCode::Subtract => {
                    let (lhs, rhs) = state.pop_pair_from_stack()?;
                    state.stack.push(lhs - rhs);
                }
                OpCode::Multiply => {
                    let (lhs, rhs) = state.pop_pair_from_stack()?;
                    state.stack.push(lhs * rhs);
                }
                OpCode::Divide => {
                    let (lhs, rhs) = state.pop_pair_from_stack()?;
                    state.stack.push(lhs / rhs);
                }
                OpCode::Negate => {
                    let value = state.pop_from_stack()?;
                    state.stack.push(-value);
                }
                OpCode::Return => {
                    if let Some(v) = state.stack.pop() {
                        println!("{}", v);
                    }
                    return Ok((false, state));
                }
            },
            Err(_) => {
                return Err(InterpretError::Runtime(format!(
                    "Could not convert upcode from instruction {}",
                    byte
                )));
            }
        };
        Ok((true, state))
    }

    pub fn interpret<'a>(&'a self, mut state: VmState<'a>) -> Result<VmState<'a>, InterpretError> {
        let mut compiler = Compiler::new();
        let chunk = compiler
            .compile(&state.source)
            .map_err(InterpretError::Compile)?;
        state.chunks.push(chunk);

        loop {
            let (continue_running, new_state) = self.run_once(state)?;
            state = new_state;
            // Continue running while we're able to
            if !continue_running {
                // Non-error exit is fine
                return Ok(state);
            }
        }
    }

    #[cfg(test)]
    fn build_state<'a>(&self, chunks: Vec<Chunk>) -> Result<VmState<'a>, InterpretError> {
        let chunk_iter = chunks.iter().enumerate();
        let instruction_iter = chunks
            .first()
            .ok_or_else(|| InterpretError::Runtime(format!("No chunks were available in slice")))?
            .code_iter()
            .enumerate();
        let mut state = VmState::new("".to_owned());
        state.chunk_iter = chunk_iter;
        state.instruction_iter = instruction_iter;
        Ok(state)
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
        let vm = Vm::new();

        let state = vm.build_state(chunks);
        assert!(state.is_ok(), "{}", state.unwrap_err());
        let state = state.unwrap();

        // Interprets add constant
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        // Interprets negate
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        // Interprets return
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);
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
        let vm = Vm::new();

        let state = vm.build_state(chunks);
        assert!(state.is_ok(), "{}", state.unwrap_err());
        let state = state.unwrap();

        // Interprets add constant
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        // Interprets negate
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        let value = state.stack.last();
        assert!(value.is_some());
        assert_eq!(*value.unwrap(), -1.2);

        // post-negate
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
        match res.unwrap_err() {
            // Without a return, we expect to hit the end of execution ungracefully and without cleaning up the stack
            InterpretError::InstructionOutOfRange(_) => (),
            err => assert!(false, "{}", err),
        }
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
        let vm = Vm::new();

        let state = vm.build_state(chunks);
        assert!(state.is_ok(), "{}", state.unwrap_err());
        let state = state.unwrap();

        // Interprets add constant
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        // Interprets add constant
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        // Interprets substract
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        let value = state.stack.last();
        assert!(value.is_some());
        assert_eq!(*value.unwrap(), 2.0);
    }

    #[test]
    fn test_operation_order() {
        let mut chunks = vec![];
        {
            let mut chunk = Chunk::new();
            chunk.write_constant(3.0, 123);
            chunk.write_constant(1.0, 123);
            chunks.push(chunk);
        }
        let vm = Vm::new();

        let state = vm.build_state(chunks);
        assert!(state.is_ok(), "{}", state.unwrap_err());
        let state = state.unwrap();

        // add constant
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        // add constant
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        let chunk = state.peek_latest_chunk();
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
