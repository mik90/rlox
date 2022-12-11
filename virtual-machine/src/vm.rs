use crate::{
    chunk::{debug, Chunk, OpCode},
    compiler::{Compiler, CompilerError},
    debug, debugln, herefmt,
    value::{Value, ValueArray},
};
use std::fmt;

pub struct Vm {}

#[derive(Debug)]
pub struct VmState {
    chunks: Vec<Chunk>,
    chunk_index: usize,       //< idx into chunks
    instruction_index: usize, //< idx into the current chunk's instructions
    stack: ValueArray,
}

pub type ErrorMessage = String;

#[derive(Debug, Clone)]
pub enum InterpretError {
    Compile(CompilerError),
    Runtime(ErrorMessage),
    InstructionOutOfRange(&'static str, u32, usize), // File, line, Position in instruction iterator
    ConstantOutOfRange(&'static str, u32, usize, u8), // File, line, Chunk position, constant position
}

impl std::error::Error for InterpretError {}

impl fmt::Display for InterpretError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            InterpretError::Compile(err) => write!(f, "Compile error during interpret: {}", err),
            InterpretError::Runtime(err) => write!(f, "Runtime error during interpret: {}", err),
            InterpretError::InstructionOutOfRange(file, line, chunk_pos) => {
                write!(
                    f,
                    "[{}:{}] Instruction pointer out of range with index {}",
                    file, line, chunk_pos
                )
            }
            InterpretError::ConstantOutOfRange(file, line, chunk_pos, constant_pos) => write!(
                f,
                "[{}:{}] Constant offset out of range with index {} in chunk index {}",
                file, line, constant_pos, chunk_pos
            ),
        }
    }
}

impl VmState {
    pub fn new() -> VmState {
        VmState {
            chunks: vec![],
            chunk_index: 0,
            instruction_index: 0,
            stack: vec![],
        }
    }

    fn end_of_instructions(&self) -> Result<bool, InterpretError> {
        Ok(self.instruction_index >= self.peek_instructions()?.len())
    }

    fn read_byte(&mut self) -> Result<u8, InterpretError> {
        let byte = *self
            .peek_instructions()?
            .get(self.instruction_index)
            .ok_or(InterpretError::InstructionOutOfRange(
                file!(),
                line!(),
                self.instruction_index,
            ))?;

        self.instruction_index += 1;
        Ok(byte)
    }

    /// Reads another byte from the bytecode input and uses it as an index into the constants table for the current chunk
    fn read_constant(&mut self) -> Result<Value, InterpretError> {
        let constant_index = self.read_byte()?;

        match self.peek_latest_chunk() {
            Ok(chunk) => match chunk.get_constant_value(constant_index as usize) {
                Some(v) => Ok(v.clone()),
                None => Err(InterpretError::ConstantOutOfRange(
                    file!(),
                    line!(),
                    self.chunk_index,
                    constant_index,
                )),
            },
            Err(e) => Err(e),
        }
    }

    fn peek_latest_chunk(&self) -> Result<&Chunk, InterpretError> {
        self.chunks.get(self.chunk_index).ok_or_else(|| {
            InterpretError::Runtime(herefmt!(
                "Chunk index '{}' is out of range. Only {} chunks exist",
                self.chunk_index,
                self.chunks.len()
            ))
        })
    }

    fn peek_instructions(&self) -> Result<&[u8], InterpretError> {
        let instructions = self.peek_latest_chunk()?.code_iter().as_slice();
        Ok(instructions)
    }

    fn pop_from_stack(&mut self) -> Result<Value, InterpretError> {
        self.stack.pop().ok_or_else(|| {
            InterpretError::Runtime(herefmt!(
                "Could not pop value off stack since there were no values there. Chunk index: {}",
                self.chunk_index
            ))
        })
    }

    fn peek_on_stack(&self, distance: usize) -> Result<&Value, InterpretError> {
        self.stack.iter().rev().nth(distance).ok_or(InterpretError::Runtime(
            herefmt!("Tried to get Value {} elements from top of stack but the stack was only {} elements deep", distance, self.stack.len())))
    }

    /// Pops pair of same type from the stack
    fn pop_pair_from_stack(&mut self) -> Result<(Value, Value), InterpretError> {
        let rhs = self.pop_from_stack()?;
        let lhs = self.pop_from_stack()?;
        Ok((lhs, rhs))
    }

    #[allow(dead_code)]
    /// Only used for debugging
    fn dump_stack(&self) -> String {
        let mut out = String::from(" ");
        for value in &self.stack {
            out.push_str(format!("[ {} ]", *value).as_str());
        }
        out
    }

    fn disassemble_latest_instruction(&self) -> String {
        if let Ok(chunk) = self.peek_latest_chunk() {
            let instruction_len = chunk.code_iter().clone().count();
            if self.instruction_index >= instruction_len {
                return format!(
                    "No instructions left. instruction_index={}, instruction.len()={}\n",
                    self.instruction_index, instruction_len
                );
            } else {
                let (debug_instruction, _) =
                    debug::dissassemble_instruction(chunk, self.instruction_index);
                return debug_instruction;
            }
        }

        format!(
            "No chunks left. chunk_index={}, chunk.len()={}",
            self.chunk_index,
            self.chunks.len(),
        )
    }

    fn reset_stack(&mut self) {
        self.stack.clear()
    }

    pub fn runtime_error(&self, message: String) -> InterpretError {
        let instruction_index = self.instruction_index;

        let script_info = if let Ok(chunk) = self.peek_latest_chunk() {
            // Index - 1 since the interpreter has moved past the failed instruction and we need to go back to check it
            let line = chunk.line_at(instruction_index - 1);
            format!("[line {}] in script", line)
        } else {
            String::from("[unknown line/chunk] in script")
        };
        // TODO the book has me resetting the stack before this exits, unsure why
        //self.reset_stack();
        InterpretError::Runtime(format!("{}\n{}", message, script_info))
    }
}

impl Vm {
    pub fn new() -> Self {
        Self {}
    }

    /// Disassembles a single instructions and returns whether or not it should continue running
    fn run_once(&self, mut state: VmState) -> Result<(bool, VmState), InterpretError> {
        debugln!("---------------------------------");
        //debugln!("stack data  : {}", self.dump_stack());
        debug!("{}", state.disassemble_latest_instruction());
        if state.end_of_instructions()? {
            return Ok((false, state));
        }

        let byte = state.read_byte()?;
        match OpCode::try_from(byte) {
            Ok(opcode) => match opcode {
                OpCode::Constant => {
                    let constant = state.read_constant()?;
                    state.stack.push(constant);
                }
                OpCode::Add => match state.pop_pair_from_stack()? {
                    (Value::Number(lhs), Value::Number(rhs)) => {
                        state.stack.push(Value::Number(lhs + rhs))
                    }
                    (lhs, rhs) => {
                        let err = state.runtime_error(herefmt!(
                            "Operands must be numbers but were lhs={:?} and rhs={:?}",
                            lhs,
                            rhs
                        ));
                        state.stack.push(rhs);
                        state.stack.push(lhs);
                        return Err(err);
                    }
                },
                OpCode::Subtract => match state.pop_pair_from_stack()? {
                    (Value::Number(lhs), Value::Number(rhs)) => {
                        state.stack.push(Value::Number(lhs - rhs));
                    }
                    (lhs, rhs) => {
                        let err = state.runtime_error(herefmt!(
                            "Operands must be numbers but were lhs={:?} and rhs={:?}",
                            lhs,
                            rhs
                        ));
                        state.stack.push(rhs);
                        state.stack.push(lhs);
                        return Err(err);
                    }
                },
                OpCode::Multiply => match state.pop_pair_from_stack()? {
                    (Value::Number(lhs), Value::Number(rhs)) => {
                        state.stack.push(Value::Number(lhs * rhs))
                    }
                    (lhs, rhs) => {
                        let err = state.runtime_error(herefmt!(
                            "Operands must be numbers but were lhs={:?} and rhs={:?}",
                            lhs,
                            rhs
                        ));
                        state.stack.push(rhs.clone());
                        state.stack.push(lhs.clone());
                        return Err(err);
                    }
                },
                OpCode::Divide => match state.pop_pair_from_stack()? {
                    (Value::Number(lhs), Value::Number(rhs)) => {
                        state.stack.push(Value::Number(lhs / rhs))
                    }
                    (lhs, rhs) => {
                        let err = state.runtime_error(herefmt!(
                            "Operands must be numbers but were lhs={:?} and rhs={:?}",
                            lhs,
                            rhs
                        ));
                        state.stack.push(rhs);
                        state.stack.push(lhs);
                        return Err(err);
                    }
                },
                OpCode::Negate => {
                    match state.pop_from_stack()? {
                        Value::Number(value) => state.stack.push(Value::Number(-value)),
                        other => {
                            // put it back on the stack since it wasn't what we expected
                            state.stack.push(other);
                            return Err(state.runtime_error(herefmt!(
                                "Operand to unary negation must be a number"
                            )));
                        }
                    };
                }
                OpCode::Return => {
                    if let Some(v) = state.stack.pop() {
                        println!("Returning {}", v);
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

    pub fn interpret(&self, source: &str, mut state: VmState) -> Result<VmState, InterpretError> {
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(source).map_err(InterpretError::Compile)?;
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
}

#[cfg(test)]
mod test {
    use super::*;

    fn build_state(chunks: Vec<Chunk>) -> VmState {
        let mut state = VmState::new();
        state.chunks = chunks;
        state
    }

    #[test]
    fn test_example_eval() {
        let mut chunks = vec![];
        {
            let mut chunk = Chunk::new();

            assert!(chunk.write_constant(Value::Number(1.2), 123).is_ok());

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

        let state = build_state(chunks);

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

        // Interprets return, should stop running after this
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let (continue_running, _) = res.unwrap();
        assert!(!continue_running);
    }

    #[test]
    fn test_negation_eval() {
        let mut chunks = vec![];
        {
            let mut chunk = Chunk::new();

            assert!(chunk.write_constant(Value::Number(1.2), 123).is_ok());

            chunk.write_opcode(OpCode::Negate, 123);

            chunks.push(chunk);
        }
        let vm = Vm::new();

        let state = build_state(chunks);

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
        assert_eq!(*value.unwrap(), Value::Number(-1.2));

        // post-negate
        // A lack of instructions is not an error
        let res = vm.run_once(state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
    }

    #[test]
    fn test_subtraction_eval() {
        let mut chunks = vec![];
        {
            let mut chunk = Chunk::new();
            chunk.write_constant(Value::Number(3.0), 123);
            chunk.write_constant(Value::Number(1.0), 123);
            chunk.write_opcode(OpCode::Subtract, 123);

            chunks.push(chunk);
        }
        let vm = Vm::new();

        let state = build_state(chunks);

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
        assert_eq!(*value.unwrap(), Value::Number(2.0));
    }

    #[test]
    fn test_operation_order() {
        let mut chunks = vec![];
        {
            let mut chunk = Chunk::new();
            let res = chunk.write_constant(Value::Number(3.0), 123);
            assert!(res.is_ok(), "{}", res.unwrap_err());
            let res = chunk.write_constant(Value::Number(1.0), 123);
            assert!(res.is_ok(), "{}", res.unwrap_err());
            chunks.push(chunk);
        }
        let vm = Vm::new();

        let state = build_state(chunks);

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
        assert!(chunk.is_ok());
        let chunk = chunk.unwrap();
        let value = chunk.get_constant_value(0);
        assert!(value.is_some());
        assert_eq!(*value.unwrap(), Value::Number(3.0));

        let value = chunk.get_constant_value(1);
        assert!(value.is_some());
        assert_eq!(*value.unwrap(), Value::Number(1.0));
    }

    #[test]
    fn interpret() {
        let vm = Vm::new();
        let state = VmState::new();
        let res = vm.interpret("1 + 2\0", state);
        assert!(res.is_ok(), "{}", res.unwrap_err());
    }
}
