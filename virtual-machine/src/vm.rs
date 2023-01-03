use crate::{
    chunk::{debug, Chunk, OpCode},
    compiler::{Compiler, CompilerError},
    debug, debugln, herefmt,
    value::{Obj, Value},
};
use std::{collections::HashMap, fmt};

pub struct Vm {}

#[derive(Clone, Debug)]
pub struct VmState {
    chunks: Vec<Chunk>,
    chunk_index: usize,       //< idx into chunks
    instruction_index: usize, //< idx into the current chunk's instructions
    stack: Vec<Value>,
    // Normally, the linked list of Objects would be stored here but I have all thle objects in stored as Arc<Mutex<T>>
    // Maybe I should use unsafe so I could write a garbage collector?
    globals: HashMap<String, Value>, //< Global hash map of values, no string interning :(
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
            globals: HashMap::new(),
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

    fn read_short(&mut self) -> Result<u16, InterpretError> {
        // First byte is the upper, second is the lower
        let upper = (self.read_byte()? as u16) << 8;
        let lower = self.read_byte()? as u16;
        let short = lower | upper;

        Ok(short)
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

    // TODO Technically this could be a borrow but im unsure how i'd handle that
    // Maybe the string type should just be an Rc<String>?
    fn read_string_constant(&mut self) -> Result<String, InterpretError> {
        match self.read_constant()? {
            Value::Obj(rc_obj) => {
                let obj: &Obj = &rc_obj;
                match obj {
                    Obj::String(name) => {
                        return Ok(name.clone());
                    }
                    _ => {
                        return Err(self.runtime_error(herefmt!(
                            "Expected to get an Obj::String from read_constant but got {:?}",
                            obj
                        )));
                    }
                }
            }
            other => {
                return Err(self.runtime_error(herefmt!(
                    "Expected to get an Obj from read_constant but got {:?}",
                    other
                )));
            }
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
        debugln!("--------------------------------------------------------");
        debugln!("run_once()");
        debugln!("stack data: {}", state.dump_stack());
        debug!("{}", state.disassemble_latest_instruction());
        if state.end_of_instructions()? {
            return Ok((false, state));
        }

        let byte = state.read_byte()?;
        match OpCode::try_from(byte) {
            Ok(opcode) => {
                match opcode {
                    OpCode::Constant => {
                        let constant = state.read_constant()?;
                        state.stack.push(constant);
                    }
                    OpCode::Nil => {
                        state.stack.push(Value::Nil);
                    }
                    OpCode::True => {
                        state.stack.push(Value::Bool(true));
                    }
                    OpCode::False => {
                        state.stack.push(Value::Bool(false));
                    }
                    OpCode::Equal => {
                        let (lhs, rhs) = state.pop_pair_from_stack()?;
                        state.stack.push(Value::Bool(lhs == rhs));
                    }
                    OpCode::Pop => {
                        state.stack.pop();
                    }
                    OpCode::GetLocal => {
                        let slot = state.read_byte()?;
                        // Push value to the top of the stack so it can be used
                        // I can't tell if the book is doing a copy or using a pointer here, i'll just defer to copy
                        let value = state.stack[slot as usize].clone();
                        //debugln!("Got local with slot '{}' and value '{}'", slot, value);
                        state.stack.push(value);
                    }
                    OpCode::SetLocal => {
                        let slot = state.read_byte()? as usize;
                        let value = state.peek_on_stack(0)?;
                        let top_of_stack_index = state.stack.len() - 1;
                        //debugln!(
                        //    "SetLocal wiht slot={slot}, value={value}, stack_height={top_of_stack_index}\nstack: {}", state.dump_stack()
                        //);

                        if slot == top_of_stack_index + 1 {
                            state.stack.push(value.clone());
                        } else if slot <= top_of_stack_index {
                            state.stack[slot] = value.clone();
                        } else {
                            return Err(InterpretError::Runtime(herefmt!(
                                "Could not set local with slot {} in stack of height {}",
                                slot,
                                top_of_stack_index
                            )));
                        }
                    }
                    OpCode::GetGlobal => {
                        let identifier = state.read_string_constant()?;

                        match state.globals.get(&identifier) {
                            Some(value) => {
                                // Copies from the global table onto the stack
                                state.stack.push(value.clone());
                            }
                            None => {
                                return Err(state.runtime_error(herefmt!(
                                    "Undefined variable '{}'",
                                    identifier,
                                )));
                            }
                        }
                    }
                    OpCode::SetGlobal => {
                        let identifier = state.read_string_constant()?;
                        let value = state.peek_on_stack(0)?.clone();
                        //debugln!("SetGlobal '{}' to {}", identifier, value);

                        match state.globals.get_mut(&identifier) {
                            Some(old_value) => {
                                *old_value = value;
                            }
                            None => {
                                // We tried to set a value that didn't exist
                                return Err(state.runtime_error(herefmt!(
                                    "Undefined variable '{}'",
                                    identifier,
                                )));
                            }
                        }
                    }
                    OpCode::DefineGlobal => {
                        let identifier = state.read_string_constant()?;
                        let value = state.peek_on_stack(0)?;
                        // Note, the book stores value pointers here while i store copies
                        state.globals.insert(identifier, value.clone());
                        state.stack.pop();
                    }
                    OpCode::Greater => {
                        let (lhs, rhs) = state.pop_pair_from_stack()?;
                        state.stack.push(Value::Bool(lhs > rhs));
                    }
                    OpCode::Less => {
                        let (lhs, rhs) = state.pop_pair_from_stack()?;
                        state.stack.push(Value::Bool(lhs < rhs));
                    }
                    OpCode::Add => match state.pop_pair_from_stack()? {
                        (Value::Obj(lhs), Value::Obj(rhs)) => {
                            let lhs: &Obj = &lhs;
                            let rhs: &Obj = &rhs;
                            match (lhs, rhs) {
                                (Obj::String(lhs), Obj::String(rhs)) => {
                                    let mut combined_string = lhs.clone();
                                    combined_string.push_str(rhs);
                                    let value = Value::from(Obj::String(combined_string));
                                    state.stack.push(value);
                                }
                            }
                        }
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            state.stack.push(Value::Number(lhs + rhs))
                        }
                        (lhs, rhs) => {
                            let err = state.runtime_error(herefmt!(
                                "Operands must be two numbers or strings but were lhs={:?} and rhs={:?}",
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
                                &lhs,
                                &rhs
                            ));
                            state.stack.push(rhs);
                            state.stack.push(lhs);
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
                    OpCode::Not => {
                        let value = state.pop_from_stack()?;
                        state.stack.push(Value::Bool(value.falsey()));
                    }
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
                    OpCode::Print => {
                        if let Some(value) = state.stack.pop() {
                            println!("{}", value)
                        } else {
                            return Err(InterpretError::Runtime(herefmt!(
                                "Could not print value since the stack was empty"
                            )));
                        }
                    }
                    OpCode::Jump => {
                        let offset = state.read_short()?;
                        state.instruction_index += offset as usize;
                    }
                    OpCode::JumpIfFalse => {
                        debugln!(
                            "JumpIfFalse start: instruction index is {}",
                            state.instruction_index
                        );
                        let offset = state.read_short()?;
                        let top_value = state.peek_on_stack(0)?;
                        if top_value.falsey() {
                            debugln!("Incrementing instruction index by {}", offset);
                            state.instruction_index += offset as usize;
                        }
                        debugln!(
                            "JumpIfFalse end: Instruction index is {}",
                            state.instruction_index
                        );
                    }
                    OpCode::Loop => {
                        let offset = state.read_short()?;
                        state.instruction_index -= offset as usize;
                    }
                    OpCode::Return => {
                        return Ok((false, state));
                    }
                }
            }
            Err(e) => {
                return Err(InterpretError::Runtime(e));
            }
        };
        Ok((true, state))
    }

    pub fn interpret(&self, source: &str, mut state: VmState) -> Result<VmState, InterpretError> {
        // TODO use same stateful compiler over iteration
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(source).map_err(InterpretError::Compile)?;
        state.chunks.push(chunk);
        // Update indexes of state to match the new generated chunk
        state.chunk_index = state.chunks.len() - 1;
        state.instruction_index = 0;

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

    fn build_state_from_source(source: &str) -> VmState {
        let mut state = VmState::new();
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(source);
        assert!(chunk.is_ok(), "{}", chunk.unwrap_err());
        state.chunks = vec![chunk.unwrap()];
        state
    }

    fn get_current_opcode_from_state(state: &VmState) -> OpCode {
        let current_instruction_byte =
            state.chunks[state.chunk_index].byte_at(state.instruction_index);
        let current_opcode = OpCode::try_from(current_instruction_byte);

        assert!(current_opcode.is_ok());
        return current_opcode.unwrap();
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
        assert!(res.is_ok());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        // Interprets negate
        let res = vm.run_once(state);
        assert!(res.is_ok());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        // Interprets return, should stop running after this
        let res = vm.run_once(state);
        assert!(res.is_ok());
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
        assert!(res.is_ok());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        // Interprets negate
        let res = vm.run_once(state);
        assert!(res.is_ok());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        let value = state.stack.last();
        assert!(value.is_some());
        assert_eq!(*value.unwrap(), Value::Number(-1.2));

        // post-negate
        // A lack of instructions is not an error
        let res = vm.run_once(state);
        assert!(res.is_ok());
    }

    #[test]
    fn test_subtraction_eval() {
        let mut chunks = vec![];
        {
            let mut chunk = Chunk::new();
            assert!(chunk.write_constant(Value::Number(3.0), 123).is_ok());
            assert!(chunk.write_constant(Value::Number(1.0), 123).is_ok());
            chunk.write_opcode(OpCode::Subtract, 123);

            chunks.push(chunk);
        }
        let vm = Vm::new();

        let state = build_state(chunks);

        // Interprets add constant
        let res = vm.run_once(state);
        assert!(res.is_ok());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        // Interprets add constant
        let res = vm.run_once(state);
        assert!(res.is_ok());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        // Interprets substract
        let res = vm.run_once(state);
        assert!(res.is_ok());
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
            assert!(res.is_ok());
            let res = chunk.write_constant(Value::Number(1.0), 123);
            assert!(res.is_ok());
            chunks.push(chunk);
        }
        let vm = Vm::new();

        let state = build_state(chunks);

        // add constant
        let res = vm.run_once(state);
        assert!(res.is_ok());
        let (continue_running, state) = res.unwrap();
        assert!(continue_running);

        // add constant
        let res = vm.run_once(state);
        assert!(res.is_ok());
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
        let res = vm.interpret("print 1 + 2;\0", state);
        assert!(res.is_ok());
    }

    #[test]
    fn declare_variable_nil() {
        let vm = Vm::new();
        let mut state = VmState::new();
        let res = vm.interpret("var i;\0", state);
        assert!(res.is_ok());
        state = res.unwrap();

        let var = state.globals.get("i");
        assert!(var.is_some());
        assert_eq!(*var.unwrap(), Value::Nil);
    }

    #[test]
    fn declare_variable() {
        let vm = Vm::new();
        let mut state = VmState::new();
        let res = vm.interpret("var i = \"hello\";\0", state);
        assert!(res.is_ok());
        state = res.unwrap();

        let var = state.globals.get("i");
        assert!(var.is_some());
        assert_eq!(*var.unwrap(), Value::from(Obj::String("hello".to_owned())));
    }

    #[test]
    fn read_variable() {
        let vm = Vm::new();
        let mut state = VmState::new();

        let res = vm.interpret("var i = \"hello\";\0", state);
        assert!(res.is_ok());
        state = res.unwrap();

        let res = vm.interpret("var f = i + \" world\";\0", state);
        assert!(res.is_ok());
        state = res.unwrap();

        let var = state.globals.get("f");
        assert!(var.is_some());
        assert_eq!(
            *var.unwrap(),
            Value::from(Obj::String("hello world".to_owned()))
        );
    }

    #[test]
    fn set_global() {
        let mut state = build_state_from_source(
            "var breakfast = \"beignets\";
             var beverage = \"cafe au lait\";
             breakfast = \"beignets with \" + beverage;
        \0",
        );
        let vm = Vm::new();

        loop {
            let current_opcode = get_current_opcode_from_state(&state);
            if let OpCode::Return = current_opcode {
                println!("Stopping before return get local");
                let var = state.globals.get("breakfast");
                assert!(var.is_some());
                assert_eq!(
                    *var.unwrap(),
                    Value::from(Obj::String("beignets with cafe au lait".to_owned()))
                );
                break;
            }

            println!("-----------------");
            println!("{}", current_opcode);

            println!("stack before: {}", state.dump_stack());
            let res = vm.run_once(state);
            assert!(res.is_ok(), "{}", res.unwrap_err());
            let (_, new_state) = res.unwrap();
            state = new_state;
            println!("stack after: {}", state.dump_stack());
        }
    }

    #[test]
    fn interpret_global_assignment() {
        let vm = Vm::new();
        let mut state = VmState::new();

        // First statement
        let res = vm.interpret(
            "var global = -1;
                    var a = 2; 
                    global = a;
                    \0",
            state,
        );
        assert!(res.is_ok(), "{}", res.unwrap_err());
        state = res.unwrap();

        let var = state.globals.get("global");
        assert!(var.is_some());
        assert_eq!(*var.unwrap(), Value::Number(2.0));
    }

    #[test]
    fn interpret_local_assignment() {
        let vm = Vm::new();
        let state = VmState::new();

        // First statement
        let res = vm.interpret(
            "{
                        var local = -1;
                        var a = 2; 
                        local = a;
                        print local;
                    }\0",
            state,
        );
        assert!(res.is_ok(), "{}", res.unwrap_err());
    }

    #[test]
    fn step_through_local_assignment() {
        let mut compiler = Compiler::new();

        // This should assign 'local' to 2
        let chunk = compiler.compile(
            "{
                        var local = -1;
                        var a = 2; 
                        local = a;
                        print local;
                    }\0",
        );
        assert!(chunk.is_ok(), "{}", chunk.unwrap_err());

        let mut state = VmState::new();
        state.chunks.push(chunk.unwrap());

        let vm = Vm::new();

        loop {
            let current_instruction_byte =
                state.chunks[state.chunk_index].byte_at(state.instruction_index);
            let current_opcode = OpCode::try_from(current_instruction_byte).unwrap();
            println!("-----------------");
            println!("{}", current_opcode);

            println!("stack before: {}", state.dump_stack());
            let res = vm.run_once(state);
            assert!(res.is_ok(), "{}", res.unwrap_err());
            let (_, new_state) = res.unwrap();
            state = new_state;
            println!("stack after: {}", state.dump_stack());

            if let OpCode::GetLocal = current_opcode {
                println!("Stopping after get local");
                let top_of_stack = state.stack.last();
                assert!(top_of_stack.is_some());
                assert_eq!(*top_of_stack.unwrap(), Value::Number(2.0));
                break;
            }
        }
    }

    #[test]
    fn interpret_locals_set_after_init() {
        let vm = Vm::new();
        let mut state = VmState::new();

        // First statement
        let res = vm.interpret(
            "var global = -1;
                    {
                        var a; 
                        a = 2; 
                        global = a; 
                    }\0",
            state,
        );
        assert!(res.is_ok(), "{}", res.unwrap_err());
        state = res.unwrap();

        let var = state.globals.get("global");
        assert!(var.is_some());
        assert_eq!(*var.unwrap(), Value::Number(2.0));
    }

    #[test]
    fn interpret_locals_set_in_init() {
        let vm = Vm::new();
        let mut state = VmState::new();

        // First statement
        let res = vm.interpret(
            "var global = -1;
                    {
                        var a = 2; 
                        global = a; 
                    }\0",
            state,
        );
        assert!(res.is_ok(), "{}", res.unwrap_err());
        state = res.unwrap();

        let var = state.globals.get("global");
        assert!(var.is_some());
        assert_eq!(*var.unwrap(), Value::Number(2.0));
    }

    #[test]
    fn multi_scope() {
        // This should assign 'outer' to 2
        let mut state = build_state_from_source(
            "{
                        var outer = -1;
                        {
                            var inner = 2; 
                            outer = inner;
                        }
                        print outer;
                    }\0",
        );

        let vm = Vm::new();

        loop {
            let current_opcode = get_current_opcode_from_state(&state);
            if let OpCode::Print = current_opcode {
                println!("Stopping before print get local");
                let top_of_stack = state.stack.last();
                assert!(top_of_stack.is_some());
                assert_eq!(*top_of_stack.unwrap(), Value::Number(2.0));
                break;
            }

            let res = vm.run_once(state);
            assert!(res.is_ok(), "{}", res.unwrap_err());
            let (_, new_state) = res.unwrap();
            state = new_state;
        }
    }

    #[test]
    fn interpret_else() {
        let mut state = build_state_from_source(
            "var value = 0;
                    if (false) {
                        value = -1;
                    } else {
                        value = 1;
                    }
                    print value;
            \0",
        );

        let vm = Vm::new();

        loop {
            if let OpCode::Print = get_current_opcode_from_state(&state) {
                let value = state.globals.get("value").unwrap();
                assert_eq!(*value, Value::Number(1.0));
                break;
            }

            let res = vm.run_once(state);
            assert!(res.is_ok(), "{}", res.unwrap_err());
            let (_, new_state) = res.unwrap();
            state = new_state;
        }
    }

    #[test]
    fn interpret_and_short_circuit() {
        let mut state = build_state_from_source(
            "var value = 0;
                    if (false and (value = 3)) {
                        value = -1;
                    }
                    print value;
            \0",
        );

        let vm = Vm::new();

        loop {
            if let OpCode::Print = get_current_opcode_from_state(&state) {
                let value = state.globals.get("value").unwrap();
                assert_eq!(*value, Value::Number(0.0));
                break;
            }

            let res = vm.run_once(state);
            assert!(res.is_ok(), "{}", res.unwrap_err());
            let (_, new_state) = res.unwrap();
            state = new_state;
        }
    }

    #[test]
    fn interpret_and() {
        let mut state = build_state_from_source(
            "var value = 0;
                    if (true and (3 == 3)) {
                        value = 1;
                    }
                    print value;
            \0",
        );

        let vm = Vm::new();

        loop {
            if let OpCode::Print = get_current_opcode_from_state(&state) {
                let value = state.globals.get("value").unwrap();
                assert_eq!(*value, Value::Number(1.0));
                break;
            }

            let res = vm.run_once(state);
            assert!(res.is_ok(), "{}", res.unwrap_err());
            let (_, new_state) = res.unwrap();
            state = new_state;
        }
    }

    #[test]
    fn interpret_or_short_circuit() {
        let mut state = build_state_from_source(
            "var value = 0;
                    if (true or false) {
                        value = value + 1;
                    }
                    if (false or true) {
                        value = value + 1;
                    }
                    print value;
            \0",
        );

        let vm = Vm::new();

        loop {
            if let OpCode::Print = get_current_opcode_from_state(&state) {
                let value = state.globals.get("value").unwrap();
                assert_eq!(*value, Value::Number(2.0));
                break;
            }

            let res = vm.run_once(state);
            assert!(res.is_ok(), "{}", res.unwrap_err());
            let (_, new_state) = res.unwrap();
            state = new_state;
        }
    }
    #[test]
    fn global_increment() {
        let mut state = build_state_from_source(
            "var value = 0;
            value = value + 1;
            value = value + 1;
            print value;
            \0",
        );

        let vm = Vm::new();

        loop {
            if let OpCode::Print = get_current_opcode_from_state(&state) {
                let value = state.globals.get("value").unwrap();
                assert_eq!(*value, Value::Number(2.0));
                break;
            }

            let res = vm.run_once(state);
            assert!(res.is_ok(), "{}", res.unwrap_err());
            let (_, new_state) = res.unwrap();
            state = new_state;
        }
    }

    #[test]
    fn interpret_while() {
        let mut state = build_state_from_source(
            "var value = 0;
            while (value != 3) {
                value = value + 1;
            }
            print value;
            \0",
        );

        let vm = Vm::new();

        loop {
            if let OpCode::Print = get_current_opcode_from_state(&state) {
                let value = state.globals.get("value").unwrap();
                assert_eq!(*value, Value::Number(3.0));
                break;
            }

            let res = vm.run_once(state);
            assert!(res.is_ok(), "{}", res.unwrap_err());
            let (_, new_state) = res.unwrap();
            state = new_state;
        }
    }

    #[test]
    fn interpret_for() {
        let mut state = build_state_from_source(
            "var value = 0;
            for (var i = 0; i < 3; i = i + 1) {
                value = i;
            }
            print value;
            \0",
        );

        let vm = Vm::new();

        loop {
            if let OpCode::Print = get_current_opcode_from_state(&state) {
                let value = state.globals.get("value").unwrap();
                assert_eq!(*value, Value::Number(2.0));
                break;
            }

            let res = vm.run_once(state);
            assert!(res.is_ok(), "{}", res.unwrap_err());
            let (_, new_state) = res.unwrap();
            state = new_state;
        }
    }
}
