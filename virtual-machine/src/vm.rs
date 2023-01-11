use crate::{
    chunk::{debug, Chunk, OpCode},
    compiler::{Compiler, CompilerError},
    debug, debugln, herefmt,
    value::{Obj, ObjFunction, Value},
};
use std::{collections::HashMap, fmt};

pub struct Vm {}

const FRAME_MAX: usize = 64;
const STACK_MAX: usize = FRAME_MAX * (u8::MAX as usize);

#[derive(Clone, Debug)]
pub struct VmState {
    frames: Vec<CallFrame>,

    stack: Vec<Value>,
    // Normally, the linked list of Objects would be stored here but I have all thle objects in stored as Arc<Mutex<T>>
    // Maybe I should use unsafe so I could write a garbage collector?
    globals: HashMap<String, Value>, //< Global hash map of values, no string interning :(
}

/// Represents single outgoing function call
#[derive(Clone, Debug)]
struct CallFrame {
    value_stack_index: usize, //< Index into VM's stack in order to get access to the function
    ip: usize, //< Caller stores its own instruction pointer. Unsure why the book has this as a C pointer
    //< on return, the VM will return to the caller's ip
}

impl CallFrame {
    fn new(value_stack_index: usize, ip: usize) -> CallFrame {
        CallFrame {
            value_stack_index,
            ip,
        }
    }
}

pub type ErrorMessage = String;

#[derive(Debug, Clone)]
pub enum InterpretError {
    Compile(CompilerError),
    Runtime(ErrorMessage),
    InstructionOutOfRange(&'static str, u32, usize), // File, line, Position in instruction iterator
    ConstantOutOfRange(&'static str, u32, u8),       // File, line, constant position
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
            InterpretError::ConstantOutOfRange(file, line, constant_pos) => write!(
                f,
                "[{}:{}] Constant offset out of range with index {}",
                file, line, constant_pos
            ),
        }
    }
}

impl VmState {
    pub fn new() -> VmState {
        VmState {
            frames: vec![],
            stack: vec![],
            globals: HashMap::new(),
        }
    }

    fn end_of_instructions(&self) -> Result<bool, InterpretError> {
        let index = self.get_instruction_index()? as usize;
        let len = self.peek_instructions()?.len();
        Ok(index >= len)
    }

    fn get_instruction_index(&self) -> Result<usize, InterpretError> {
        Ok(self.get_current_frame()?.ip as usize)
    }

    fn get_instruction_index_mut(&mut self) -> Result<&mut usize, InterpretError> {
        Ok(&mut self.get_current_frame_mut()?.ip)
    }

    fn read_byte(&mut self) -> Result<u8, InterpretError> {
        let instruction_index = self.get_instruction_index()?;
        let byte = *self.peek_instructions()?.get(instruction_index).ok_or(
            InterpretError::InstructionOutOfRange(file!(), line!(), instruction_index),
        )?;

        *self.get_instruction_index_mut()? += 1;
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

        match self.get_current_frame_mut() {
            Ok(frame) => match frame
                .function
                .chunk
                .get_constant_value(constant_index as usize)
            {
                Some(v) => Ok(v.clone()),
                None => Err(InterpretError::ConstantOutOfRange(
                    file!(),
                    line!(),
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

    fn peek_instructions(&self) -> Result<&[u8], InterpretError> {
        let instructions = self
            .get_current_frame()?
            .function
            .chunk
            .code_iter()
            .as_slice();
        Ok(instructions)
    }

    fn push_onto_stack(&mut self, v: Value) -> Result<(), InterpretError> {
        if self.stack.len() + 1 >= STACK_MAX {
            return Err(InterpretError::Runtime(format!(
                "Hit max stack lengh of {STACK_MAX}"
            )));
        }
        self.stack.push(v);
        Ok(())
    }

    fn pop_from_stack(&mut self) -> Result<Value, InterpretError> {
        self.stack.pop().ok_or_else(|| {
            InterpretError::Runtime(herefmt!(
                "Could not pop value off stack since there were no values there",
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
            let ip = self.get_instruction_index().unwrap_or_default();
            if ip >= instruction_len {
                return format!(
                    "No instructions left. instruction_index={}, instruction.len()={}\n",
                    ip, instruction_len
                );
            } else {
                let (debug_instruction, _) = debug::dissassemble_instruction(chunk, ip);
                return debug_instruction;
            }
        }

        format!("No function in callframe")
    }

    fn reset_stack(&mut self) {
        self.stack.clear()
    }

    fn get_local_from_frame(&self, slot: u8) -> Result<&Value, InterpretError> {
        let frame = self.get_current_frame()?;
        let slot_index = frame.value_stack_index + (slot as usize);
        // jump up to the index where the callframe starts
        self.stack.iter().nth(slot_index).ok_or_else(|| InterpretError::Runtime(herefmt!("Stack height is {} but tried to access value at offset '{}' from top for function '{}'", self.stack.len(), slot_index, frame.function.name)))
    }

    /// returns slot index adjusted for frame offset
    fn adjust_slot_for_frame_offset(&mut self, slot: u8) -> Result<usize, InterpretError> {
        let slot_index = self.get_current_frame()?.value_stack_index + (slot as usize);
        Ok(slot_index)
    }

    fn get_local_from_frame_mut(&mut self, slot: u8) -> Result<&mut Value, InterpretError> {
        let slot_index = self.get_current_frame()?.value_stack_index + (slot as usize);
        // go backwards from top of stack to the value
        let stack_len = self.stack.len();
        self.stack.iter_mut().rev().nth(slot_index).ok_or_else(|| {
            InterpretError::Runtime(herefmt!(
                "Stack height is {} but tried to access value at offset '{}' from top",
                stack_len,
                slot_index
            ))
        })
    }

    fn get_current_frame_mut(&mut self) -> Result<&mut CallFrame, InterpretError> {
        self.frames
            .last_mut()
            .ok_or_else(|| InterpretError::Runtime(herefmt!("No callframes available")))
    }

    fn get_current_frame(&self) -> Result<&CallFrame, InterpretError> {
        self.frames
            .last()
            .ok_or_else(|| InterpretError::Runtime(herefmt!("No callframes available")))
    }

    fn peek_latest_chunk(&self) -> Result<&Chunk, InterpretError> {
        self.frames
            .last()
            .map(|f| &f.function.chunk)
            .ok_or_else(|| InterpretError::Runtime(herefmt!("No callframes available")))
    }

    pub fn runtime_error(&self, message: String) -> InterpretError {
        let instruction_index = self.get_instruction_index().unwrap_or_default();

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
                        state.push_onto_stack(constant)?;
                    }
                    OpCode::Nil => {
                        state.push_onto_stack(Value::Nil)?;
                    }
                    OpCode::True => {
                        state.push_onto_stack(Value::Bool(true))?;
                    }
                    OpCode::False => {
                        state.push_onto_stack(Value::Bool(false))?;
                    }
                    OpCode::Equal => {
                        let (lhs, rhs) = state.pop_pair_from_stack()?;
                        state.push_onto_stack(Value::Bool(lhs == rhs))?;
                    }
                    OpCode::Pop => {
                        state.stack.pop();
                    }
                    OpCode::GetLocal => {
                        let slot = state.read_byte()?;
                        // Push value to the top of the stack so it can be used
                        // I can't tell if the book is doing a copy or using a pointer here, i'll just defer to copy
                        let value = state.get_local_from_frame(slot)?;
                        //debugln!("Got local with slot '{}' and value '{}'", slot, value);
                        state.push_onto_stack(value.clone())?;
                    }
                    OpCode::SetLocal => {
                        let slot = state.read_byte()?;
                        let slot = state.adjust_slot_for_frame_offset(slot)?;

                        let value = state.peek_on_stack(0)?;
                        let top_of_stack_index = state.stack.len() - 1;
                        //debugln!(
                        //    "SetLocal wiht slot={slot}, value={value}, stack_height={top_of_stack_index}\nstack: {}", state.dump_stack()
                        //);

                        if slot == top_of_stack_index + 1 {
                            state.push_onto_stack(value.clone())?;
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
                                state.push_onto_stack(value.clone())?;
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
                        state.push_onto_stack(Value::Bool(lhs > rhs))?;
                    }
                    OpCode::Less => {
                        let (lhs, rhs) = state.pop_pair_from_stack()?;
                        state.push_onto_stack(Value::Bool(lhs < rhs))?;
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
                                    state.push_onto_stack(value)?;
                                }
                                (Obj::String(_), Obj::Function(_)) => todo!(),
                                (Obj::Function(_), Obj::String(_)) => todo!(),
                                (Obj::Function(_), Obj::Function(_)) => todo!(),
                            }
                        }
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            state.push_onto_stack(Value::Number(lhs + rhs))?
                        }
                        (lhs, rhs) => {
                            let err = state.runtime_error(herefmt!(
                                "Operands must be two numbers or strings but were lhs={:?} and rhs={:?}",
                                lhs,
                                rhs
                            ));
                            state.push_onto_stack(rhs)?;
                            state.push_onto_stack(lhs)?;
                            return Err(err);
                        }
                    },
                    OpCode::Subtract => match state.pop_pair_from_stack()? {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            state.push_onto_stack(Value::Number(lhs - rhs))?;
                        }
                        (lhs, rhs) => {
                            let err = state.runtime_error(herefmt!(
                                "Operands must be numbers but were lhs={:?} and rhs={:?}",
                                lhs,
                                rhs
                            ));
                            state.push_onto_stack(rhs)?;
                            state.push_onto_stack(lhs)?;
                            return Err(err);
                        }
                    },
                    OpCode::Multiply => match state.pop_pair_from_stack()? {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            state.push_onto_stack(Value::Number(lhs * rhs))?
                        }
                        (lhs, rhs) => {
                            let err = state.runtime_error(herefmt!(
                                "Operands must be numbers but were lhs={:?} and rhs={:?}",
                                &lhs,
                                &rhs
                            ));
                            state.push_onto_stack(rhs)?;
                            state.push_onto_stack(lhs)?;
                            return Err(err);
                        }
                    },
                    OpCode::Divide => match state.pop_pair_from_stack()? {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            state.push_onto_stack(Value::Number(lhs / rhs))?
                        }
                        (lhs, rhs) => {
                            let err = state.runtime_error(herefmt!(
                                "Operands must be numbers but were lhs={:?} and rhs={:?}",
                                lhs,
                                rhs
                            ));
                            state.push_onto_stack(rhs)?;
                            state.push_onto_stack(lhs)?;
                            return Err(err);
                        }
                    },
                    OpCode::Not => {
                        let value = state.pop_from_stack()?;
                        state.push_onto_stack(Value::Bool(value.falsey()))?;
                    }
                    OpCode::Negate => {
                        match state.pop_from_stack()? {
                            Value::Number(value) => state.push_onto_stack(Value::Number(-value))?,
                            other => {
                                // put it back on the stack since it wasn't what we expected
                                state.push_onto_stack(other)?;
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
                        *state.get_instruction_index_mut()? += offset as usize;
                    }
                    OpCode::JumpIfFalse => {
                        let offset = state.read_short()?;
                        let top_value = state.peek_on_stack(0)?;
                        if top_value.falsey() {
                            debugln!("Incrementing instruction index by {}", offset);
                            *state.get_instruction_index_mut()? += offset as usize;
                        }
                    }
                    OpCode::Loop => {
                        let offset = state.read_short()?;
                        *state.get_instruction_index_mut()? -= offset as usize;
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
        let function = compiler.compile(source).map_err(InterpretError::Compile)?;


        state.push_onto_stack(Value::from(Obj::Function(function)))?;
        state.frames.push(CallFrame::new(state.stack.len() - 1, ip, slot_offset))
        todo!();
        //state.frames.push(function);
        // Update indexes of state to match the new generated chunk
        //state.chunk_index = state.chunks.len() - 1;
        //state.instruction_index = 0;

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

    fn build_state(chunk: Chunk) -> VmState {
        let mut state = VmState::new();
        state.get_current_frame_mut().unwrap().function.chunk = chunk;
        state
    }

    fn build_state_from_source(source: &str) -> VmState {
        let mut compiler = Compiler::new();
        let function = compiler.compile(source).unwrap();
        build_state(function.chunk)
    }

    fn get_current_instruction_byte(state: &VmState) -> u8 {
        state
            .get_current_frame()
            .unwrap()
            .function
            .chunk
            .byte_at(state.get_instruction_index().unwrap())
    }

    fn get_current_opcode_from_state(state: &VmState) -> OpCode {
        let current_opcode = OpCode::try_from(get_current_instruction_byte(state));

        assert!(current_opcode.is_ok());
        return current_opcode.unwrap();
    }

    #[test]
    fn test_example_eval() {
        let mut chunk = Chunk::new();

        assert!(chunk.write_constant(Value::Number(1.2), 123).is_ok());

        chunk.write_opcode(OpCode::Negate, 123);

        chunk.write_opcode(OpCode::Return, 123);

        println!(
            "OpCodes (u8): {:?}",
            chunk.code_iter().collect::<Vec<&u8>>()
        );
        println!(
            "Constants : {:?}",
            chunk.constant_iter().collect::<Vec<&Value>>()
        );
        let vm = Vm::new();

        let state = build_state(chunk);

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
        let mut chunk = Chunk::new();

        assert!(chunk.write_constant(Value::Number(1.2), 123).is_ok());

        chunk.write_opcode(OpCode::Negate, 123);

        let vm = Vm::new();

        let state = build_state(chunk);

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
        let mut chunk = Chunk::new();
        assert!(chunk.write_constant(Value::Number(3.0), 123).is_ok());
        assert!(chunk.write_constant(Value::Number(1.0), 123).is_ok());
        chunk.write_opcode(OpCode::Subtract, 123);
        let vm = Vm::new();

        let state = build_state(chunk);

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
        let mut chunk = Chunk::new();
        let res = chunk.write_constant(Value::Number(3.0), 123);
        assert!(res.is_ok());
        let res = chunk.write_constant(Value::Number(1.0), 123);
        assert!(res.is_ok());
        let vm = Vm::new();

        let state = build_state(chunk);

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
        let state = build_state_from_source(
            "{
                        var local = -1;
                        var a = 2; 
                        local = a;
                        print local;
                    }\0",
        );

        let vm = Vm::new();

        loop {
            let current_opcode = get_current_opcode_from_state(&state);
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
