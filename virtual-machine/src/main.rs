mod chunk;
mod macros;
mod value;
mod vm;

use chunk::debug;
use chunk::{Chunk, OpCode};
use vm::Vm;

fn make_test_chunk() -> Chunk {
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2);
    chunk.write_opcode(OpCode::Constant, 123);
    chunk.write_byte(constant as u8, 123);
    chunk.write_opcode(OpCode::Negate, 123);

    chunk.write_opcode(OpCode::Return, 123);

    print!("{}", debug::dissassemble_chunk(&chunk, "test chunk"));
    chunk
}

fn main() {
    let chunks = vec![make_test_chunk()];
    println!("================");
    println!("Running vm...");
    let mut vm = Vm::new(chunks.iter().enumerate(), chunks[0].code_iter().enumerate());
    if let Err(e) = vm.interpret(chunks.iter().enumerate()) {
        eprintln!("{}", e);
    }
}
