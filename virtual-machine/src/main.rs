mod chunk;
mod value;

use chunk::debug;
use chunk::{Chunk, OpCode};

fn main() {
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2);
    chunk.write_opcode(OpCode::Constant);
    chunk.write_byte(constant as u8);

    chunk.write_opcode(OpCode::Return);
    debug::dissassemble_chunk(&chunk, "test chunk");
}
