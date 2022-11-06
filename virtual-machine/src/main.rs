mod chunk;

use chunk::debug;
use chunk::{Chunk, OpCode};

fn main() {
    let mut chunk = Chunk::new();
    chunk.code.push(OpCode::Return);
    debug::dissassemble_chunk(&chunk, "test chunk");
}
