use crate::chunk::{Chunk, OpCode};

mod chunk;
mod value;

fn main() {
    let mut chunk = Chunk::new();
    let constant_idx = chunk.add_constant(1.2);
    chunk.write_op(OpCode::Cosntant, 123);
    chunk.write_byte(constant_idx, 123);
    chunk.write_op(OpCode::Return, 123);

    chunk.disassemble("test chunk");
    chunk.free();
}
