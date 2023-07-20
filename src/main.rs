use vm::VM;

use crate::chunk::{Chunk, OpCode};

mod chunk;
mod value;
mod vm;

fn main() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();
    let mut constant_idx = chunk.add_constant(1.2);
    chunk.write_op(OpCode::Constant, 123);
    chunk.write_byte(constant_idx, 123);

    constant_idx = chunk.add_constant(3.4);
    chunk.write_op(OpCode::Constant, 123);
    chunk.write_byte(constant_idx, 123);

    chunk.write_op(OpCode::Add, 123);

    constant_idx = chunk.add_constant(5.6);
    chunk.write_op(OpCode::Constant, 123);
    chunk.write_byte(constant_idx, 123);

    chunk.write_op(OpCode::Divide, 123);
    chunk.write_op(OpCode::Negate, 123);

    chunk.write_op(OpCode::Return, 123);

    chunk.disassemble("test chunk");
    vm.interpret(&chunk);

    chunk.free();
    vm.free();
}
