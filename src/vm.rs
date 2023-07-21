#![allow(unused)]
use crate::{
    chunk::{Chunk, OpCode},
    compiler::Compiler,
    value::Value,
};

const STACK_MAX: usize = 256;

pub enum InterpretResult {
    Ok,
    _CompileError,
    _RuntimeError,
}

pub struct VM {
    ip: usize,         // instead of a pointer, we're gonna use an index into the array
    stack: Vec<Value>, // No need to impl a stack data structure... Vec does it all
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: Vec::with_capacity(STACK_MAX),
        }
    }

    // TODO: Check if needed
    pub fn _reset_stack(&mut self) {
        self.stack = Vec::with_capacity(STACK_MAX);
    }

    pub fn free(&mut self) {}

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let compiler = Compiler::new();
        compiler.compile(source);
        InterpretResult::Ok
    }

    fn run(&mut self, chunk: &Chunk) -> InterpretResult {
        loop {
            #[cfg(feature = "debug-trace-execution")]
            {
                print!("          ");
                for slot in &self.stack {
                    print!("[ {slot} ]");
                }
                println!(); // newline
                chunk.disassemble_instruction(self.ip);
            }

            let instruction = OpCode::from(self.read_byte(chunk));
            match instruction {
                OpCode::Constant => {
                    let constant = self.read_constant(chunk);
                    self.stack.push(constant);
                }
                OpCode::Return => {
                    println!("{v}", v = self.stack.pop().unwrap());
                    return InterpretResult::Ok;
                }
                OpCode::Negate => {
                    let last_idx = self.stack.len() - 1;
                    self.stack[last_idx] = -self.stack[last_idx];

                    // NOTE: Not sure which is faster
                    // let value = -self.stack.pop().unwrap();
                    // self.stack.push(value);
                }
                OpCode::Add => self.binary_op(|a, b| a + b),
                OpCode::Subtract => self.binary_op(|a, b| a - b),
                OpCode::Multiply => self.binary_op(|a, b| a * b),
                OpCode::Divide => self.binary_op(|a, b| a / b),
                _ => todo!(),
            }
        }
    }

    // TODO: Move to closure? Only used in run. Author def'n as macro in run and undef'n after
    // --- POTENTIAL CLOSURES BEGIN ---
    fn read_byte(&mut self, chunk: &Chunk) -> u8 {
        let result = chunk.read_byte(self.ip);
        self.ip += 1;
        result
    }

    fn read_constant(&mut self, chunk: &Chunk) -> Value {
        chunk.constants.values[self.read_byte(chunk) as usize]
    }

    fn binary_op(&mut self, op_closure: fn(a: Value, b: Value) -> Value) {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();

        self.stack.push(op_closure(a, b));
    }
    // --- POTENTIAL CLOSURES END ---
}
