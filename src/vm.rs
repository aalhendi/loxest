#![allow(unused)]
use crate::{
    chunk::{Chunk, OpCode},
    compiler::Compiler,
    value::Value,
};

const STACK_MAX: usize = 256;

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
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
    pub fn reset_stack(&mut self) {
        self.stack = Vec::with_capacity(STACK_MAX);
    }

    pub fn free(&mut self) {}

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretResult> {
        let mut chunk = Chunk::new();
        let mut compiler = Compiler::new(source, &mut chunk);
        if (!compiler.compile()) {
            chunk.free();
            return Err(InterpretResult::CompileError);
        }

        self.ip = 0;
        self.run(&chunk)?;
        chunk.free();
        Ok(())
    }

    fn run(&mut self, chunk: &Chunk) -> Result<(), InterpretResult> {
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
                    return Ok(());
                }
                OpCode::Negate => {
                    // NOTE: Not sure which is faster
                    // let value = -self.stack.pop().unwrap();
                    // self.stack.push(value);
                    // or
                    // self.stack[last_idx] = -self.stack[last_idx]
                    match self.peek_top(0).clone() {
                        Value::Number(_) => {
                            let value = self.stack.pop().unwrap();
                            self.stack.push(-value);
                        }
                        _ => {
                            self.runtime_error(chunk, "Operand must be a number.");
                            return Err(InterpretResult::RuntimeError);
                        }
                    }
                }
                OpCode::Add => self.binary_op(chunk, |a, b| a + b)?,
                OpCode::Subtract => self.binary_op(chunk, |a, b| a - b)?,
                OpCode::Multiply => self.binary_op(chunk, |a, b| a * b)?,
                OpCode::Divide => self.binary_op(chunk, |a, b| a / b)?,
                OpCode::Greater => self.binary_op(chunk, |a, b| Value::Boolean(a > b))?,
                OpCode::Less => self.binary_op(chunk, |a, b| Value::Boolean(a < b))?,
                OpCode::False => self.stack.push(Value::Boolean(false)),
                OpCode::True => self.stack.push(Value::Boolean(true)),
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::Not => {
                    let last = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(self.is_falsey(last)));
                }
                OpCode::Equal => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(self.values_equal(a, b)));
                }
                _ => todo!(),
            }
        }
    }

    fn values_equal(&self, a: Value, b: Value) -> bool {
        match (a, b) {
            (Value::Number(n1), Value::Number(n2)) => n1 == n2,
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }

    // NOTE: Lox follows ruby in that only false and nil are false in lox
    fn is_falsey(&self, value: Value) -> bool {
        match value {
            Value::Number(v) => false,
            Value::Boolean(v) => !v,
            Value::Nil => true,
        }
    }

    fn peek_top(&self, distance: usize) -> &Value {
        let len = self.stack.len();
        &self.stack[len - 1 - distance]
    }

    fn runtime_error(&mut self, chunk: &Chunk, message: &str) {
        eprintln!("{message}");
        eprintln!("[line {line}] in script", line = chunk.lines[self.ip - 1]);
        self.reset_stack();
    }

    // TODO: Move to closure? Only used in run. Author def'n as macro in run and undef'n after
    // --- POTENTIAL CLOSURES BEGIN ---
    fn read_byte(&mut self, chunk: &Chunk) -> u8 {
        let result = chunk.read_byte(self.ip);
        self.ip += 1;
        result
    }

    fn read_constant(&mut self, chunk: &Chunk) -> Value {
        let idx = self.read_byte(chunk) as usize;
        chunk.constants.values[idx].clone()
    }

    fn binary_op(
        &mut self,
        chunk: &Chunk,
        op_closure: fn(a: Value, b: Value) -> Value,
    ) -> Result<(), InterpretResult> {
        let b = self.peek_top(0);
        let a = self.peek_top(1);
        match (a, b) {
            (Value::Number(_), Value::Number(_)) => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(op_closure(a, b));
                Ok(())
            }
            _ => {
                self.runtime_error(chunk, "Operands must be numbers.");
                Err(InterpretResult::RuntimeError)
            }
        }
    }
    // --- POTENTIAL CLOSURES END ---
}
