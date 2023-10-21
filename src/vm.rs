#![allow(unused)]
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use crate::{
    chunk::{Chunk, OpCode},
    compiler::Compiler,
    object::Obj,
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
    chunk: Rc<RefCell<Chunk>>,
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: Vec::with_capacity(STACK_MAX),
            chunk: Rc::new(RefCell::new(Chunk::new())),
            globals: HashMap::new(),
        }
    }

    // TODO: Check if needed
    pub fn reset_stack(&mut self) {
        // Clear has no effect on capacity of vec
        self.stack.clear();
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
        self.chunk = Rc::new(RefCell::new(chunk));
        self.run()?;
        // NOTE(aalhendi): is this rly needed?
        self.chunk.borrow_mut().free();
        Ok(())
    }

    fn run(&mut self) -> Result<(), InterpretResult> {
        loop {
            #[cfg(feature = "debug-trace-execution")]
            {
                print!("          ");
                for slot in &self.stack {
                    print!("[ {slot} ]");
                }
                println!(); // newline
                self.chunk.borrow().disassemble_instruction(self.ip);
            }

            let instruction = OpCode::from(self.read_byte());
            match instruction {
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.stack.push(constant);
                }
                OpCode::Return => {
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
                            self.runtime_error("Operand must be a number.");
                            return Err(InterpretResult::RuntimeError);
                        }
                    }
                }
                OpCode::Add => {
                    let b = self.peek_top(0);
                    let a = self.peek_top(1);
                    match (a, b) {
                        // TODO(aalhendi): use binary op?
                        (Value::Obj(Obj::String(_)), Value::Obj(Obj::String(_))) => {
                            let b = self.stack.pop().unwrap();
                            let a = self.stack.pop().unwrap();
                            match (a, b) {
                                (Value::Obj(Obj::String(mut s1)), Value::Obj(Obj::String(s2))) => {
                                    s1.push_str(&s2);
                                    self.stack.push(Value::Obj(Obj::String(s1)));
                                }
                                _ => unreachable!("Can only be strings at this point"),
                            }
                        }
                        (Value::Number(_), Value::Number(_)) => self.binary_op(|a, b| a + b)?,
                        (_, _) => {
                            self.runtime_error("Operands must be two numbers or two strings.")
                        }
                    }
                }
                OpCode::Subtract => self.binary_op(|a, b| a - b)?,
                OpCode::Multiply => self.binary_op(|a, b| a * b)?,
                OpCode::Divide => self.binary_op(|a, b| a / b)?,
                OpCode::Greater => self.binary_op(|a, b| Value::Boolean(a > b))?,
                OpCode::Less => self.binary_op(|a, b| Value::Boolean(a < b))?,
                OpCode::False => self.stack.push(Value::Boolean(false)),
                OpCode::True => self.stack.push(Value::Boolean(true)),
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::Not => {
                    let last = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(last.is_falsey()));
                }
                OpCode::Equal => {
                    // TODO(aalhendi): Unwrap unchecked everywhere
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(self.values_equal(a, b)));
                }
                OpCode::Print => println!("{v}", v = self.stack.pop().unwrap()),
                OpCode::Pop => {
                    self.stack.pop().unwrap();
                }
                OpCode::DefineGlobal => {
                    let name = self.read_string();
                    self.globals.insert(name, self.peek_top(0).clone());
                    self.stack.pop();
                }
                OpCode::GetGlobal => {
                    let name = self.read_string();
                    match self.globals.get(&name) {
                        Some(v) => self.stack.push(v.clone()),
                        None => {
                            self.runtime_error(&format!("Undefined variable '{name}'."));
                            return Err(InterpretResult::RuntimeError);
                        }
                    }
                }
                OpCode::SetGlobal => {
                    let name = self.read_string();
                    let value = self.peek_top(0).clone();
                    if let Entry::Occupied(mut e) = self.globals.entry(name.clone()) {
                        e.insert(value);
                    } else {
                        self.runtime_error(&format!("Undefined variable '{name}'."));
                        return Err(InterpretResult::RuntimeError);
                    }
                }
                OpCode::GetLocal => {
                    let slot = self.read_byte() as usize;
                    self.stack.push(self.stack[slot].clone());
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte() as usize;
                    self.stack[slot] = self.peek_top(0).clone();
                }
                _ => todo!(),
            }
        }
    }

    fn read_string(&mut self) -> String {
        match self.read_constant() {
            Value::Obj(Obj::String(s)) => s,
            _ => unreachable!(),
        }
    }

    // TODO(aalhendi): impl Eq, Partial Eq on Value
    fn values_equal(&self, a: Value, b: Value) -> bool {
        match (a, b) {
            (Value::Number(n1), Value::Number(n2)) => n1 == n2,
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            (Value::Nil, Value::Nil) => true,
            (Value::Obj(Obj::String(s1)), Value::Obj(Obj::String(s2))) => s1 == s2,
            _ => false,
        }
    }

    fn peek_top(&self, distance: usize) -> &Value {
        let len = self.stack.len();
        &self.stack[len - 1 - distance]
    }

    fn runtime_error(&mut self, message: &str) {
        eprintln!("{message}");
        eprintln!(
            "[line {line}] in script",
            line = self.chunk.borrow().lines[self.ip - 1]
        );
        self.reset_stack();
    }

    // TODO: Move to closure? Only used in run. Author def'n as macro in run and undef'n after
    // --- POTENTIAL CLOSURES BEGIN ---
    fn read_byte(&mut self) -> u8 {
        let result = self.chunk.borrow().read_byte(self.ip);
        self.ip += 1;
        result
    }

    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte() as usize;
        self.chunk.borrow().constants.values[idx].clone()
    }

    fn binary_op(
        &mut self,
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
                self.runtime_error("Operands must be numbers.");
                Err(InterpretResult::RuntimeError)
            }
        }
    }
    // --- POTENTIAL CLOSURES END ---
}
