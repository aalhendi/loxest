#![allow(unused)]
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use crate::{
    chunk::{Chunk, OpCode},
    compiler::{Compiler, FunctionType},
    object::{native_clock, NativeFn, Obj, ObjFunction, ObjNative},
    value::Value,
};

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * (u8::MAX as usize + 1);

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

pub struct CallFrame {
    pub function: Rc<ObjFunction>, // Ptr to fuction being called. Used to look up constants and other stuff.
    pub ip: usize, // Caller stores its own IP index (as opposed to storing its own ptr in C)
    pub slots: usize, // index into VM's stack. Points to first slot the function can use.
}

impl CallFrame {
    pub fn new(function: Rc<ObjFunction>, ip: usize, slots: usize) -> Self {
        Self {
            function,
            ip,
            slots,
        }
    }
}

pub struct VM {
    stack: Vec<Value>, // No need to impl a stack data structure... Vec does it all
    globals: HashMap<String, Value>,
    frames: Vec<CallFrame>,
}

impl VM {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(STACK_MAX),
            globals: HashMap::new(),
            frames: Vec::with_capacity(FRAMES_MAX), // TODO(aalhendi): fixed size array?
        };

        vm.define_native("clock", native_clock);

        vm
    }

    fn define_native(&mut self, name: &str, function: NativeFn) {
        // C version pushes 2 values to the stack. String: name and Obj: func
        // it sets these values in the globals table then pops the 2 values off
        // the stack. That is done for GC purposes to potentially trigger a collection.
        // Since we are going with an RC approach this can all be summerized in a globals.insert
        self.globals.insert(
            name.to_owned(),
            Value::Obj(Obj::Native(ObjNative::new(function))),
        );
    }

    // TODO: Check if needed
    pub fn reset_stack(&mut self) {
        // Clear has no effect on capacity of vec
        self.stack.clear();
    }

    pub fn free(&mut self) {}

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretResult> {
        let mut compiler = Compiler::new(source, FunctionType::Script);
        let function = compiler.compile();
        if function.is_none() {
            // NOTE(aalhendi): is this rly needed?
            compiler.current_chunk().borrow_mut().free();
            return Err(InterpretResult::CompileError);
        }

        // TODO(aalhendi): Cleaner impl?
        let function = function.unwrap();
        self.stack
            .push(Value::Obj(Obj::Function(Rc::clone(&function))));
        self.call(function, 0);

        let result = self.run();
        self.stack.pop();
        result
    }

    fn ip(&self) -> usize {
        self.frames.last().unwrap().ip
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
                let ip = self.ip();
                self.chunk().borrow().disassemble_instruction(ip);
            }

            let instruction = OpCode::from(self.read_byte());
            match instruction {
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.stack.push(constant);
                }
                OpCode::Return => {
                    let result = self.stack.pop().unwrap();
                    let prev_frame = self.frames.pop().unwrap();
                    if self.frames.is_empty() {
                        self.stack.pop();
                        return Ok(());
                    }
                    // Pop the function and its params from the stack
                    self.stack.truncate(prev_frame.slots);
                    self.stack.push(result);
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
                    let slot_offset = self.frame().slots;
                    self.stack.push(self.stack[slot + slot_offset].clone());
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte() as usize;
                    let slot_offset = self.frame().slots;
                    self.stack[slot + slot_offset] = self.peek_top(0).clone();
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short();
                    if self.peek_top(0).is_falsey() {
                        self.frames.last_mut().unwrap().ip += offset as usize;
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_short();
                    self.frames.last_mut().unwrap().ip += offset as usize;
                }
                OpCode::Loop => {
                    let offset = self.read_short();
                    self.frames.last_mut().unwrap().ip -= offset as usize;
                }
                OpCode::Call => {
                    let arg_count = self.read_byte() as usize;
                    let func_to_call = self.peek_top(arg_count);
                    if !self.call_value(func_to_call.clone(), arg_count) {
                        return Err(InterpretResult::RuntimeError);
                    }
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

    fn call(&mut self, function: Rc<ObjFunction>, arg_count: usize) -> bool {
        if arg_count != function.arity {
            self.runtime_error(&format!(
                "Expected {arity} arguments but got {arg_count}",
                arity = function.arity
            ));
            return false;
        }
        if self.frames.len() == FRAMES_MAX {
            self.runtime_error("Stack overflow.");
            return false;
        }
        let slots = self.stack.len() - arg_count - 1;
        let frame = CallFrame::new(function, 0, slots);
        self.frames.push(frame);
        true
    }

    fn call_value(&mut self, callee: Value, arg_count: usize) -> bool {
        match callee {
            Value::Obj(o) => match o {
                Obj::String(_) => {
                    self.runtime_error("Can only call functions and classes.");
                    false
                }
                Obj::Function(f) => self.call(f, arg_count),
                Obj::Native(f) => {
                    // From the stack top - arg count to the stack top
                    let slice = self.stack.len() - arg_count..self.stack.len();
                    let result = (f.function)(arg_count, &self.stack[slice]);
                    self.stack.truncate(self.stack.len() - arg_count + 1);
                    self.stack.push(result);
                    true
                }
            },
            _ => {
                self.runtime_error("Can only call functions and classes.");
                false
            }
        }
    }

    fn runtime_error(&mut self, message: &str) {
        eprintln!("{message}");

        for frame in self.frames.iter().rev() {
            // -1 because IP already sitting on the next instruction to be executed
            // but we want stack trace to point to the previous failed instruction.
            let i = frame.ip - 1;
            let name = if frame.function.name.is_empty() {
                "script".to_owned()
            } else {
                frame.function.name.clone()
            };
            eprintln!(
                "[line {line}] in {name}()",
                line = frame.function.chunk.borrow().lines[i]
            );
        }

        self.reset_stack();
    }

    // TODO: Move to closure? Only used in run. Author def'n as macro in run and undef'n after
    // --- POTENTIAL CLOSURES BEGIN ---
    fn frame(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn chunk(&mut self) -> &Rc<RefCell<Chunk>> {
        &self.frame().function.chunk
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.frame();
        frame.ip += 1;
        let ip_idx = frame.ip - 1;
        self.chunk().borrow().read_byte(ip_idx)
    }

    fn read_short(&mut self) -> u16 {
        // TODO(aalhendi): Do we really need to clone to read here?
        let chunk = self.chunk().borrow().clone();
        let frame = self.frame();
        frame.ip += 2;
        let byte1 = chunk.read_byte(frame.ip - 2) as u16;
        let byte2 = chunk.read_byte(frame.ip - 1) as u16;
        (byte1 << 8) | byte2
    }

    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte() as usize;
        self.chunk().borrow().constants.values[idx].clone()
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
