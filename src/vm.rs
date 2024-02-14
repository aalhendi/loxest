#![allow(unused)]
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    ops::Deref,
    rc::Rc,
};

use crate::{
    chunk::{Chunk, OpCode},
    compiler::{Compiler, FunctionType},
    object::{
        native_clock, NativeFn, Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction,
        ObjInstance, ObjNative, ObjUpvalue,
    },
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
    pub closure: Rc<ObjClosure>, // Ptr to fuction being called. Used to look up constants and other stuff.
    pub ip: usize, // Caller stores its own IP index (as opposed to storing its own ptr in C)
    pub slots: usize, // index into VM's stack. Points to first slot the function can use.
}

impl CallFrame {
    pub fn new(closure: Rc<ObjClosure>, ip: usize, slots: usize) -> Self {
        Self { closure, ip, slots }
    }
}

pub struct VM {
    stack: Vec<Value>, // No need to impl a stack data structure... Vec does it all
    globals: HashMap<String, Value>,
    frames: Vec<CallFrame>,
    open_upvalues: Vec<Rc<RefCell<ObjUpvalue>>>,
}

impl VM {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(STACK_MAX),
            globals: HashMap::new(),
            frames: Vec::with_capacity(FRAMES_MAX), // TODO(aalhendi): fixed size array?
            open_upvalues: Vec::new(),
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
            Value::Obj(Obj::Native(ObjNative::new(function)).into()),
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
        let closure = Rc::new(ObjClosure::new(function.unwrap()));
        self.stack
            .push(Value::Obj(Obj::Closure(closure.clone()).into()));
        self.call(closure, 0);

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
                    let slot = prev_frame.slots;
                    self.close_upvalues(slot);
                    if self.frames.is_empty() {
                        self.stack.pop();
                        return Ok(());
                    }
                    // Pop the function and its params from the stack
                    self.stack.truncate(prev_frame.slots);
                    self.stack.push(result);
                }
                OpCode::Negate => {
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
                #[allow(clippy::collapsible_match)]
                OpCode::Add => {
                    // ty jprochazk
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    if let Value::Number(left) = left {
                        if let Value::Number(right) = right {
                            self.stack.push(Value::Number(left + right));
                            continue;
                        }
                    }
                    if let Value::Obj(left) = left {
                        if let Obj::String(left) = left.deref() {
                            if let Value::Obj(right) = right {
                                if let Obj::String(right) = right.deref() {
                                    let new_obj = Value::Obj(
                                        Obj::String(format!("{}{}", left, right)).into(),
                                    );
                                    self.stack.push(new_obj);
                                    continue;
                                }
                            }
                        }
                    }
                    self.runtime_error("Operands must be two numbers or two strings.");
                    return Err(InterpretResult::RuntimeError);
                }
                OpCode::Subtract => self.binary_op(|a, b| Value::Number(a - b))?,
                OpCode::Multiply => self.binary_op(|a, b| Value::Number(a * b))?,
                OpCode::Divide => self.binary_op(|a, b| Value::Number(a / b))?,
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
                    self.stack.pop();
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
                OpCode::Closure => {
                    let function = &self.read_constant().as_closure().clone().function;
                    let mut closure = ObjClosure::new(function.clone());
                    for _ in 0..function.upvalue_count {
                        let is_local = self.read_byte() == 1;
                        let index = self.read_byte() as usize;
                        let captured = if is_local {
                            let idx = self.frame().slots + index;
                            self.capture_upvalue(idx)
                        } else {
                            self.frame().closure.upvalues[index].clone()
                        };
                        closure.upvalues.push(captured);
                    }
                    self.stack
                        .push(Value::Obj(Obj::Closure(Rc::new(closure)).into()));
                }
                OpCode::GetUpvalue => {
                    let slot = self.read_byte() as usize;
                    let upvalue = self.frame().closure.upvalues[slot].borrow().clone();
                    let value = match &upvalue.closed {
                        Some(value) => value.clone(),
                        None => self.stack[upvalue.location].clone(),
                    };
                    self.stack.push(value);
                }
                OpCode::SetUpvalue => {
                    let slot = self.read_byte() as usize;
                    let value = self.peek_top(0).clone();
                    let upvalue = self.frame().closure.upvalues[slot].borrow().clone();
                    if upvalue.closed.is_none() {
                        self.stack[upvalue.location] = value;
                    } else {
                        self.frame().closure.upvalues[slot].borrow_mut().closed = Some(value);
                    }
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1);
                    self.stack.pop();
                }
                OpCode::Class => {
                    let class_name = self.read_string();
                    let value = Value::Obj(
                        Obj::Class(Rc::new(RefCell::new(ObjClass::new(class_name)))).into(),
                    );
                    self.stack.push(value);
                }
                OpCode::GetProperty => {
                    let instance_rc = if let Some(i) = self.peek_top(0).as_instance_maybe() {
                        i.clone()
                    } else {
                        self.runtime_error("Only instances have properties.");
                        return Err(InterpretResult::RuntimeError);
                    };
                    let name = self.read_string();

                    let instance = instance_rc.borrow();
                    if let Some(value) = instance.fields.get(&name) {
                        self.stack.pop(); // pop the instance
                        self.stack.push(value.clone());
                    } else if !self.bind_method(instance.klass.clone(), name.clone()) {
                        return Err(InterpretResult::RuntimeError);
                    }
                }
                OpCode::SetProperty => {
                    let instance_rc = if let Some(i) = self.peek_top(1).as_instance_maybe() {
                        i.clone()
                    } else {
                        self.runtime_error("Only instances have fields.");
                        return Err(InterpretResult::RuntimeError);
                    };

                    let name = self.read_string();
                    let value = self.peek_top(0).clone();
                    {
                        let mut instance = instance_rc.borrow_mut();
                        instance.fields.insert(name, value);
                    }

                    // PERF(aalhendi): is `.remove(len-2)` faster? no pop/push and no clone
                    let value = self.stack.pop().unwrap().clone();
                    self.stack.pop(); // pop instance
                    self.stack.push(value);
                }
                OpCode::Method => {
                    let name = self.read_string();
                    self.define_method(name);
                }
                OpCode::Invoke => {
                    let method = self.read_string();
                    let arg_count = self.read_byte() as usize;
                    if !self.invoke(&method, arg_count) {
                        return Err(InterpretResult::RuntimeError);
                    }
                }
                OpCode::Inherit => {
                    let superclass = if let Some(v) = self.peek_top(1).as_class_maybe() {
                        v
                    } else {
                        self.runtime_error("Superclass must be a class.");
                        return Err(InterpretResult::RuntimeError);
                    };
                    let subclass = self.peek_top(0).as_class();

                    // copy-down inheritance. works here because Lox classes are /closed/
                    let super_methods = superclass.borrow().methods.clone();
                    subclass.borrow_mut().methods.extend(super_methods);
                    self.stack.pop(); // subclass
                }
                OpCode::GetSuper => {
                    let name = self.read_string();
                    let superclass = self.stack.pop().unwrap().as_class().clone();
                    if !self.bind_method(superclass, name) {
                        return Err(InterpretResult::RuntimeError);
                    }
                }
                OpCode::SuperInvoke => {
                    let method = self.read_string();
                    let arg_count = self.read_byte() as usize;
                    let superclass = self.stack.pop().unwrap().as_class().clone();
                    if !self.invoke_from_class(superclass, &method, arg_count) {
                        return Err(InterpretResult::RuntimeError);
                    }
                }
                _ => todo!(),
            }
        }
    }


    fn read_string(&mut self) -> String {
        // NOTE(aalhendi): Essentially this but avoids the clone
        // self.read_constant().as_string()
        let idx = self.read_byte() as usize;
        self.chunk().borrow().constants.values[idx].as_string()
    }

    // TODO(aalhendi): impl Eq, Partial Eq on Value
    fn values_equal(&self, a: Value, b: Value) -> bool {
        match (a, b) {
            (Value::Number(n1), Value::Number(n2)) => n1 == n2,
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            (Value::Nil, Value::Nil) => true,
            (Value::Obj(o1), Value::Obj(o2)) => match (o1.deref(), o2.deref()) {
                (Obj::String(s1), Obj::String(s2)) => s1 == s2,
                (Obj::Class(s1), Obj::Class(s2)) => s1 == s2,
                (Obj::BoundMethod(s1), Obj::BoundMethod(s2)) => s1 == s2,
                _ => false,
            },
            _ => false,
        }
    }

    fn peek_top(&self, distance: usize) -> &Value {
        let len = self.stack.len();
        &self.stack[len - 1 - distance]
    }

    fn call(&mut self, closure: Rc<ObjClosure>, arg_count: usize) -> bool {
        if arg_count != closure.function.arity {
            self.runtime_error(&format!(
                "Expected {arity} arguments but got {arg_count}.",
                arity = closure.function.arity
            ));
            return false;
        }
        if self.frames.len() == FRAMES_MAX {
            self.runtime_error("Stack overflow.");
            return false;
        }
        let slots = self.stack.len() - arg_count - 1;
        let frame = CallFrame::new(closure, 0, slots);
        self.frames.push(frame);
        true
    }

    fn call_value(&mut self, callee: Value, arg_count: usize) -> bool {
        match callee {
            Value::Obj(o) => match o.deref() {
                Obj::String(_) | Obj::_Upvalue(_) | Obj::Instance(_) => {
                    self.runtime_error("Can only call functions and classes.");
                    false
                }
                // NOTE(aalhendi): Funcs are wrapped in Closures now. Is this needed?
                Obj::_Function(f) => self.call(Rc::new(ObjClosure::new(f.clone())), arg_count),
                Obj::Native(f) => {
                    // From the stack top - arg count to the stack top
                    let slice = self.stack.len() - arg_count..self.stack.len();
                    let result = (f.function)(arg_count, &self.stack[slice]);
                    self.stack.truncate(self.stack.len() - (arg_count + 1));
                    self.stack.push(result);
                    true
                }
                Obj::Closure(c) => self.call(c.clone(), arg_count),
                Obj::Class(c) => {
                    let idx = self.stack.len() - arg_count - 1;
                    self.stack[idx] = Value::Obj(
                        Obj::Instance(Rc::new(RefCell::new(ObjInstance::new(c.clone())))).into(),
                    );
                    if let Some(initializer) = c.borrow().methods.get("init") {
                        let init = initializer.as_closure();
                        self.call(init.clone(), arg_count)
                    } else if arg_count != 0 {
                        self.runtime_error(&format!("Expected 0 arguments but got {arg_count}."));
                        return false;
                    } else {
                        true
                    }
                }
                Obj::BoundMethod(m) => {
                    let idx = self.stack.len() - arg_count - 1;
                    self.stack[idx] = m.receiver.clone();
                    self.call(m.method.clone(), arg_count)
                }
            },
            _ => {
                self.runtime_error("Can only call functions and classes.");
                false
            }
        }
    }

    fn invoke_from_class(
        &mut self,
        klass: Rc<RefCell<ObjClass>>,
        name: &str,
        arg_count: usize,
    ) -> bool {
        if let Some(method) = klass.borrow().methods.get(name) {
            let closure = method.as_closure();
            self.call(closure.clone(), arg_count)
        } else {
            self.runtime_error(&format!("Undefined property '{name}'."));
            false
        }
    }

    fn invoke(&mut self, name: &str, arg_count: usize) -> bool {
        let receiver = self.peek_top(arg_count);
        let instance = if let Some(v) = receiver.as_instance_maybe() {
            v.borrow().clone()
        } else {
            self.runtime_error("Only instances have methods.");
            return false;
        };

        if let Some(value) = instance.fields.get(name) {
            let idx = self.stack.len() - arg_count - 1;
            self.stack[idx] = value.clone();
            return self.call_value(value.clone(), arg_count);
        }

        self.invoke_from_class(instance.klass, name, arg_count)
    }

    fn bind_method(&mut self, klass: Rc<RefCell<ObjClass>>, name: String) -> bool {
        if let Some(method) = klass.borrow().methods.get(&name) {
            let closure = method.as_closure();
            let receiver = self.peek_top(0).clone();
            let bound = Value::Obj(
                Obj::BoundMethod(Rc::new(ObjBoundMethod::new(receiver, closure.clone()))).into(),
            );

            self.stack.pop();
            self.stack.push(bound);
            true
        } else {
            self.runtime_error(&format!("Undefined property '{name}'."));
            false
        }
    }

    fn capture_upvalue(&mut self, local: usize) -> Rc<RefCell<ObjUpvalue>> {
        for upvalue in &self.open_upvalues {
            if upvalue.borrow().location == local {
                return upvalue.clone();
            }
        }
        let created_upvalue = Rc::new(RefCell::new(ObjUpvalue::new(local)));
        self.open_upvalues.push(created_upvalue.clone());
        created_upvalue
    }

    /// Closes all upvalues that have a stack index greater than or equal to `last`.
    fn close_upvalues(&mut self, last: usize) {
        // We iterate in reverse to avoid having to deal with changing indices after removal.
        let mut i = self.open_upvalues.len();
        while i != 0 {
            i -= 1; // Decrement first since we're going in reverse
            let upvalue_rc = &self.open_upvalues[i];
            let location = upvalue_rc.borrow().location;

            if location >= last {
                let closed_value = self.stack[location].clone();
                upvalue_rc.borrow_mut().closed = Some(closed_value);
                // NOTE(aalhendi): How expensive is remove?
                self.open_upvalues.remove(i);
            }
        }
    }

    fn define_method(&mut self, name: String) {
        let method = self.peek_top(0);
        let klass = self.peek_top(1).as_class();
        klass.borrow_mut().methods.insert(name, method.clone());
        self.stack.pop();
    }

    fn runtime_error(&mut self, message: &str) {
        eprintln!("{message}");

        for frame in self.frames.iter().rev() {
            // -1 because IP already sitting on the next instruction to be executed
            // but we want stack trace to point to the previous failed instruction.
            let i = frame.ip - 1;
            let name = if frame.closure.function.name.is_empty() {
                "script".to_owned()
            } else {
                frame.closure.function.name.clone()
            };
            eprintln!(
                "[line {line}] in {name}()",
                line = frame.closure.function.chunk.borrow().lines[i]
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
        &self.frame().closure.function.chunk
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.frame();
        frame.ip += 1;
        let ip_idx = frame.ip - 1;
        self.chunk().borrow().read_byte(ip_idx)
    }

    fn read_short(&mut self) -> u16 {
        let ip = {
            let frame = self.frame();
            frame.ip += 2;
            frame.ip
        };
        let chunk = self.chunk().borrow();
        let byte1 = chunk.read_byte(ip - 2) as u16;
        let byte2 = chunk.read_byte(ip - 1) as u16;
        (byte1 << 8) | byte2
    }

    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte() as usize;
        self.chunk().borrow().constants.values[idx].clone()
    }

    fn binary_op(&mut self, op_closure: fn(f64, f64) -> Value) -> Result<(), InterpretResult> {
        let b = self.peek_top(0);
        let a = self.peek_top(1);
        match (a, b) {
            (Value::Number(a), Value::Number(b)) => {
                let res = op_closure(*a, *b);
                self.stack.pop(); //b
                self.stack.pop(); //a
                self.stack.push(res);
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
