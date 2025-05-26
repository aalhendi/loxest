use std::{
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use crate::{
    chunk::OpCode,
    compiler::{Compiler, FunctionType},
    object::{
        NativeFn, Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjInstance, ObjNative, ObjUpvalue,
        native_clock,
    },
    value::{FALSE_VAL, NIL_VAL, TRUE_VAL, Value},
};

macro_rules! frame_mut {
    ($self:ident) => {
        $self.frames.last_mut().unwrap()
    };
}

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * (u8::MAX as usize + 1);

pub enum InterpretResult {
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
    globals: HashMap<Rc<str>, Value>,
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

        vm.define_native("clock".into(), native_clock);

        vm
    }

    fn define_native(&mut self, name: Rc<str>, function: NativeFn) {
        // C version pushes 2 values to the stack. String: name and Obj: func
        // it sets these values in the globals table then pops the 2 values off
        // the stack. That is done for GC purposes to potentially trigger a collection.
        // Since we are going with an RC approach this can all be summerized in a globals.insert
        self.globals.insert(
            name,
            Value::obj_val(Obj::Native(ObjNative::new(function)).into()),
        );
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretResult> {
        let mut compiler = Compiler::new(source, FunctionType::Script);
        if let Some(function) = compiler.compile() {
            let closure = Rc::new(ObjClosure::new(function));
            self.stack
                .push(Value::obj_val(Obj::Closure(closure.clone()).into()));
            match self.call(closure, 0) {
                Ok(_) => {
                    let result = self.run();
                    self.stack.pop();
                    result
                }
                Err(e) => Err(e),
            }
        } else {
            // NOTE(aalhendi): is this rly needed?
            compiler.current_chunk().free();
            Err(InterpretResult::CompileError)
        }
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
                let frame = frame_mut!(self);
                let chunk = &frame.closure.function.chunk;
                chunk.disassemble_instruction(frame.ip);
            }

            let instruction = OpCode::from(self.read_byte());
            match instruction {
                OpCode::Constant => self.op_constant(),
                OpCode::Negate => self.op_negate()?,
                OpCode::Add => self.add_values()?,
                OpCode::Subtract => self.op_binary(|a, b| Value::num(a - b))?,
                OpCode::Multiply => self.op_binary(|a, b| Value::num(a * b))?,
                OpCode::Divide => self.op_binary(|a, b| Value::num(a / b))?,
                OpCode::Greater => self.op_binary(|a, b| Value::from_bool(a > b))?,
                OpCode::Less => self.op_binary(|a, b| Value::from_bool(a < b))?,
                OpCode::False => self.stack.push(FALSE_VAL),
                OpCode::True => self.stack.push(TRUE_VAL),
                OpCode::Nil => self.stack.push(NIL_VAL),
                OpCode::Not => self.op_not(),
                OpCode::Equal => self.op_equal(),
                OpCode::Print => println!("{v}", v = self.stack.pop().unwrap()),
                OpCode::Pop => self.op_pop(),
                OpCode::DefineGlobal => self.op_define_global(),
                OpCode::GetGlobal => self.op_get_global()?,
                OpCode::SetGlobal => self.op_set_global()?,
                OpCode::GetLocal => self.op_get_local(),
                OpCode::SetLocal => self.op_set_local(),
                OpCode::JumpIfFalse => self.op_jump_if_false(),
                OpCode::Jump => self.op_jump(),
                OpCode::Loop => self.op_loop(),
                OpCode::Call => self.op_call()?,
                OpCode::Closure => self.op_closure(),
                OpCode::GetUpvalue => self.op_get_upvalue(),
                OpCode::SetUpvalue => self.op_set_upvalue(),
                OpCode::CloseUpvalue => self.op_close_upvalue(),
                OpCode::Class => self.op_class(),
                OpCode::GetProperty => self.op_get_property()?,
                OpCode::SetProperty => self.op_set_property()?,
                OpCode::Method => self.op_method(),
                OpCode::Invoke => self.op_invoke()?,
                OpCode::Inherit => self.op_inherit()?,
                OpCode::GetSuper => self.op_get_super()?,
                OpCode::SuperInvoke => self.op_super_invoke()?,
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
                    self.stack.truncate(slot);
                    self.stack.push(result);
                }
            }
        }
    }

    // --- Ops Start

    fn op_constant(&mut self) {
        let constant = self.read_constant().clone();
        self.stack.push(constant);
    }

    fn op_negate(&mut self) -> Result<(), InterpretResult> {
        if !self.peek_top(0).is_number() {
            return self.runtime_error("Operand must be a number.");
        }
        let v = Value::num(-self.stack.last_mut().unwrap().to_num());
        *self.stack.last_mut().unwrap() = v;
        Ok(())
    }

    /// Adds the top two values on the stack.
    ///
    /// This function supports adding two numeric values or concatenating two strings.
    /// In the case of numeric values, it expects both operands to be `Value::Number`
    /// and pushes the result of their addition back onto the stack.
    /// For string values, it expects both operands to be `Value::Obj` containing `Obj::String`,
    /// concatenates them via new allocation, and pushes the result back onto the stack as a new `Obj::String`.
    ///
    /// # Errors
    ///
    /// Returns an `Err(InterpretResult::RuntimeError)` if:
    /// - The top two values on the stack are not both numbers or both strings.
    /// - The stack is underflowed (does not contain at least two values). [Should not happen]
    fn add_values(&mut self) -> Result<(), InterpretResult> {
        let b = self.peek_top(0).clone();
        let a = self.peek_top(1).clone();
        let v = if a.is_number() && b.is_number() {
            Value::num(b.to_num() + a.to_num())
        } else if a.is_obj() && b.is_obj() {
            let concatenated = format!("{}{}", a.as_string(), b.as_string());
            Value::obj_val(Rc::new(Obj::String(concatenated.into())))
        } else {
            return self.runtime_error("Operands must be two numbers or two strings.");
        };
        self.stack.pop();
        *self.stack.last_mut().unwrap() = v;

        /*
        let right = self.stack.pop().unwrap();
        // only peek "a" to later modify in-place
        let left = self.peek_top(0);

        match (&left, &right) {
            (Value::Number(left_num), Value::Number(right_num)) => {
                *self.stack.last_mut().unwrap() = Value::Number(left_num + right_num);
            }
            (Value::Obj(left_obj), Value::Obj(right_obj)) => {
                if let (Obj::String(left_str), Obj::String(right_str)) =
                    (&left_obj.as_ref(), &right_obj.as_ref())
                {
                    let concatenated = format!("{}{}", left_str, right_str);
                    *self.stack.last_mut().unwrap() =
                        Value::obj_val(Rc::new(Obj::String(concatenated.into())));
                } else {
                    return self.runtime_error("Operands must be two numbers or two strings.");
                }
            }
            _ => return self.runtime_error("Operands must be two numbers or two strings."),
        }
        */
        Ok(())
    }

    fn op_binary(&mut self, op_closure: fn(f64, f64) -> Value) -> Result<(), InterpretResult> {
        if !self.peek_top(0).is_number() || !self.peek_top(1).is_number() {
            return self.runtime_error("Operands must be numbers.");
        }

        let b = self.stack.pop().unwrap();
        let a = self.stack.last().unwrap();
        let v = op_closure(a.to_num(), b.to_num());
        *self.stack.last_mut().unwrap() = v;
        Ok(())
    }

    fn op_not(&mut self) {
        *self.stack.last_mut().unwrap() = Value::from_bool(self.stack.last().unwrap().is_falsey());
    }

    fn op_equal(&mut self) {
        // TODO(aalhendi): Unwrap unchecked everywhere
        let b = self.stack.pop().unwrap();
        // pop "a" and push result (in-place)
        let a = self.stack.pop().unwrap();
        // NOTE(aalhendi): IEEE specs lists NaN != NaN
        // this ensures “real” arithmetic NaNs produced in lox are not equal to themselves
        if cfg!(feature = "nan-boxing") && a.is_number() && b.is_number() {
            self.stack.push(Value::from_bool(a.to_num() == b.to_num()));
            return;
        }
        if a.is_obj() && b.is_obj() {
            self.stack.push(Value::from_bool(a.as_obj() == b.as_obj()));
        } else {
            self.stack.push(Value::from_bool(a == b));
        }
    }

    fn op_pop(&mut self) {
        self.stack.pop();
    }

    fn op_define_global(&mut self) {
        let name = self.read_string();
        self.globals.insert(name, self.stack.pop().unwrap());
    }

    fn op_get_global(&mut self) -> Result<(), InterpretResult> {
        let name = self.read_string();
        match self.globals.get(&name) {
            Some(v) => {
                self.stack.push(v.clone());
                Ok(())
            }
            None => self.runtime_error(&format!("Undefined variable '{name}'.")),
        }
    }

    fn op_set_global(&mut self) -> Result<(), InterpretResult> {
        let name = self.read_string();
        let value = self.peek_top(0).clone();
        if let Entry::Occupied(mut e) = self.globals.entry(name.clone()) {
            e.insert(value);
            Ok(())
        } else {
            self.runtime_error(&format!("Undefined variable '{name}'."))
        }
    }

    fn op_get_local(&mut self) {
        let slot = self.read_byte() as usize;
        let slot_offset = self.frame().slots;
        self.stack.push(self.stack[slot + slot_offset].clone());
    }

    fn op_set_local(&mut self) {
        let slot = self.read_byte() as usize;
        let slot_offset = self.frame().slots;
        self.stack[slot + slot_offset] = self.peek_top(0).clone();
    }

    fn op_jump_if_false(&mut self) {
        let offset = self.read_short() as usize;
        if self.peek_top(0).is_falsey() {
            frame_mut!(self).ip += offset;
        }
    }

    fn op_jump(&mut self) {
        let offset = self.read_short() as usize;
        frame_mut!(self).ip += offset;
    }

    fn op_loop(&mut self) {
        let offset = self.read_short() as usize;
        frame_mut!(self).ip -= offset;
    }

    fn op_call(&mut self) -> Result<(), InterpretResult> {
        let arg_count = self.read_byte() as usize;
        let func_to_call = self.peek_top(arg_count).clone();
        self.call_value(&func_to_call, arg_count)
    }

    fn op_closure(&mut self) {
        let function = self.read_constant().as_closure().function.clone();
        let upvalue_count = function.upvalue_count;
        let mut closure = ObjClosure::new(function);
        for _ in 0..upvalue_count {
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
            .push(Value::obj_val(Obj::Closure(Rc::new(closure)).into()));
    }

    fn op_get_upvalue(&mut self) {
        let slot = self.read_byte() as usize;
        let upvalue = self.frame().closure.upvalues[slot].borrow().clone();
        let value = match &upvalue.closed {
            Some(value) => value.clone(),
            None => self.stack[upvalue.location].clone(),
        };
        self.stack.push(value);
    }

    fn op_set_upvalue(&mut self) {
        let slot = self.read_byte() as usize;
        let value = self.peek_top(0).clone();
        let upvalue = self.frame().closure.upvalues[slot].borrow().clone();
        if upvalue.closed.is_none() {
            self.stack[upvalue.location] = value;
        } else {
            self.frame().closure.upvalues[slot].borrow_mut().closed = Some(value);
        }
    }

    fn op_close_upvalue(&mut self) {
        self.close_upvalues(self.stack.len() - 1);
        self.stack.pop();
    }

    fn op_class(&mut self) {
        let class_name = self.read_string();
        let value =
            Value::obj_val(Obj::Class(Rc::new(RefCell::new(ObjClass::new(class_name)))).into());
        self.stack.push(value);
    }

    fn op_get_property(&mut self) -> Result<(), InterpretResult> {
        let instance_rc = if let Some(i) = self.peek_top(0).as_instance_maybe() {
            i.clone()
        } else {
            return self.runtime_error("Only instances have properties.");
        };
        let name = self.read_string();

        let instance = instance_rc.borrow();
        if let Some(value) = instance.fields.get(&name) {
            // pop instance & push the value (in-place)
            *self.stack.last_mut().unwrap() = value.clone();
            Ok(())
        } else {
            self.bind_method(&instance.klass, &name)
        }
    }

    fn op_set_property(&mut self) -> Result<(), InterpretResult> {
        let instance_rc = if let Some(i) = self.peek_top(1).as_instance_maybe() {
            i.clone()
        } else {
            return self.runtime_error("Only instances have fields.");
        };

        let name = self.read_string();
        let value = self.peek_top(0).clone();
        {
            let mut instance = instance_rc.borrow_mut();
            instance.fields.insert(name, value);
        }

        let value = self.stack.pop().unwrap().clone();
        // pop instance & push the value back (in-place)
        *self.stack.last_mut().unwrap() = value;
        Ok(())
    }

    fn op_method(&mut self) {
        let name = self.read_string();
        // Define method
        let method = self.peek_top(0).clone();
        let klass = self.peek_top(1).as_class();
        klass.borrow_mut().methods.insert(name, method);
        self.stack.pop();
    }

    fn op_invoke(&mut self) -> Result<(), InterpretResult> {
        let method = self.read_string();
        let arg_count = self.read_byte() as usize;
        self.invoke(&method, arg_count)
    }

    fn op_inherit(&mut self) -> Result<(), InterpretResult> {
        let Some(superclass) = self.peek_top(1).as_class_maybe() else {
            return self.runtime_error("Superclass must be a class.");
        };
        // copy-down inheritance. works here because Lox classes are /closed/
        let super_methods = superclass.borrow().methods.clone();

        let subclass = self.peek_top(0).as_class();
        subclass.borrow_mut().methods.extend(super_methods);
        self.stack.pop(); // subclass
        Ok(())
    }

    fn op_get_super(&mut self) -> Result<(), InterpretResult> {
        let name = self.read_string();
        let superclass_value = self.stack.pop().unwrap();
        self.bind_method(&superclass_value.as_class(), &name)
    }

    fn op_super_invoke(&mut self) -> Result<(), InterpretResult> {
        let method = self.read_string();
        let arg_count = self.read_byte() as usize;
        let superclass_value = self.stack.pop().unwrap();
        self.invoke_from_class(&superclass_value.as_class(), &method, arg_count)
    }

    // --- Ops End

    fn read_string(&mut self) -> Rc<str> {
        self.read_constant().as_string().clone()
    }

    fn peek_top(&mut self, distance: usize) -> &mut Value {
        let len = self.stack.len();
        &mut self.stack[len - 1 - distance]
    }

    fn call(&mut self, closure: Rc<ObjClosure>, arg_count: usize) -> Result<(), InterpretResult> {
        if arg_count != closure.function.arity {
            return self.runtime_error(&format!(
                "Expected {arity} arguments but got {arg_count}.",
                arity = closure.function.arity
            ));
        }
        if self.frames.len() == FRAMES_MAX {
            return self.runtime_error("Stack overflow.");
        }
        let slots = self.stack.len() - arg_count - 1;
        let frame = CallFrame::new(closure, 0, slots);
        self.frames.push(frame);
        Ok(())
    }

    fn call_value(&mut self, callee: &Value, arg_count: usize) -> Result<(), InterpretResult> {
        if !callee.is_obj() {
            return self.runtime_error("Can only call functions and classes.");
        }
        match callee.as_obj().as_ref() {
            Obj::String(_) | Obj::Instance(_) => {
                self.runtime_error("Can only call functions and classes.")
            }
            Obj::Native(f) => {
                // From the stack top - arg count to the stack top
                let slice = self.stack.len() - arg_count..self.stack.len();
                let result = (f.function)(arg_count, &self.stack[slice]);
                self.stack.truncate(self.stack.len() - (arg_count + 1));
                self.stack.push(result);
                Ok(())
            }
            // NOTE(aalhendi): All functions are closures
            Obj::Closure(c) => self.call(c.clone(), arg_count),
            Obj::Class(c) => {
                let idx = self.stack.len() - arg_count - 1;
                self.stack[idx] = Value::obj_val(
                    Obj::Instance(Rc::new(RefCell::new(ObjInstance::new(c.clone())))).into(),
                );
                if let Some(initializer) = c.borrow().methods.get("init") {
                    let init = initializer.as_closure();
                    self.call(init, arg_count)
                } else if arg_count != 0 {
                    return self
                        .runtime_error(&format!("Expected 0 arguments but got {arg_count}."));
                } else {
                    Ok(())
                }
            }
            Obj::BoundMethod(m) => {
                let idx = self.stack.len() - arg_count - 1;
                self.stack[idx] = m.receiver.clone();
                self.call(m.method.clone(), arg_count)
            }
        }
    }

    fn invoke_from_class(
        &mut self,
        klass: &Rc<RefCell<ObjClass>>,
        name: &str,
        arg_count: usize,
    ) -> Result<(), InterpretResult> {
        if let Some(method) = klass.borrow().methods.get(name) {
            let closure = method.as_closure();
            self.call(closure, arg_count)
        } else {
            self.runtime_error(&format!("Undefined property '{name}'."))
        }
    }

    fn invoke(&mut self, name: &str, arg_count: usize) -> Result<(), InterpretResult> {
        let receiver = self.peek_top(arg_count);
        let instance = if let Some(v) = receiver.as_instance_maybe() {
            v.borrow().clone()
        } else {
            return self.runtime_error("Only instances have methods.");
        };

        if let Some(value) = instance.fields.get(name) {
            let idx = self.stack.len() - arg_count - 1;
            self.stack[idx] = value.clone();
            return self.call_value(value, arg_count);
        }

        self.invoke_from_class(&instance.klass, name, arg_count)
    }

    fn bind_method(
        &mut self,
        klass: &Rc<RefCell<ObjClass>>,
        name: &Rc<str>,
    ) -> Result<(), InterpretResult> {
        if let Some(method) = klass.borrow().methods.get(name) {
            let closure = method.as_closure();
            let receiver = self.peek_top(0).clone();
            let bound = Value::obj_val(
                Obj::BoundMethod(Rc::new(ObjBoundMethod::new(receiver, closure.clone()))).into(),
            );

            // pop receiver push bound method in place
            *self.stack.last_mut().unwrap() = bound;
            Ok(())
        } else {
            self.runtime_error(&format!("Undefined property '{name}'."))
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

    fn runtime_error(&mut self, message: &str) -> Result<(), InterpretResult> {
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
                line = frame.closure.function.chunk.lines[i]
            );
        }

        // Clear has no effect on capacity of vec
        self.stack.clear();

        Err(InterpretResult::RuntimeError)
    }

    /// Returns a reference to the current `CallFrame`.
    ///
    /// This function retrieves the last `CallFrame` from the VM's call stack,
    /// which represents the current execution context.
    #[inline]
    fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn read_byte(&mut self) -> u8 {
        let frame = frame_mut!(self);
        let chunk = &frame.closure.function.chunk;
        let v = chunk.read_byte(frame.ip);
        frame.ip += 1;
        v
    }

    fn read_short(&mut self) -> u16 {
        let frame = frame_mut!(self);
        frame.ip += 2;
        let chunk = &frame.closure.function.chunk;
        let byte1 = chunk.read_byte(frame.ip - 2) as u16;
        let byte2 = chunk.read_byte(frame.ip - 1) as u16;
        (byte1 << 8) | byte2
    }

    fn read_constant(&mut self) -> &Value {
        let idx = self.read_byte() as usize;
        let frame = frame_mut!(self);
        let chunk = &frame.closure.function.chunk;
        &chunk.constants.values[idx]
    }
}
