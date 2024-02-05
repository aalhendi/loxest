use std::{cell::RefCell, fmt::Display, rc::Rc, time::SystemTime};

use crate::{chunk::Chunk, value::Value};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Obj {
    String(String),
    _Function(Rc<ObjFunction>), // All functions are wrapped in closures
    Native(ObjNative),
    Closure(Rc<ObjClosure>),
    Upvalue(Rc<ObjUpvalue>),
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Obj::String(v) => write!(f, "{v}"),
            Obj::_Function(v) => write!(f, "{v}"),
            Obj::Native(v) => write!(f, "{v}"),
            Obj::Closure(v) => write!(f, "{v}"),
            Obj::Upvalue(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ObjUpvalue {
    pub location: usize, // index into the stack, serving as a ptr
}

impl ObjUpvalue {
    pub fn new(slot: usize) -> Self {
        Self { location: slot }
    }
}

impl Display for ObjUpvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "upvalue")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ObjClosure {
    pub function: Rc<ObjFunction>,
    pub upvalues: Vec<ObjUpvalue>,
}

impl ObjClosure {
    pub fn new(function: Rc<ObjFunction>) -> Self {
        Self {
            upvalues: Vec::with_capacity(function.upvalue_count),
            function,
        }
    }
}

impl Display for ObjClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = format!("{}", self.function);
        write!(f, "{name}")
    }
}

pub fn native_clock(_arg_count: usize, _args: &[Value]) -> Value {
    match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(n) => Value::Number(n.as_secs_f64()),
        Err(e) => panic!("{e}"),
    }
}

pub type NativeFn = fn(arg_count: usize, args: &[Value]) -> Value;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ObjNative {
    pub function: NativeFn,
}

impl ObjNative {
    pub fn new(function: NativeFn) -> Self {
        Self { function }
    }
}

impl Display for ObjNative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ObjFunction {
    pub arity: usize,
    pub chunk: Rc<RefCell<Chunk>>,
    pub name: String,
    pub upvalue_count: usize,
}

impl ObjFunction {
    pub fn new(name: &str, arity: usize) -> Self {
        Self {
            arity,
            chunk: Rc::new(RefCell::new(Chunk::new())),
            name: name.to_string(),
            upvalue_count: 0,
        }
    }
}

impl Display for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = if self.name.is_empty() {
            "<script>".to_owned()
        } else {
            format!("fn<{}>", self.name.as_str())
        };
        write!(f, "{name}")
    }
}
