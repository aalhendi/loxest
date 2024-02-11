use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc, time::SystemTime};

use crate::{chunk::Chunk, value::Value};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Obj {
    String(String),
    _Function(Rc<ObjFunction>), // All functions are wrapped in closures
    Native(ObjNative),
    Closure(Rc<ObjClosure>),
    _Upvalue(Rc<ObjUpvalue>),
    Class(Rc<ObjClass>),
    Instance(Rc<RefCell<ObjInstance>>),
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Obj::String(v) => write!(f, "{v}"),
            Obj::_Function(v) => write!(f, "{v}"),
            Obj::Native(v) => write!(f, "{v}"),
            Obj::Closure(v) => write!(f, "{v}"),
            Obj::_Upvalue(v) => write!(f, "{v}"),
            Obj::Class(v) => write!(f, "{v}"),
            Obj::Instance(v) => write!(f, "{}", v.borrow()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ObjClass {
    pub name: String,
}

impl ObjClass {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl Display for ObjClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{name}", name = self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjInstance {
    pub fields: HashMap<String, Value>,
    pub klass: Rc<ObjClass>,
}

impl PartialOrd for ObjInstance {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        todo!("Placeholder")
    }
}

impl ObjInstance {
    pub fn new(klass: Rc<ObjClass>) -> Self {
        Self {
            klass,
            fields: HashMap::new(),
        }
    }
}

impl Display for ObjInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{name}'s instance", name = self.klass.name)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ObjUpvalue {
    pub location: usize, // index into the stack, serving as a ptr
    pub closed: Option<Value>,
}

impl ObjUpvalue {
    pub fn new(slot: usize) -> Self {
        Self {
            location: slot,
            closed: None,
        }
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
    pub upvalues: Vec<Rc<RefCell<ObjUpvalue>>>,
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
