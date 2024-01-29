use std::{cell::RefCell, fmt::Display, rc::Rc, time::SystemTime};

use crate::{chunk::Chunk, value::Value};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Obj {
    String(String),
    Function(Rc<ObjFunction>),
    Native(ObjNative),
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Obj::String(v) => write!(f, "{v}"),
            Obj::Function(v) => write!(f, "{v}"),
            Obj::Native(v) => write!(f, "{v}"),
        }
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
}

impl ObjFunction {
    pub fn new(name: &str, arity: usize) -> Self {
        Self {
            arity,
            chunk: Rc::new(RefCell::new(Chunk::new())),
            name: name.to_string(),
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
