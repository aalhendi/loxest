use std::{fmt::Display, rc::Rc, cell::RefCell};

use crate::chunk::Chunk;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Obj {
    String(String),
    Function(ObjFunction),
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Obj::String(v) => write!(f, "{v}"),
            Obj::Function(v) => write!(f, "{v}"),
        }
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
            "<script>"
        } else {
            self.name.as_str()
        };
        write!(f, "fn<{name}>")
    }
}
