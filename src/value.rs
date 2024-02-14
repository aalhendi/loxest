use std::{
    cell::RefCell,
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Sub},
    rc::Rc,
};

use crate::object::{Obj, ObjClass, ObjClosure, ObjInstance};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    // TODO(aalhendi): Ptr to obj... something struct size
    // Obj(Rc<Obj>),
    Obj(Obj),
}

impl Value {
    // NOTE: Lox follows ruby in that only false and nil are false in lox
    // TODO(aalhendi): just impl bool?
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Number(_) => false,
            Value::Obj(_) => false,
            Value::Boolean(v) => !v,
            Value::Nil => true,
        }
    }

    #[allow(clippy::collapsible_match)]
    // TODO(aalhendi): String interning?
    pub fn as_string(&self) -> String {
        match self {
            Value::Obj(o) => match o {
                Obj::String(c) => c.to_string(),
                _ => unreachable!("Must be a string"),
            },
            _ => unreachable!("Must be a string"),
        }
    }

    #[allow(clippy::collapsible_match)]
    pub fn as_closure(&self) -> &Rc<ObjClosure> {
        match self {
            Value::Obj(o) => match o {
                Obj::Closure(c) => c,
                _ => unreachable!("Must be a closure"),
            },
            _ => unreachable!("Must be a closure"),
        }
    }

    #[allow(dead_code)]
    #[allow(clippy::collapsible_match)]
    pub fn as_closure_mut(&mut self) -> &mut Rc<ObjClosure> {
        match self {
            Value::Obj(o) => match o {
                Obj::Closure(c) => c,
                _ => unreachable!("Must be a closure"),
            },
            _ => unreachable!("Must be a closure"),
        }
    }

    #[allow(clippy::collapsible_match)]
    pub fn as_class(&self) -> &Rc<RefCell<ObjClass>> {
        match self {
            Value::Obj(o) => match o {
                Obj::Class(c) => c,
                _ => unreachable!("Must be a class"),
            },
            _ => unreachable!("Must be a class"),
        }
    }

    #[allow(clippy::collapsible_match)]
    pub fn as_instance_maybe(&self) -> Option<&Rc<RefCell<ObjInstance>>> {
        match self {
            Value::Obj(o) => match o {
                Obj::Instance(i) => Some(i),
                _ => None,
            },
            _ => None,
        }
    }

    #[allow(clippy::collapsible_match)]
    pub fn as_class_maybe(&self) -> Option<&Rc<RefCell<ObjClass>>> {
        match self {
            Value::Obj(o) => match o {
                Obj::Class(v) => Some(v),
                _ => None,
            },
            _ => None,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{v}"),
            Value::Boolean(v) => write!(f, "{v}"),
            Value::Nil => write!(f, "nil"),
            Value::Obj(v) => write!(f, "{v}"),
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Self::Number(n1 + n2),
            (_, _) => panic!("TODO: Change to unreachable."),
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Self::Number(n1 - n2),
            (_, _) => panic!("TODO: Change to unreachable."),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Self::Number(n1 * n2),
            (_, _) => panic!("TODO: Change to unreachable."),
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Self::Number(n1 / n2),
            (_, _) => panic!("TODO: Change to unreachable."),
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(v) => Self::Number(-v),
            _ => panic!("TODO: Change to unreachable."),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ValueArray {
    pub values: Vec<Value>,
}

impl ValueArray {
    pub fn new() -> Self {
        Self {
            values: Vec::with_capacity(8),
        }
    }

    pub fn write(&mut self, value: Value) {
        self.values.push(value);
    }

    pub fn free(&mut self) {
        self.values = Vec::new();
    }

    pub fn print_value(&self, constant_idx: usize) {
        print!("{v}", v = self.values[constant_idx])
    }
}
