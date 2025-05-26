use std::{cell::RefCell, fmt::Display, rc::Rc};

#[cfg(not(feature = "nan-boxing"))]
use std::ops::Neg;

use crate::object::{Obj, ObjClass, ObjClosure, ObjInstance};

#[cfg(not(feature = "nan-boxing"))]
pub const FALSE_VAL: Value = Value::Boolean(false);
#[cfg(not(feature = "nan-boxing"))]
pub const TRUE_VAL: Value = Value::Boolean(true);
#[cfg(not(feature = "nan-boxing"))]
pub const NIL_VAL: Value = Value::Nil;

#[cfg(feature = "nan-boxing")]
const SIGN_BIT: u64 = 0b1000000000000000000000000000000000000000000000000000000000000000;
#[cfg(feature = "nan-boxing")]
// 0x7ffc000000000000 - all exponent bits, QNAN bit and one more to avoid Intel magic value
const QNAN: u64 = 0b0111111111111100000000000000000000000000000000000000000000000000;
#[cfg(feature = "nan-boxing")]
const TAG_NIL: u64 = 0b01;
#[cfg(feature = "nan-boxing")]
const TAG_FALSE: u64 = 0b10;
#[cfg(feature = "nan-boxing")]
const TAG_TRUE: u64 = 0b11;
#[cfg(feature = "nan-boxing")]
pub const FALSE_VAL: Value = Value(QNAN | TAG_FALSE);
#[cfg(feature = "nan-boxing")]
pub const TRUE_VAL: Value = Value(QNAN | TAG_TRUE);
#[cfg(feature = "nan-boxing")]
pub const NIL_VAL: Value = Value(QNAN | TAG_NIL);

#[cfg(feature = "nan-boxing")]
const TYPE_MASK: u64 = 0b111; // Use 3 bits for type
#[cfg(feature = "nan-boxing")]
const TYPE_STRING: u64 = 0;
#[cfg(feature = "nan-boxing")]
const TYPE_NATIVE: u64 = 1;
#[cfg(feature = "nan-boxing")]
const TYPE_CLOSURE: u64 = 2;
#[cfg(feature = "nan-boxing")]
const TYPE_CLASS: u64 = 3;
#[cfg(feature = "nan-boxing")]
const TYPE_INSTANCE: u64 = 4;
#[cfg(feature = "nan-boxing")]
const TYPE_BOUND: u64 = 5;

#[cfg(feature = "nan-boxing")]
#[derive(Debug, Clone, PartialEq, PartialOrd, Copy)]
pub struct Value(pub u64);

#[cfg(feature = "nan-boxing")]
impl Value {
    pub fn to_num(self) -> f64 {
        f64::from_bits(self.0)
    }

    pub fn num(n: f64) -> Self {
        Self(n.to_bits())
    }

    pub fn from_bool(b: bool) -> Self {
        if b { TRUE_VAL } else { FALSE_VAL }
    }

    pub fn as_bool(&self) -> bool {
        *self == TRUE_VAL
    }

    pub fn is_bool(&self) -> bool {
        (self.0 | 1) == TRUE_VAL.0
        // self.0 == TRUE_VAL.0 || self.0 == FALSE_VAL.0
    }

    pub fn obj_val(obj: Rc<Obj>) -> Self {
        let type_tag = match *obj {
            Obj::String(_) => TYPE_STRING,
            Obj::Native(_) => TYPE_NATIVE,
            Obj::Closure(_) => TYPE_CLOSURE,
            Obj::Class(_) => TYPE_CLASS,
            Obj::Instance(_) => TYPE_INSTANCE,
            Obj::BoundMethod(_) => TYPE_BOUND,
        };
        let ptr_value = Rc::into_raw(obj) as u64;
        Self(SIGN_BIT | QNAN | (ptr_value & !(TYPE_MASK)) | type_tag)
    }

    pub fn as_obj(&self) -> Rc<Obj> {
        let ptr = (self.0 & !(SIGN_BIT | QNAN | TYPE_MASK)) as *const Obj;

        unsafe {
            Rc::increment_strong_count(ptr);
            Rc::from_raw(ptr)
        }
    }
    pub fn is_obj(&self) -> bool {
        self.0 & (QNAN | SIGN_BIT) == QNAN | SIGN_BIT
    }

    pub fn is_number(&self) -> bool {
        (self.0 & QNAN) != QNAN
    }

    pub fn as_closure(&self) -> Rc<ObjClosure> {
        match self.as_obj().as_ref() {
            Obj::Closure(c) => c.clone(),
            _ => unreachable!(),
        }
    }

    pub fn as_class(&self) -> Rc<RefCell<ObjClass>> {
        match self.as_obj().as_ref() {
            Obj::Class(c) => c.clone(),
            _ => unreachable!(),
        }
    }

    pub fn as_string(&self) -> Rc<str> {
        match self.as_obj().as_ref() {
            Obj::String(c) => c.clone(),
            _ => unreachable!(),
        }
    }

    pub fn as_instance_maybe(&self) -> Option<Rc<RefCell<ObjInstance>>> {
        if !self.is_obj() {
            return None;
        }
        match self.as_obj().as_ref() {
            Obj::Instance(i) => Some(i.clone()),
            _ => None,
        }
    }

    pub fn as_class_maybe(&self) -> Option<Rc<RefCell<ObjClass>>> {
        if !self.is_obj() {
            return None;
        }
        match self.as_obj().as_ref() {
            Obj::Class(v) => Some(v.clone()),
            _ => None,
        }
    }

    pub fn is_falsey(&self) -> bool {
        if self.is_number() {
            false
        } else if self.is_bool() {
            !self.as_bool()
        } else if *self == NIL_VAL {
            true
        } else if self.is_obj() {
            false
        } else {
            unreachable!()
        }
    }
}

#[cfg(feature = "nan-boxing")]
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_bool() {
            write!(f, "{}", self.as_bool())
        } else if self.is_number() {
            write!(f, "{}", f64::from_bits(self.0))
        } else if *self == NIL_VAL {
            write!(f, "nil")
        } else if self.is_obj() {
            write!(f, "{}", self.as_obj())
        } else {
            unreachable!()
        }
    }
}

#[cfg(not(feature = "nan-boxing"))]
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    Obj(Rc<Obj>),
}

#[cfg(not(feature = "nan-boxing"))]
impl Value {
    pub fn obj_val(obj: Rc<Obj>) -> Self {
        Self::Obj(obj)
    }

    pub fn from_bool(b: bool) -> Self {
        Self::Boolean(b)
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_obj(&self) -> bool {
        matches!(self, Value::Obj(_))
    }

    pub fn as_obj(&self) -> Rc<Obj> {
        match self {
            Value::Obj(o) => o.clone(),
            _ => unreachable!(),
        }
    }

    pub fn num(n: f64) -> Self {
        Self::Number(n)
    }

    pub fn to_num(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
            _ => unreachable!(),
        }
    }

    // NOTE: Lox follows ruby in that only false and nil are false in lox
    // TODO(aalhendi): just impl bool?
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Number(_) | Value::Obj(_) => false,
            Value::Boolean(v) => !v,
            Value::Nil => true,
        }
    }

    pub fn as_string(&self) -> &Rc<str> {
        match self {
            Value::Obj(o) => match &**o {
                Obj::String(c) => c,
                _ => unreachable!("Must be a string"),
            },
            _ => unreachable!("Must be a string"),
        }
    }

    pub fn as_closure(&self) -> Rc<ObjClosure> {
        match self {
            Value::Obj(o) => match &**o {
                Obj::Closure(c) => c.clone(),
                _ => unreachable!("Must be a closure"),
            },
            _ => unreachable!("Must be a closure"),
        }
    }

    pub fn as_class(&self) -> &Rc<RefCell<ObjClass>> {
        match self {
            Value::Obj(o) => match &**o {
                Obj::Class(c) => c,
                _ => unreachable!("Must be a class"),
            },
            _ => unreachable!("Must be a class"),
        }
    }

    pub fn as_instance_maybe(&self) -> Option<&Rc<RefCell<ObjInstance>>> {
        match self {
            Value::Obj(o) => match &**o {
                Obj::Instance(i) => Some(i),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_class_maybe(&self) -> Option<&Rc<RefCell<ObjClass>>> {
        match self {
            Value::Obj(o) => match &**o {
                Obj::Class(v) => Some(v),
                _ => None,
            },
            _ => None,
        }
    }
}

#[cfg(not(feature = "nan-boxing"))]
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

#[cfg(not(feature = "nan-boxing"))]
impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(v) => Self::Number(-v),
            _ => unreachable!(),
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

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    pub fn print_value(&self, constant_idx: usize, terminator: Option<char>) {
        if let Some(t) = terminator {
            println!("{v}{t}", v = self.values[constant_idx])
        } else {
            println!("{v}", v = self.values[constant_idx])
        }
    }
}
