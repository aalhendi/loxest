use std::{fmt::Display, ops::{Add, Sub, Mul, Div, Neg}};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
}

impl Value {
    // NOTE: Lox follows ruby in that only false and nil are false in lox
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Number(_) => false,
            Value::Boolean(v) => !v,
            Value::Nil => true,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{v}"),
            Value::Boolean(v) => write!(f, "{v}"),
            Value::Nil => write!(f, "nil"),
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
