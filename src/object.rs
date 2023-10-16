use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Obj {
    String(String),
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Obj::String(v) => write!(f, "{v}"),
        }
    }
}
