use std::fmt::Display;

use crate::value::{Value, ValueArray};

#[derive(Debug)]
#[repr(u8)]
pub enum OpCode {
    Cosntant,
    Return,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            OpCode::Cosntant => "OP_CONSTANT",
            OpCode::Return => "OP_RETURN",
        };
        write!(f, "{out}")
    }
}

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        match value {
            0 => OpCode::Cosntant,
            1 => OpCode::Return,
            _ => panic!("Unknown opcode {value}"),
        }
    }
}

pub struct Chunk {
    code: Vec<u8>,
    constants: ValueArray,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(8),
            constants: ValueArray::new(),
            lines: Vec::with_capacity(8),
        }
    }

    pub fn write_byte(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn write_op(&mut self, code: OpCode, line: usize) {
        self.code.push(code as u8);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.write(value);
        (self.constants.values.len() - 1) as u8
    }

    pub fn free(&mut self) {
        self.code = Vec::new();
        self.lines = Vec::new();
        self.constants.free();
    }

    fn simple_instruction(&self, code: OpCode, offset: usize) -> usize {
        println!("{code}");
        offset + 1
    }

    fn constant_instruction(&self, code: OpCode, offset: usize) -> usize {
        let constant_idx = self.code[offset + 1] as usize;
        let name = code.to_string(); // This makes formatting work for some reason
        println!(
            "{name:<16} {constant_idx:4} '{v}'",
            v = self.constants.values[constant_idx]
        );
        offset + 2
    }

    fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{offset:04} ");

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{line:4} ", line = self.lines[offset]);
        }

        let instruction = OpCode::from(self.code[offset]);
        match instruction {
            OpCode::Cosntant => self.constant_instruction(OpCode::Cosntant, offset),
            OpCode::Return => self.simple_instruction(OpCode::Return, offset),
        }
    }

    pub fn disassemble<T: Display>(&self, name: T) {
        println!("== {name} ==");

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }
}
