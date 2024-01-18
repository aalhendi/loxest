#![allow(unused)]
use std::fmt::Display;

use crate::value::{Value, ValueArray};

#[derive(Debug)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    Nil,
    True,
    False,
    Pop,
    GetLocal,
    SetLocal,
    GetGlobal,
    DefineGlobal,
    SetGlobal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    GetSuper,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Invoke,
    SuperInvoke,
    Closure,
    CloseUpvalue,
    Return,
    Class,
    Inherit,
    Method,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            OpCode::Constant => "OP_CONSTANT",
            OpCode::Nil => "OP_NIL",
            OpCode::True => "OP_TRUE",
            OpCode::False => "OP_FALSE",
            OpCode::Pop => "OP_POP",
            OpCode::GetLocal => "OP_GET_LOCAL",
            OpCode::SetLocal => "OP_SET_LOCAL",
            OpCode::GetGlobal => "OP_GET_GLOBAL",
            OpCode::DefineGlobal => "OP_DEFINE_GLOBAL",
            OpCode::SetGlobal => "OP_SET_GLOBAL",
            OpCode::GetUpvalue => todo!(),
            OpCode::SetUpvalue => todo!(),
            OpCode::GetProperty => todo!(),
            OpCode::SetProperty => todo!(),
            OpCode::GetSuper => todo!(),
            OpCode::Equal => "OP_EQUAL",
            OpCode::Greater => "OP_GREATER",
            OpCode::Less => "OP_LESS",
            OpCode::Add => "OP_ADD",
            OpCode::Subtract => "OP_SUBTRACT",
            OpCode::Multiply => "OP_MULTIPLY",
            OpCode::Divide => "OP_DIVIDE",
            OpCode::Not => "OP_NOT",
            OpCode::Negate => "OP_NEGATE",
            OpCode::Print => "OP_PRINT",
            OpCode::Jump => "OP_JUMP",
            OpCode::JumpIfFalse => "OP_JUMP_IF_FALSE",
            OpCode::Loop => "OP_LOOP",
            OpCode::Call => "OP_CALL",
            OpCode::Invoke => todo!(),
            OpCode::SuperInvoke => todo!(),
            OpCode::Closure => todo!(),
            OpCode::CloseUpvalue => todo!(),
            OpCode::Return => "OP_RETURN",
            OpCode::Class => todo!(),
            OpCode::Inherit => todo!(),
            OpCode::Method => todo!(),
        };
        write!(f, "{out}")
    }
}

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        match value {
            0 => OpCode::Constant,
            1 => OpCode::Nil,
            2 => OpCode::True,
            3 => OpCode::False,
            4 => OpCode::Pop,
            5 => OpCode::GetLocal,
            6 => OpCode::SetLocal,
            7 => OpCode::GetGlobal,
            8 => OpCode::DefineGlobal,
            9 => OpCode::SetGlobal,
            10 => OpCode::GetUpvalue,
            11 => OpCode::SetUpvalue,
            12 => OpCode::GetProperty,
            13 => OpCode::SetProperty,
            14 => OpCode::GetSuper,
            15 => OpCode::Equal,
            16 => OpCode::Greater,
            17 => OpCode::Less,
            18 => OpCode::Add,
            19 => OpCode::Subtract,
            20 => OpCode::Multiply,
            21 => OpCode::Divide,
            22 => OpCode::Not,
            23 => OpCode::Negate,
            24 => OpCode::Print,
            25 => OpCode::Jump,
            26 => OpCode::JumpIfFalse,
            27 => OpCode::Loop,
            28 => OpCode::Call,
            29 => OpCode::Invoke,
            30 => OpCode::SuperInvoke,
            31 => OpCode::Closure,
            32 => OpCode::CloseUpvalue,
            33 => OpCode::Return,
            34 => OpCode::Class,
            35 => OpCode::Inherit,
            36 => OpCode::Method,
            _ => panic!("Unknown opcode {value}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: ValueArray,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(8),
            constants: ValueArray::new(),
            lines: Vec::with_capacity(8),
        }
    }

    pub fn count(&self) -> usize {
        self.lines.len()
    }

    pub fn write_byte(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn read_byte(&self, ip_idx: usize) -> u8 {
        self.code[ip_idx]
    }

    pub fn write_op(&mut self, code: OpCode, line: usize) {
        self.code.push(code as u8);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> Option<u8> {
        self.constants.write(value);
        u8::try_from(self.constants.values.len() - 1).ok()
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

    fn byte_instruction(&self, name: OpCode, offset: usize) -> usize {
        let slot = self.code[offset + 1];
        println!("{name:-16} {slot:4}");
        offset + 2
    }

    fn constant_instruction(&self, code: OpCode, offset: usize) -> usize {
        let constant_idx = self.code[offset + 1] as usize;
        let name = code.to_string(); // This makes formatting work for some reason
        print!("{name:-16} {constant_idx:4} '");
        self.constants.print_value(constant_idx);
        println!("'");
        offset + 2
    }

    // TODO(aalhendi): Readability: Use a JumpDirection Enum or consts to remain as bool.
    fn jump_instruction(&self, name: OpCode, is_neg: bool, offset: usize) -> usize {
        let jump = ((self.code[offset + 1] as u16) << 8) | (self.code[offset + 2] as u16);

        // NOTE: This could underflow and that would be a bug in the impl so it shouldn't.
        let dst = match is_neg {
            true => offset + 3 - jump as usize,
            false => offset + 3 + jump as usize,
        };
        println!("{name:-16} {offset:4} -> {dst}",);
        offset + 3
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{offset:04} ");

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{line:4} ", line = self.lines[offset]);
        }

        let instruction = OpCode::from(self.code[offset]);
        match instruction {
            OpCode::Constant => self.constant_instruction(OpCode::Constant, offset),
            OpCode::Return => self.simple_instruction(OpCode::Return, offset),
            OpCode::Negate => self.simple_instruction(OpCode::Negate, offset),
            OpCode::Add => self.simple_instruction(OpCode::Add, offset),
            OpCode::Subtract => self.simple_instruction(OpCode::Subtract, offset),
            OpCode::Multiply => self.simple_instruction(OpCode::Multiply, offset),
            OpCode::Divide => self.simple_instruction(OpCode::Divide, offset),
            OpCode::False => self.simple_instruction(OpCode::False, offset),
            OpCode::True => self.simple_instruction(OpCode::True, offset),
            OpCode::Nil => self.simple_instruction(OpCode::Nil, offset),
            OpCode::Not => self.simple_instruction(OpCode::Not, offset),
            OpCode::Equal => self.simple_instruction(OpCode::Equal, offset),
            OpCode::Greater => self.simple_instruction(OpCode::Greater, offset),
            OpCode::Less => self.simple_instruction(OpCode::Less, offset),
            OpCode::Print => self.simple_instruction(OpCode::Print, offset),
            OpCode::Pop => self.simple_instruction(OpCode::Pop, offset),
            OpCode::DefineGlobal => self.constant_instruction(OpCode::DefineGlobal, offset),
            OpCode::GetGlobal => self.constant_instruction(OpCode::GetGlobal, offset),
            OpCode::SetGlobal => self.constant_instruction(OpCode::SetGlobal, offset),
            OpCode::GetLocal => self.byte_instruction(OpCode::GetLocal, offset),
            OpCode::SetLocal => self.byte_instruction(OpCode::SetLocal, offset),
            OpCode::Jump => self.jump_instruction(OpCode::Jump, false, offset),
            OpCode::JumpIfFalse => self.jump_instruction(OpCode::JumpIfFalse, false, offset),
            OpCode::Loop => self.jump_instruction(OpCode::Loop, true, offset),
            OpCode::Call => self.byte_instruction(OpCode::Call, offset),
            _ => unimplemented!(),
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
