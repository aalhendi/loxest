use crate::{scanner::Scanner, token::TokenType};

pub struct Compiler {}

impl Compiler {

    pub fn new() -> Self {
        Self {  }
    }

    pub fn compile(&self, soruce: &str) {
        let mut scanner = Scanner::new(soruce);
        let mut line = 0;
        loop {
            let token = scanner.scan_token();
            if token.line != line {
                print!("{l:4} ", l = token.line);
                line = token.line;
            } else {
                print!("   | ");
            }
            // TODO: Do I have to clone?
            // println!("{kind:2} '{lexeme}'", kind = token.kind.clone() as usize, lexeme = token.lexeme);
            println!("{kind:8?} '{lexeme}'", kind = token.kind, lexeme = token.lexeme);

            if token.kind == TokenType::Eof {
                break;
            }
        }
    }
}
