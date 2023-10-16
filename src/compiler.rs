use crate::{
    chunk::{Chunk, OpCode},
    object::Obj,
    scanner::Scanner,
    token::{Token, TokenType},
    value::Value,
};

#[derive(Debug, PartialEq, PartialOrd)]
#[repr(u8)]
enum Precedence {
    None,
    Assignment, // =
    _Or,        // or
    _And,       // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    _Call,      // . ()
    Primary,
}

struct ParseRule {
    prefix: Option<fn(&mut Compiler)>,
    infix: Option<fn(&mut Compiler)>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(
        prefix: Option<fn(&mut Compiler)>,
        infix: Option<fn(&mut Compiler)>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

pub struct Parser {
    previous: Token,
    current: Token,
    had_error: bool,
    panic_mode: bool,
}

impl Parser {
    fn new() -> Self {
        Self {
            previous: Token::new(TokenType::Undefined, "", 0),
            current: Token::new(TokenType::Undefined, "", 0),
            had_error: false,
            panic_mode: false,
        }
    }
}

pub struct Compiler<'a> {
    parser: Parser,
    chunk: &'a mut Chunk,
    scanner: Scanner<'a>,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str, chunk: &'a mut Chunk) -> Self {
        Self {
            parser: Parser::new(),
            chunk,
            scanner: Scanner::new(source),
        }
    }

    pub fn compile(&mut self) -> bool {
        self.advance();
        self.expression();
        self.consume(TokenType::Eof, "Expect end of expression.");
        self.end_compiler();
        !self.parser.had_error
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write_byte(byte, self.parser.previous.line);
    }

    fn emit_opcode(&mut self, opcode: OpCode) {
        self.chunk.write_op(opcode, self.parser.previous.line);
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        #[cfg(feature = "debug-print-code")]
        {
            if !self.parser.had_error {
                self.chunk.disassemble("code");
            }
        }
    }

    fn emit_return(&mut self) {
        self.emit_opcode(OpCode::Return);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn number(&mut self) {
        let value = self
            .parser
            .previous
            .lexeme
            .parse::<f64>()
            .expect("Unable to parse number");
        self.emit_constant(Value::Number(value));
    }

    fn string(&mut self) {
        // This strips the quotation marks
        let str = self.parser.previous.lexeme[1..self.parser.previous.lexeme.len() - 1].to_string();
        self.emit_constant(Value::Obj(Obj::String(str)))
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self) {
        let operator_type = &self.parser.previous.kind.clone();
        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Bang => self.emit_opcode(OpCode::Not),
            TokenType::Minus => self.emit_opcode(OpCode::Negate),
            _ => unreachable!(),
        }
    }

    // NOTE: According to IEEE 754, all comparison operators return false when an operand is NaN./
    // That means NaN <= 1 is false and NaN > 1 is also false.
    // But our desugaring assumes the latter is always the negation of the former.
    // BONUS: Create instructions for (!=, <=, and >=). VM would execute faster if we did.
    fn binary(&mut self) {
        let operator_kind = &self.parser.previous.kind.clone();
        let rule = self.get_rule(operator_kind);

        let next = unsafe { self.next_precedence(rule.precedence) };
        self.parse_precedence(next);

        match operator_kind {
            TokenType::Plus => self.emit_opcode(OpCode::Add),
            TokenType::Minus => self.emit_opcode(OpCode::Subtract),
            TokenType::Star => self.emit_opcode(OpCode::Multiply),
            TokenType::Slash => self.emit_opcode(OpCode::Divide),
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal as u8, OpCode::Not as u8),
            TokenType::EqualEqual => self.emit_opcode(OpCode::Equal),
            TokenType::Greater => self.emit_opcode(OpCode::Greater),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
            TokenType::Less => self.emit_opcode(OpCode::Less),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
            _ => unreachable!(),
        }
    }

    fn literal(&mut self) {
        match self.parser.previous.kind {
            TokenType::False => self.emit_opcode(OpCode::False),
            TokenType::True => self.emit_opcode(OpCode::True),
            TokenType::Nil => self.emit_opcode(OpCode::Nil),
            _ => unreachable!(),
        }
    }

    /// .
    ///
    /// # Safety: Should be ok...
    ///
    /// TODO: ...
    /// .
    unsafe fn next_precedence(&self, precedence: Precedence) -> Precedence {
        // TODO: Remove? or keep?
        if precedence == Precedence::Primary {
            panic!("Has no next.");
        }
        std::mem::transmute(precedence as u8 + 1)
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        match self.get_rule(&self.parser.previous.kind).prefix {
            Some(prefix_rule) => {
                prefix_rule(self);

                while precedence <= self.get_rule(&self.parser.current.kind).precedence {
                    self.advance();
                    if let Some(infix_rule) = self.get_rule(&self.parser.previous.kind).infix {
                        infix_rule(self);
                    }
                }
            }
            None => self.error("Expect Expression."),
        }
    }

    fn get_rule(&self, kind: &TokenType) -> ParseRule {
        // TODO: Make a fixed array called rules. Once cell it or something.
        // TODO: Kill this function once array is used. Just index the array
        use TokenType as t;
        match kind {
            t::LeftParen => ParseRule::new(Some(|c| c.grouping()), None, Precedence::None),
            t::RightParen => ParseRule::new(None, None, Precedence::None),
            t::LeftBrace => ParseRule::new(None, None, Precedence::None),
            t::RightBrace => ParseRule::new(None, None, Precedence::None),
            t::Comma => ParseRule::new(None, None, Precedence::None),
            t::Dot => ParseRule::new(None, None, Precedence::None),
            t::Minus => ParseRule::new(Some(|c| c.unary()), Some(|c| c.binary()), Precedence::Term),
            t::Plus => ParseRule::new(None, Some(|c| c.binary()), Precedence::Term),
            t::Semicolon => ParseRule::new(None, None, Precedence::None),
            t::Slash => ParseRule::new(None, Some(|c| c.binary()), Precedence::Factor),
            t::Star => ParseRule::new(None, Some(|c| c.binary()), Precedence::Factor),
            t::Bang => ParseRule::new(Some(|c| c.unary()), None, Precedence::None),
            t::BangEqual => ParseRule::new(None, Some(|c| c.binary()), Precedence::Equality),
            t::Equal => ParseRule::new(None, None, Precedence::None),
            t::EqualEqual => ParseRule::new(None, Some(|c| c.binary()), Precedence::Equality),
            t::Greater => ParseRule::new(None, Some(|c| c.binary()), Precedence::Comparison),
            t::GreaterEqual => ParseRule::new(None, Some(|c| c.binary()), Precedence::Comparison),
            t::Less => ParseRule::new(None, Some(|c| c.binary()), Precedence::Comparison),
            t::LessEqual => ParseRule::new(None, Some(|c| c.binary()), Precedence::Comparison),
            t::Identifier => ParseRule::new(None, None, Precedence::None),
            t::String => ParseRule::new(Some(|c| c.string()), None, Precedence::None),
            t::Number => ParseRule::new(Some(|c| c.number()), None, Precedence::None),
            t::And => ParseRule::new(None, None, Precedence::None),
            t::Class => ParseRule::new(None, None, Precedence::None),
            t::Else => ParseRule::new(None, None, Precedence::None),
            t::False => ParseRule::new(Some(|c| c.literal()), None, Precedence::None),
            t::Fun => ParseRule::new(None, None, Precedence::None),
            t::For => ParseRule::new(None, None, Precedence::None),
            t::If => ParseRule::new(None, None, Precedence::None),
            t::Nil => ParseRule::new(Some(|c| c.literal()), None, Precedence::None),
            t::Or => ParseRule::new(None, None, Precedence::None),
            t::Print => ParseRule::new(None, None, Precedence::None),
            t::Return => ParseRule::new(None, None, Precedence::None),
            t::Super => ParseRule::new(None, None, Precedence::None),
            t::This => ParseRule::new(None, None, Precedence::None),
            t::True => ParseRule::new(Some(|c| c.literal()), None, Precedence::None),
            t::Var => ParseRule::new(None, None, Precedence::None),
            t::While => ParseRule::new(None, None, Precedence::None),
            t::Error => ParseRule::new(None, None, Precedence::None),
            t::Eof => ParseRule::new(None, None, Precedence::None),
            t::Undefined => unimplemented!(),
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::Constant as u8, constant);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        match self.chunk.add_constant(value) {
            Some(constant) => constant,
            None => {
                self.error("Too many constants in one chunk.");
                0
            }
        }
    }
    fn advance(&mut self) {
        self.parser.previous = self.parser.current.clone();

        loop {
            self.parser.current = self.scanner.scan_token();
            if self.parser.current.kind != TokenType::Error {
                break;
            }

            self.error_at_current(self.parser.current.lexeme.clone());
        }
    }

    fn consume(&mut self, kind: TokenType, message: &str) {
        if self.parser.current.kind == kind {
            self.advance();
        } else {
            self.error_at_current(message.to_owned());
        }
    }

    fn error_at_current(&mut self, message: String) {
        self.error_at(&self.parser.current.clone(), message);
    }

    fn error(&mut self, message: &str) {
        self.error_at(&self.parser.previous.clone(), message.to_owned());
    }

    fn error_at(&mut self, token: &Token, message: String) {
        if self.parser.panic_mode {
            return;
        }
        self.parser.panic_mode = true;
        eprint!("[line {line}] Error", line = token.line);
        match token.kind {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => {}
            _ => eprint!(" at '{lexeme}'", lexeme = token.lexeme),
        }
        eprintln!(": {message}");
        self.parser.had_error = true;
    }
}
