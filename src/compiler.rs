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
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    _Call,      // . ()
    Primary,
}

struct ParseRule {
    prefix: Option<fn(&mut Compiler, bool)>,
    infix: Option<fn(&mut Compiler, bool)>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(
        prefix: Option<fn(&mut Compiler, bool)>,
        infix: Option<fn(&mut Compiler, bool)>,
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

struct Local {
    name: Token,
    depth: Option<u8>,
}

impl Local {
    pub fn new(name: Token, depth: Option<u8>) -> Self {
        Self { name, depth }
    }
}

pub struct Compiler<'a> {
    parser: Parser,
    chunk: &'a mut Chunk,
    scanner: Scanner<'a>,
    locals: Vec<Local>,
    scope_depth: u8,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str, chunk: &'a mut Chunk) -> Self {
        Self {
            parser: Parser::new(),
            chunk,
            scanner: Scanner::new(source),
            locals: Vec::with_capacity(u8::MAX.into()),
            scope_depth: 0,
        }
    }

    pub fn compile(&mut self) -> bool {
        self.advance();
        while !self.is_match(&TokenType::Eof) {
            self.declaration();
        }
        self.end_compiler();
        !self.parser.had_error
    }

    // TODO(aalhendi): have this take an Into<u8> so only one function is used. Replaces `emit_opcode()`.
    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write_byte(byte, self.parser.previous.line);
    }

    fn emit_opcode(&mut self, opcode: OpCode) {
        self.chunk.write_op(opcode, self.parser.previous.line);
    }

    fn emit_jump(&mut self, instruction: OpCode) -> usize {
        self.emit_opcode(instruction);
        self.emit_byte(u8::MAX);
        self.emit_byte(u8::MAX);

        // currentChunk->count-2
        self.chunk.lines.len() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        // -2 to adjust for bytecode for jmp offset itself
        let jump = self.chunk.lines.len() - offset - 2;

        if jump > u16::MAX.into() {
            self.error("Too much code to jump over.");
        }

        self.chunk.code[offset] = ((jump >> 8) & u8::MAX as usize) as u8;
        self.chunk.code[offset + 1] = (jump as u8) & u8::MAX;
        // TODO: Change to something like this
        // self.chunk.code[offset..offset + 2].copy_from_slice(&jump.to_le_bytes());
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

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
        // While not empty and greater than current scope depth
        while self
            .locals
            .last()
            .is_some_and(|l| l.depth > Some(self.scope_depth))
        {
            self.emit_opcode(OpCode::Pop);
            self.locals.pop();
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

    fn block(&mut self) {
        while !self.check(&TokenType::RightBrace) && !self.check(&TokenType::Eof) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.is_match(&TokenType::Equal) {
            self.expression();
        } else {
            // Desugars an empty declaration like ``var a;`` to ``var a = nil;``
            self.emit_opcode(OpCode::Nil);
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_opcode(OpCode::Pop);
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop);
        self.statement();

        let else_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(then_jump);
        self.emit_opcode(OpCode::Pop);

        if self.is_match(&TokenType::Else) {
            self.statement();
        }

        self.patch_jump(else_jump);
    }

    fn declaration(&mut self) {
        if self.is_match(&TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.parser.panic_mode {
            self.synchronize();
        }
    }

    /// Compiles statements. Statements have zero stack effect.
    fn statement(&mut self) {
        // TODO(aalhendi): Replace with match...
        if self.is_match(&TokenType::Print) {
            self.print_statement();
        } else if self.is_match(&TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self.is_match(&TokenType::If) {
            self.if_statement();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_opcode(OpCode::Print);
    }

    fn synchronize(&mut self) {
        self.parser.panic_mode = false;

        while self.parser.current.kind != TokenType::Eof {
            if self.parser.previous.kind == TokenType::Semicolon {
                return;
            }
            match self.parser.current.kind {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }
            self.advance();
        }
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

    /// Or expressions are "lazy" evaluated and short circuit if the left-hand side is truthy.
    /// This means we skip the right operand and need to jump to the end of the right operand expression.
    fn or(&mut self) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(else_jump);
        self.emit_opcode(OpCode::Pop);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn string(&mut self) {
        // This strips the quotation marks
        let str = self.parser.previous.lexeme[1..self.parser.previous.lexeme.len() - 1].to_string();
        self.emit_constant(Value::Obj(Obj::String(str)))
    }

    fn named_variable(&mut self, name: &Token, can_assign: bool) {
        // Check if local can be resolved. If not, its a global
        let arg_local = self.resolve_local(name);
        // TODO(aalhendi): refactor this
        match arg_local {
            Some(arg) => {
                if can_assign && self.is_match(&TokenType::Equal) {
                    self.expression();
                    self.emit_bytes(OpCode::SetLocal as u8, arg);
                } else {
                    self.emit_bytes(OpCode::GetLocal as u8, arg);
                }
            }
            None => {
                let arg = self.identifier_constant(name);
                if can_assign && self.is_match(&TokenType::Equal) {
                    self.expression();
                    self.emit_bytes(OpCode::SetGlobal as u8, arg);
                } else {
                    self.emit_bytes(OpCode::GetGlobal as u8, arg);
                }
            }
        }
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(&self.parser.previous.clone(), can_assign);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self, _can_assignn: bool) {
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
    fn binary(&mut self, can_assign: bool) {
        let operator_kind = &self.parser.previous.kind.clone();
        let rule = self.get_rule(operator_kind, can_assign);

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
        let can_assign = precedence <= Precedence::Assignment;
        self.advance();
        match self.get_rule(&self.parser.previous.kind, can_assign).prefix {
            Some(prefix_rule) => {
                prefix_rule(self, can_assign);

                while precedence
                    <= self
                        .get_rule(&self.parser.current.kind, can_assign)
                        .precedence
                {
                    self.advance();
                    if let Some(infix_rule) =
                        self.get_rule(&self.parser.previous.kind, can_assign).infix
                    {
                        infix_rule(self, can_assign);
                    }

                    if can_assign && self.is_match(&TokenType::Equal) {
                        self.error("Invalid assignment target.")
                    }
                }
            }
            None => self.error("Expect Expression."),
        }
    }

    fn identifier_constant(&mut self, name: &Token) -> u8 {
        self.make_constant(Value::Obj(Obj::String(name.lexeme.clone())))
    }

    // NOTE(aalhendi): Is this needed?
    fn identifiers_equal(&mut self, a: &Token, b: &Token) -> bool {
        a.lexeme == b.lexeme
    }

    fn resolve_local(&mut self, name: &Token) -> Option<u8> {
        for i in (0..self.locals.len()).rev() {
            let local = &self.locals[i];
            // if self.identifiers_equal(name, &local.name.clone()) {
            if name.lexeme == local.name.lexeme {
                if local.depth.is_none() {
                    self.error("Can't read local variable in its own initializer.");
                }
                return Some(i as u8);
            }
        }
        None
    }

    fn add_local(&mut self, name: &Token) {
        if self.locals.len() > u8::MAX.into() {
            self.error("Too many local variables in function");
            return;
        }
        self.locals.push(Local::new(name.clone(), None));
    }

    fn declare_variable(&mut self) {
        if self.scope_depth == 0 {
            return;
        }

        // TODO(aalhendi): This double clone business is not cool... find a workaround? RefCell?
        let name = &self.parser.previous.clone();

        for i in (0..self.locals.len()).rev() {
            let local = &self.locals[i];
            if local.depth.is_some_and(|d| d < self.scope_depth) {
                break;
            }

            if self.identifiers_equal(name, &local.name.clone()) {
                self.error("Already a variable with his name in this scope.");
            }
        }
        self.add_local(name);
    }

    fn parse_variable(&mut self, error_msg: &str) -> u8 {
        self.consume(TokenType::Identifier, error_msg);

        self.declare_variable();
        if self.scope_depth > 0 {
            return 0;
        }
        self.identifier_constant(&self.parser.previous.clone())
    }

    fn mark_initialized(&mut self) {
        self.locals.last_mut().unwrap().depth = Some(self.scope_depth);
    }

    fn define_variable(&mut self, global: u8) {
        if self.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(OpCode::DefineGlobal as u8, global);
    }

    /// And expressions are "lazy" evaluated and short circuit if the left-hand side is falsey.
    /// This means we skip the right operand and need to jump to the end of the right operand expression.
    /// If the left-hand expression is truthy, then we discard it and eval the right operand expression.
    fn and(&mut self) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn get_rule(&self, kind: &TokenType, _can_assign: bool) -> ParseRule {
        // TODO: Make a fixed array called rules. Once cell it or something.
        // TODO: Kill this function once array is used. Just index the array
        use TokenType as t;
        match kind {
            t::LeftParen => ParseRule::new(
                Some(|c, _can_assign: bool| c.grouping()),
                None,
                Precedence::None,
            ),
            t::RightParen => ParseRule::new(None, None, Precedence::None),
            t::LeftBrace => ParseRule::new(None, None, Precedence::None),
            t::RightBrace => ParseRule::new(None, None, Precedence::None),
            t::Comma => ParseRule::new(None, None, Precedence::None),
            t::Dot => ParseRule::new(None, None, Precedence::None),
            t::Minus => ParseRule::new(
                Some(|c, can_assign: bool| c.unary(can_assign)),
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Term,
            ),
            t::Plus => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Term,
            ),
            t::Semicolon => ParseRule::new(None, None, Precedence::None),
            t::Slash => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Factor,
            ),
            t::Star => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Factor,
            ),
            t::Bang => ParseRule::new(
                Some(|c, can_assign: bool| c.unary(can_assign)),
                None,
                Precedence::None,
            ),
            t::BangEqual => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Equality,
            ),
            t::Equal => ParseRule::new(None, None, Precedence::None),
            t::EqualEqual => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Equality,
            ),
            t::Greater => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Comparison,
            ),
            t::GreaterEqual => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Comparison,
            ),
            t::Less => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Comparison,
            ),
            t::LessEqual => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Comparison,
            ),
            t::Identifier => ParseRule::new(
                Some(|c, can_assign: bool| c.variable(can_assign)),
                None,
                Precedence::None,
            ),
            t::String => ParseRule::new(
                Some(|c, _can_assign: bool| c.string()),
                None,
                Precedence::None,
            ),
            t::Number => ParseRule::new(
                Some(|c, _can_assign: bool| c.number()),
                None,
                Precedence::None,
            ),
            t::And => ParseRule::new(None, Some(|c, _can_assign| c.and()), Precedence::And),
            t::Class => ParseRule::new(None, None, Precedence::None),
            t::Else => ParseRule::new(None, None, Precedence::None),
            t::False => ParseRule::new(
                Some(|c, _can_assign: bool| c.literal()),
                None,
                Precedence::None,
            ),
            t::Fun => ParseRule::new(None, None, Precedence::None),
            t::For => ParseRule::new(None, None, Precedence::None),
            t::If => ParseRule::new(None, None, Precedence::None),
            t::Nil => ParseRule::new(
                Some(|c, _can_assign: bool| c.literal()),
                None,
                Precedence::None,
            ),
            t::Or => ParseRule::new(None, Some(|c, _can_assign| c.or()), Precedence::Or),
            t::Print => ParseRule::new(None, None, Precedence::None),
            t::Return => ParseRule::new(None, None, Precedence::None),
            t::Super => ParseRule::new(None, None, Precedence::None),
            t::This => ParseRule::new(None, None, Precedence::None),
            t::True => ParseRule::new(
                Some(|c, _can_assign: bool| c.literal()),
                None,
                Precedence::None,
            ),
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

    fn is_match(&mut self, kind: &TokenType) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, kind: &TokenType) -> bool {
        &self.parser.current.kind == kind
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
