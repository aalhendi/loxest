use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenType,
    pub lexeme: &'a str,
    pub line: usize,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenType, lexeme: &str, line: usize) -> Token {
        Token {
            kind,
            lexeme,
            line,
        }
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{token_type:?} {lexeme}",
            token_type = self.kind,
            lexeme = self.lexeme,
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
#[repr(C)]
pub enum TokenType {
    // --- Single-character tokens. ---
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    // Colon,
    Dot,
    Minus,
    Plus,
    // QuestionMark,
    Semicolon,
    Slash,
    Star,
    // --- One or two character tokens. ---
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // --- Literals. ---
    Identifier,
    String,
    Number,
    // --- Keywords. ---
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    // --- Other. ---
    Error,
    Eof,
}