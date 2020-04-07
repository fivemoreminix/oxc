use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Symbol {
    LBrace,    // {
    RBrace,    // }
    LParen,    // (
    RParen,    // )
    Semicolon, // ;
    Colon,     // :
}
use self::Symbol::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Operator {
    LogicalNegation,   // !
    Minus,             // -
    BitwiseComplement, // ~
    Plus,              // +
    Star,              // *
    Slash,             // /
    Modulo,            // %
    Assignment,        // =
    PlusAssign,        // +=
    MinusAssign,       // -=
    SlashAssign,       // /=
    StarAssign,        // *=
    ModAssign,         // %=
    LeftShiftAssign,   // <<=
    RightShiftAssign,  // >>=
    ANDAssign,         // &=
    ORAssign,          // |=
    XORAssign,         // ^=
    And,               // &&
    Or,                // ||
    EqualEqual,        // ==
    NotEqual,          // !=
    LessThan,          // <
    LessEqual,         // <=
    GreaterThan,       // >
    GreaterEqual,      // >=
    BitwiseAND,        // &
    BitwiseOR,         // |
    BitwiseXOR,        // ^
    BitwiseShiftLeft,  // <<
    BitwiseShiftRight, // >>
    QuestionMark,      // ?

}
use self::Operator::*;

impl Operator {
    pub fn is_unary(&self) -> bool {
        match self {
            | Operator::Minus
            | Operator::LogicalNegation
            | Operator::BitwiseComplement => true,
            _ => false,
        }
    }

    pub fn is_bitwise(&self) -> bool {
        // Does not include BitwiseCompliment because it is unary
        match self {
            | Operator::BitwiseAND
            | Operator::BitwiseOR
            | Operator::BitwiseXOR
            | Operator::BitwiseShiftLeft
            | Operator::BitwiseShiftRight => true,
            _ => false,
        }
    }

    pub fn is_assignment(&self) -> bool {
        match self {
            | Operator::Assignment
            | Operator::PlusAssign
            | Operator::MinusAssign
            | Operator::SlashAssign
            | Operator::StarAssign
            | Operator::ModAssign
            | Operator::LeftShiftAssign
            | Operator::RightShiftAssign
            | Operator::ANDAssign
            | Operator::ORAssign
            | Operator::XORAssign => true,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    Int,
    Return,
    If,
    Else,
    For,
    Do,
    While,
    Break,
    Continue,
}
use self::Keyword::*;

impl Keyword {
    pub fn is_type(&self) -> bool {
        match self {
            Int => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Symbol(Symbol),
    Operator(Operator),
    Keyword(Keyword),
    Id(String),
    Integer(i32),
}
use self::Token::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenData<'a> {
    pub token: Token,
    pub slice: &'a str,
    pub line: usize,
    pub col: usize,
}

impl<'a> TokenData<'a> {
    pub fn new(token: Token, slice: &'a str, line: usize, col: usize) -> TokenData {
        TokenData { token, slice, line, col }
    }
}

impl<'a> PartialEq<Token> for TokenData<'a> {
    fn eq(&self, token: &Token) -> bool {
        &self.token == token
    }
}

pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    pos: usize, // index in source
    line: usize, 
    col: usize,
    first_run: bool,
}

impl<'a> Lexer<'a> {
    /// The source must contain no return-carriage characters: `\r`.
    pub fn new(source: &'a str) -> Lexer {
        Lexer {
            source,
            chars: source.chars().peekable(),
            pos: 0,
            line: 1,
            col: 1,
            first_run: true,
        }
    }

    /// To be used instead of `self.chars.next()`
    fn next_char(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            if !self.first_run {
                // Offset counter by -1, so that
                // self.pos returns last position
                // instead of next search position.
                self.pos += 1;
            } else {
                self.first_run = false;
            }

            if c == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
            
            Some(c)
        } else {
            None
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = TokenData<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        loop { // Only intention is for `continue` on whitespace encountered
            return if let Some(c) = self.next_char() {
                match c {
                    '{' => Some(TokenData::new(Symbol(LBrace), &self.source[self.pos..=self.pos], self.line, self.col)),
                    '}' => Some(TokenData::new(Symbol(RBrace), &self.source[self.pos..=self.pos], self.line, self.col)),
                    '(' => Some(TokenData::new(Symbol(LParen), &self.source[self.pos..=self.pos], self.line, self.col)),
                    ')' => Some(TokenData::new(Symbol(RParen), &self.source[self.pos..=self.pos], self.line, self.col)),
                    ';' => Some(TokenData::new(Symbol(Semicolon), &self.source[self.pos..=self.pos], self.line, self.col)),
                    ':' => Some(TokenData::new(Symbol(Colon), &self.source[self.pos..=self.pos], self.line, self.col)),
                    '~' => Some(TokenData::new(Operator(BitwiseComplement), &self.source[self.pos..=self.pos], self.line, self.col)),
                    '?' => Some(TokenData::new(Operator(QuestionMark), &self.source[self.pos..=self.pos], self.line, self.col)),
                    '!' => if self.chars.peek() == Some(&'=') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(NotEqual), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else {
                        Some(TokenData::new(Operator(LogicalNegation), &self.source[self.pos..=self.pos], self.line, self.col))
                    }
                    '-' => if self.chars.peek() == Some(&'=') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(MinusAssign), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else {
                        Some(TokenData::new(Operator(Minus), &self.source[self.pos..=self.pos], self.line, self.col))
                    }
                    '+' => if self.chars.peek() == Some(&'=') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(PlusAssign), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else {
                        Some(TokenData::new(Operator(Plus), &self.source[self.pos..=self.pos], self.line, self.col))
                    }
                    '*' => if self.chars.peek() == Some(&'=') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(StarAssign), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else {
                        Some(TokenData::new(Operator(Star), &self.source[self.pos..=self.pos], self.line, self.col))
                    }
                    '/' => if self.chars.peek() == Some(&'=') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(SlashAssign), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else {
                        Some(TokenData::new(Operator(Slash), &self.source[self.pos..=self.pos], self.line, self.col))
                    }
                    '%' => if self.chars.peek() == Some(&'=') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(ModAssign), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else {
                        Some(TokenData::new(Operator(Modulo), &self.source[self.pos..=self.pos], self.line, self.col))
                    }
                    '&' => if self.chars.peek() == Some(&'&') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(And), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else if self.chars.peek() == Some(&'=') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(ANDAssign), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else {
                        Some(TokenData::new(Operator(BitwiseAND), &self.source[self.pos..=self.pos], self.line, self.col))
                    }
                    '|' => if self.chars.peek() == Some(&'|') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(Or), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else if self.chars.peek() == Some(&'=') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(ORAssign), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else {
                        Some(TokenData::new(Operator(BitwiseOR), &self.source[self.pos..=self.pos], self.line, self.col))
                    }
                    '=' => if self.chars.peek() == Some(&'=') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(EqualEqual), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else {
                        Some(TokenData::new(Operator(Assignment), &self.source[self.pos..=self.pos], self.line, self.col))
                    }
                    '<' => if self.chars.peek() == Some(&'=') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(LessEqual), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else if self.chars.peek() == Some(&'<') {
                        self.next_char().unwrap();
                        if self.chars.peek() == Some(&'=') {
                            self.next_char().unwrap();
                            Some(TokenData::new(Operator(LeftShiftAssign), &self.source[self.pos-2..=self.pos], self.line, self.col-2))
                        } else {
                            Some(TokenData::new(Operator(BitwiseShiftLeft), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                        }
                    } else {
                        Some(TokenData::new(Operator(LessThan), &self.source[self.pos..=self.pos], self.line, self.col))
                    }
                    '>' => if self.chars.peek() == Some(&'=') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(GreaterEqual), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else if self.chars.peek() == Some(&'>') {
                        self.next_char().unwrap();
                        if self.chars.peek() == Some(&'=') {
                            self.next_char().unwrap();
                            Some(TokenData::new(Operator(RightShiftAssign), &self.source[self.pos-2..=self.pos], self.line, self.col-2))
                        } else {
                            Some(TokenData::new(Operator(BitwiseShiftRight), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                        }
                    } else {
                        Some(TokenData::new(Operator(GreaterThan), &self.source[self.pos..=self.pos], self.line, self.col))
                    }
                    '^' => if self.chars.peek() == Some(&'=') {
                        self.next_char().unwrap();
                        Some(TokenData::new(Operator(XORAssign), &self.source[self.pos-1..=self.pos], self.line, self.col-1))
                    } else {
                        Some(TokenData::new(Operator(BitwiseXOR), &self.source[self.pos..=self.pos], self.line, self.col))
                    }
                    _ => {
                        if c.is_whitespace() {
                            continue;
                        } else if c.is_alphabetic() || c == '_' {
                            let mut full = c.to_string();
                            let start_pos = self.pos;

                            while let Some(&c) = self.chars.peek() { // Read an identifier
                                if c.is_alphabetic() || c.is_digit(10) {
                                    full.push(self.next_char().unwrap());
                                } else {
                                    break;
                                }
                            }

                            let slice = &self.source[start_pos..=self.pos];
                            let col = self.col - slice.len();
                            match &full.to_lowercase()[..] {
                                "int" => Some(TokenData::new(Keyword(Int), slice, self.line, col)),
                                "return" => Some(TokenData::new(Keyword(Return), slice, self.line, col)),
                                "if" => Some(TokenData::new(Keyword(If), slice, self.line, col)),
                                "else" => Some(TokenData::new(Keyword(Else), slice, self.line, col)),
                                "for" => Some(TokenData::new(Keyword(For), slice, self.line, col)),
                                "do" => Some(TokenData::new(Keyword(Do), slice, self.line, col)),
                                "while" => Some(TokenData::new(Keyword(While), slice, self.line, col)),
                                "break" => Some(TokenData::new(Keyword(Break), slice, self.line, col)),
                                "continue" => Some(TokenData::new(Keyword(Continue), slice, self.line, col)),
                                _ => Some(TokenData::new(Id(full), slice, self.line, col)),
                            }
                        }
                        else if c.is_digit(10) {
                            let mut full = c.to_string();
                            let start_pos = self.pos;
                            
                            while let Some(&c) = self.chars.peek() { // Read the entire number
                                if c.is_digit(10) {
                                    full.push(self.next_char().unwrap());
                                } else {
                                    break;
                                }
                            }

                            let slice = &self.source[start_pos..=self.pos];
                            Some(TokenData::new(Integer(full.parse().unwrap()), slice, self.line, self.col - slice.len()))
                        } else {
                            panic!("Unrecognized character: {:?}", c);
                        }
                    }
                }
            } else {
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basics() {
        let source = "int main() {\n    return 2 >> 3;\n}\n"; // A typical source file has a lot of whitespace
        let mut lexer = super::Lexer::new(&source);

        let tok = lexer.next().unwrap();
        eprintln!("int: {:?}", tok);
        assert!(tok.token == Token::Keyword(Keyword::Int));
        assert!(tok.slice == "int");
        
        lexer.next().unwrap();
        lexer.next().unwrap();
        lexer.next().unwrap();
        lexer.next().unwrap();

        let tok = lexer.next().unwrap(); // 'return'
        assert!(tok.token == Token::Keyword(Keyword::Return));
        assert!(tok.slice == "return");
        assert!(tok.line == 2);
        assert!(tok.col == 4);
    }
}
