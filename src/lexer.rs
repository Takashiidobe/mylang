use std::hash::Hash;
use std::{collections::HashSet, fmt};

#[cfg(test)]
use serde::{Deserialize, Serialize};

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Op {
    Star,
    Slash,
    Plus,
    Minus,
    And,
    Or,
    Bang,
    BangEqual,
    EqualEqual,
    Equal,
    Lt,
    Le,
    Gt,
    Ge,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Op::Star => "*",
            Op::Slash => "/",
            Op::Plus => "+",
            Op::Minus => "-",
            Op::And => "&&",
            Op::Or => "||",
            Op::Bang => "!",
            Op::BangEqual => "!=",
            Op::EqualEqual => "==",
            Op::Equal => "=",
            Op::Lt => "<",
            Op::Le => "<=",
            Op::Gt => ">",
            Op::Ge => ">=",
        })
    }
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Punct {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Semicolon,
    Comma,
    Dot,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Keyword {
    Function,
    For,
    While,
    If,
    Else,
    Let,
    True,
    False,
    Return,
    Nil,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenType {
    Keyword(Keyword),
    Op(Op),
    Punct(Punct),
    String,
    Identifier,
    Number,
    Eof,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceLocation {
    pub line: usize,
    pub col: usize,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Value {
    String(String),
    Number(i64),
    Identifier(String),
    Bool(bool),
    #[default]
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = match self {
            Value::String(s) => s,
            Value::Number(n) => &n.to_string(),
            Value::Identifier(ident) => ident,
            Value::Bool(b) => &b.to_string(),
            Value::Nil => "nil",
        };

        f.write_str(val)
    }
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub r#type: TokenType,
    pub loc: SourceLocation,
    pub len: usize,
    pub value: Option<Value>,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let type_val = match &self.r#type {
            TokenType::Op(op) => Some(match op {
                Op::Star => "*",
                Op::Slash => "/",
                Op::Plus => "+",
                Op::Minus => "-",
                Op::And => "&&",
                Op::Or => "||",
                Op::Bang => "!",
                Op::BangEqual => "!=",
                Op::EqualEqual => "==",
                Op::Equal => "=",
                Op::Lt => "<",
                Op::Le => "<=",
                Op::Gt => ">",
                Op::Ge => ">=",
            }),
            TokenType::Keyword(keyword) => Some(match keyword {
                Keyword::Function => "fn",
                Keyword::For => "for",
                Keyword::While => "while",
                Keyword::If => "if",
                Keyword::Else => "else",
                Keyword::Let => "let",
                Keyword::True => "true",
                Keyword::False => "false",
                Keyword::Return => "return",
                Keyword::Nil => "nil",
            }),
            TokenType::Punct(punct) => Some(match punct {
                Punct::LParen => "{",
                Punct::RParen => "}",
                Punct::LBracket => "[",
                Punct::RBracket => "]",
                Punct::LBrace => "{",
                Punct::RBrace => "}",
                Punct::Semicolon => ";",
                Punct::Comma => ",",
                Punct::Dot => ".",
            }),
            TokenType::Eof => Some("eof"),
            _ => None,
        };

        if let Some(val) = type_val {
            return f.write_str(val);
        }

        let val = match &self.value.clone().unwrap() {
            Value::String(s) => s.to_string(),
            Value::Number(number) => number.to_string(),
            Value::Identifier(ident) => ident.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Nil => "nil".to_string(),
        };

        f.write_str(&val)
    }
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lexer {
    chars: Vec<char>,
    i: usize,
    line: usize,
    col: usize,
}

impl Lexer {
    pub fn new(program: &str) -> Self {
        Self {
            chars: program.chars().collect(),
            line: 1,
            col: 0,
            i: 0,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        while let Some(c) = self.peek() {
            match c {
                '"' => tokens.push(self.string()),
                '0'..='9' => tokens.push(self.number(c)),
                'a'..='z' | 'A'..='Z' | '_' => tokens.push(self.identifier(c)),
                '(' | ')' | '[' | ']' | '{' | '}' | ';' | ',' | '.' => tokens.push(self.punct(c)),
                '!' | '=' | '<' | '>' | '*' | '+' | '/' | '-' => tokens.push(self.op(c)),
                '\n' => {
                    self.line += 1;
                    self.col = 0;
                }
                ' ' => {}
                c => panic!("Unknown character: {c}"),
            }
            if c != '\n' {
                self.incr_one();
            }
        }
        tokens.push(Token {
            r#type: TokenType::Eof,
            loc: self.loc(),
            len: 0,
            value: None,
        });
        tokens
    }

    fn identifier(&mut self, c: char) -> Token {
        let loc = self.loc();

        let mut chars = String::default();
        chars.push(c);
        self.incr_one();

        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() {
                chars.push(c);
                self.incr_one();
            } else {
                break;
            }
        }
        let len = chars.len();

        let keywords = HashSet::from([
            "fn", "for", "while", "if", "else", "true", "false", "return", "nil",
        ]);

        let token_type = if keywords.contains(&chars.as_str()) {
            if chars.as_str() == "fn" {
                TokenType::Keyword(Keyword::Function)
            } else if chars.as_str() == "for" {
                TokenType::Keyword(Keyword::For)
            } else if chars.as_str() == "while" {
                TokenType::Keyword(Keyword::While)
            } else if chars.as_str() == "if" {
                TokenType::Keyword(Keyword::If)
            } else if chars.as_str() == "else" {
                TokenType::Keyword(Keyword::Else)
            } else if chars.as_str() == "true" {
                TokenType::Keyword(Keyword::True)
            } else if chars.as_str() == "false" {
                TokenType::Keyword(Keyword::False)
            } else if chars.as_str() == "nil" {
                TokenType::Keyword(Keyword::Nil)
            } else if chars.as_str() == "return" {
                TokenType::Keyword(Keyword::Return)
            } else {
                TokenType::Identifier
            }
        } else {
            TokenType::Identifier
        };

        Token {
            loc,
            len,
            value: if token_type == TokenType::Identifier {
                Some(Value::Identifier(chars.to_string()))
            } else {
                None
            },
            r#type: token_type,
        }
    }

    fn op(&mut self, c: char) -> Token {
        let loc = self.loc();

        let (op, len) = match c {
            '*' => (Op::Star, 1),
            '+' => (Op::Plus, 1),
            '-' => (Op::Minus, 1),
            '/' => (Op::Slash, 1),
            '<' => {
                if let Some('=') = self.peek_next() {
                    self.incr_one();
                    (Op::Le, 2)
                } else {
                    (Op::Lt, 1)
                }
            }
            '>' => {
                if let Some('=') = self.peek_next() {
                    self.incr_one();
                    (Op::Ge, 2)
                } else {
                    (Op::Gt, 1)
                }
            }
            '!' => {
                if let Some('=') = self.peek_next() {
                    self.incr_one();
                    (Op::BangEqual, 2)
                } else {
                    (Op::Bang, 1)
                }
            }
            '=' => {
                if let Some('=') = self.peek_next() {
                    self.incr_one();
                    (Op::EqualEqual, 2)
                } else {
                    (Op::Equal, 1)
                }
            }
            _ => unreachable!(),
        };

        Token {
            r#type: TokenType::Op(op),
            loc,
            len,
            value: None,
        }
    }

    fn punct(&mut self, c: char) -> Token {
        let loc = self.loc();

        let punct = match c {
            '(' => Punct::LParen,
            ')' => Punct::RParen,
            '[' => Punct::LBracket,
            ']' => Punct::RBracket,
            '{' => Punct::LBrace,
            '}' => Punct::RBrace,
            ';' => Punct::Semicolon,
            ',' => Punct::Comma,
            '.' => Punct::Dot,
            _ => unreachable!(),
        };

        Token {
            r#type: TokenType::Punct(punct),
            loc,
            len: 1,
            value: None,
        }
    }

    fn number(&mut self, c: char) -> Token {
        let mut chars = String::default();
        chars.push(c);
        let loc = self.loc();

        while let Some(c) = self.peek_next() {
            if c.is_ascii_digit() {
                chars.push(c);
            } else {
                break;
            }
            self.incr_one();
        }

        Token {
            r#type: TokenType::Number,
            loc,
            len: chars.len(),
            value: Some(Value::Number(chars.parse().unwrap())),
        }
    }

    fn string(&mut self) -> Token {
        let loc = self.loc();
        self.consume('"');
        let mut chars = String::default();
        while let Some(c) = self.peek() {
            if c.is_ascii_alphabetic() {
                chars.push(c);
                self.incr_one();
            } else {
                break;
            }
        }
        self.consume('"');

        Token {
            r#type: TokenType::String,
            loc,
            len: chars.len(),
            value: Some(Value::String(chars)),
        }
    }

    fn loc(&self) -> SourceLocation {
        SourceLocation {
            line: self.line,
            col: self.col,
        }
    }

    fn consume(&mut self, c: char) {
        if !self.matches(c) {
            panic!("curr: {} did not match {c}", self.chars[self.i]);
        }
    }

    fn matches(&mut self, c: char) -> bool {
        if let Some(curr) = self.peek() {
            if curr == c {
                self.incr_one();
                return true;
            }
        }
        false
    }

    fn next(&mut self) -> Option<char> {
        match self.peek() {
            Some(c) => {
                self.incr_one();
                Some(c)
            }
            None => None,
        }
    }

    fn incr_one(&mut self) {
        self.incr(1);
    }

    fn incr(&mut self, n: usize) {
        self.i += n;
        self.col += n;
    }

    fn peek(&self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            Some(self.chars[self.i])
        }
    }

    fn peek_next(&self) -> Option<char> {
        if self.i + 1 >= self.chars.len() {
            None
        } else {
            Some(self.chars[self.i + 1])
        }
    }

    fn is_at_end(&self) -> bool {
        self.i >= self.chars.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_yaml_snapshot;

    #[test]
    fn lex_string() {
        let mut lexer = Lexer::new("\"hello\"");
        assert_yaml_snapshot!(lexer.lex());
    }

    #[test]
    fn lex_number() {
        let mut lexer = Lexer::new("1234");
        assert_yaml_snapshot!(lexer.lex());
    }

    #[test]
    fn lex_two_numbers() {
        let mut lexer = Lexer::new("1234 500");
        assert_yaml_snapshot!(lexer.lex());
    }

    #[test]
    fn lex_punct() {
        let mut lexer = Lexer::new("[]{}();,.");
        assert_yaml_snapshot!(lexer.lex());
    }

    #[test]
    fn lex_ops() {
        let mut lexer = Lexer::new("!= < <= > >= == = * + - /");
        assert_yaml_snapshot!(lexer.lex());
    }

    #[test]
    fn lex_math() {
        let mut lexer = Lexer::new("15 + 300 * 40");
        assert_yaml_snapshot!(lexer.lex());
    }

    #[test]
    fn lex_fn() {
        let mut lexer = Lexer::new("fn xyz (x, y, z) { return y; }");
        assert_yaml_snapshot!(lexer.lex());
    }
}
