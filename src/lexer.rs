use std::collections::HashSet;

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
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenType {
    Keyword,
    Identifier,
    Op,
    Punct,
    String,
    Number,
    Eof,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceLocation {
    line: usize,
    col: usize,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenValue {
    Op(Op),
    Keyword(Keyword),
    Identifier(String),
    Punct(Punct),
    String(String),
    Number(i64),
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    r#type: TokenType,
    loc: SourceLocation,
    len: usize,
    value: Option<TokenValue>,
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
                '0'..='9' => tokens.push(self.number()),
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
            self.incr_one();
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
            "fn", "for", "while", "if", "else", "true", "false", "return",
        ]);

        let token_type = if keywords.contains(&chars.as_str()) {
            TokenType::Keyword
        } else {
            TokenType::Identifier
        };

        let value = if chars.as_str() == "fn" {
            TokenValue::Keyword(Keyword::Function)
        } else if chars.as_str() == "for" {
            TokenValue::Keyword(Keyword::For)
        } else if chars.as_str() == "while" {
            TokenValue::Keyword(Keyword::While)
        } else if chars.as_str() == "if" {
            TokenValue::Keyword(Keyword::If)
        } else if chars.as_str() == "else" {
            TokenValue::Keyword(Keyword::Else)
        } else if chars.as_str() == "true" {
            TokenValue::Keyword(Keyword::True)
        } else if chars.as_str() == "false" {
            TokenValue::Keyword(Keyword::False)
        } else if chars.as_str() == "return" {
            TokenValue::Keyword(Keyword::Return)
        } else {
            TokenValue::Identifier(chars)
        };

        Token {
            r#type: token_type,
            loc,
            len,
            value: Some(value),
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
                    (Op::Le, 2)
                } else {
                    (Op::Lt, 1)
                }
            }
            '>' => {
                if let Some('=') = self.peek_next() {
                    (Op::Ge, 2)
                } else {
                    (Op::Gt, 1)
                }
            }
            '!' => {
                if let Some('=') = self.peek_next() {
                    (Op::BangEqual, 2)
                } else {
                    (Op::Bang, 1)
                }
            }
            '=' => {
                if let Some('=') = self.peek_next() {
                    (Op::EqualEqual, 2)
                } else {
                    (Op::Equal, 1)
                }
            }
            _ => unreachable!(),
        };

        Token {
            r#type: TokenType::Op,
            loc,
            len,
            value: Some(TokenValue::Op(op)),
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
            r#type: TokenType::Punct,
            loc,
            len: 1,
            value: Some(TokenValue::Punct(punct)),
        }
    }

    fn number(&mut self) -> Token {
        let mut chars = String::default();
        let loc = self.loc();

        while let Some(c) = self.next() {
            if c.is_ascii_digit() {
                chars.push(c);
            } else {
                break;
            }
        }

        Token {
            r#type: TokenType::Number,
            loc,
            len: chars.len(),
            value: Some(TokenValue::Number(chars.parse().unwrap())),
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
            value: Some(TokenValue::String(chars)),
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
    fn lex_fn() {
        let mut lexer = Lexer::new("fn xyz (x, y, z) { return y; }");
        assert_yaml_snapshot!(lexer.lex());
    }
}
