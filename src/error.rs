#[cfg(test)]
use serde::Serialize;

use crate::lexer::Token;
use std::{fmt, io};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parse { token: Token, message: String },
    Runtime { token: Token, message: String },
}

#[cfg(test)]
impl Serialize for Error {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(underlying) => write!(f, "IoError {}", underlying),
            Error::Parse { token, message } => {
                write!(f, "ParseError at token: {}, message: {}", token, message)
            }
            Error::Runtime { token, message } => {
                write!(f, "RuntimeError at token: {}, message: {}", token, message)
            }
        }
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        "Lox Error"
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}
