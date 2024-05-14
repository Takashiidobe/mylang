use std::fmt;

use crate::{
    lexer::{Op, Token, TokenType, Value},
    parser::Expr,
};

pub struct Codegen {
    depth: i64,
    instructions: Vec<AsmInstruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Reg {
    Rax,
    Rsp,
    Rsi,
    Rdi,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MoveSrc {
    Reg(Reg),
    Label(String),
    Number(i64),
}

impl fmt::Display for MoveSrc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MoveSrc::Reg(reg) => f.write_fmt(format_args!("{}", reg)),
            MoveSrc::Label(label) => f.write_fmt(format_args!("${}", label)),
            MoveSrc::Number(num) => f.write_fmt(format_args!("${}", num)),
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Reg::Rax => "%rax",
            Reg::Rsp => "%rsp",
            Reg::Rsi => "%rsi",
            Reg::Rdi => "%rdi",
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmInstruction {
    Section(String),
    Variable(String, Option<String>),
    Label(String),
    Xor(Reg, Reg),
    Push(Reg),
    Pop(Reg),
    Call(String),
    Mov(MoveSrc, Reg),
    Cqo,
    Ret,
    IMul(Reg, Reg),
    IDiv(Reg, Reg),
    Add(Reg, Reg),
    Sub(Reg, Reg),
}

impl fmt::Display for AsmInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmInstruction::Section(section) => f.write_fmt(format_args!(".{}", section)),
            AsmInstruction::Variable(var, val) => match val {
                Some(v) => f.write_fmt(format_args!("  .{} {}", var, v)),
                None => f.write_fmt(format_args!("  .{}", var)),
            },
            AsmInstruction::Label(label) => f.write_fmt(format_args!("{}:", label)),
            AsmInstruction::Xor(left, right) => {
                f.write_fmt(format_args!("  xor {}, {}", left, right))
            }
            AsmInstruction::Push(reg) => f.write_fmt(format_args!("  push {}", reg)),
            AsmInstruction::Pop(reg) => f.write_fmt(format_args!("  pop {}", reg)),
            AsmInstruction::Ret => f.write_fmt(format_args!("  ret")),
            AsmInstruction::Call(fn_name) => f.write_fmt(format_args!("  call {}", fn_name)),
            AsmInstruction::Mov(left, right) => {
                f.write_fmt(format_args!("  mov {}, {}", left, right))
            }
            AsmInstruction::Cqo => f.write_fmt(format_args!("  cqo")),
            AsmInstruction::IMul(left, right) => {
                f.write_fmt(format_args!("  imul {}, {}", left, right))
            }
            AsmInstruction::IDiv(left, right) => {
                f.write_fmt(format_args!("  idiv {}, {}", left, right))
            }
            AsmInstruction::Add(left, right) => {
                f.write_fmt(format_args!("  add {}, {}", left, right))
            }
            AsmInstruction::Sub(left, right) => {
                f.write_fmt(format_args!("  sub {}, {}", left, right))
            }
        }
    }
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            depth: 0,
            instructions: vec![],
        }
    }

    pub fn program(&mut self, expr: &Expr) -> Vec<AsmInstruction> {
        self.prologue();
        self.expr(expr);
        self.epilogue();
        self.instructions.clone()
    }

    fn prologue(&mut self) {
        self.add(AsmInstruction::Variable("text".to_string(), None));
        self.add(AsmInstruction::Label(".LC0".to_string()));
        self.add(AsmInstruction::Variable(
            "string".to_string(),
            Some("\"%d\\n\"".to_string()),
        ));
        self.add(AsmInstruction::Variable(
            "globl".to_string(),
            Some("main".to_string()),
        ));
        self.add(AsmInstruction::Label("main".to_string()));
    }

    fn epilogue(&mut self) {
        self.add(AsmInstruction::Push(Reg::Rsp));
        self.add(AsmInstruction::Mov(MoveSrc::Reg(Reg::Rax), Reg::Rsi));
        self.add(AsmInstruction::Mov(
            MoveSrc::Label(".LC0".to_string()),
            Reg::Rdi,
        ));
        self.add(AsmInstruction::Xor(Reg::Rax, Reg::Rax));
        self.add(AsmInstruction::Call("printf".to_string()));
        self.add(AsmInstruction::Xor(Reg::Rax, Reg::Rax));
        self.add(AsmInstruction::Pop(Reg::Rsp));
        self.add(AsmInstruction::Ret);
    }

    fn add(&mut self, instruction: AsmInstruction) {
        self.instructions.push(instruction);
    }

    fn push(&mut self) {
        self.add(AsmInstruction::Push(Reg::Rax));
        self.depth += 1;
    }

    fn pop(&mut self, reg: Reg) {
        self.add(AsmInstruction::Pop(reg));
        self.depth -= 1;
    }

    fn expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal { value } => match value {
                Value::Number(val) => {
                    self.add(AsmInstruction::Mov(
                        MoveSrc::Label(val.to_string()),
                        Reg::Rax,
                    ));
                }
                _ => todo!(),
            },
            Expr::Binary {
                ref left,
                op,
                ref right,
            } => match op {
                Token {
                    r#type: TokenType::Op(Op::Slash),
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cqo);
                    self.add(AsmInstruction::IDiv(Reg::Rdi, Reg::Rax));
                }
                Token {
                    r#type: TokenType::Op(Op::Plus),
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Add(Reg::Rdi, Reg::Rax));
                }
                Token {
                    r#type: TokenType::Op(Op::Minus),
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Sub(Reg::Rdi, Reg::Rax));
                }
                Token {
                    r#type: TokenType::Op(Op::Star),
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::IMul(Reg::Rdi, Reg::Rax));
                }
                _ => todo!(),
            },
            Expr::Grouping { expr } => {
                self.expr(expr);
            }
        }
    }

    fn bin_op_fetch(&mut self, left: &Expr, right: &Expr) {
        self.expr(right);
        self.push();
        self.expr(left);
        self.pop(Reg::Rdi);
    }
}

impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }
}
