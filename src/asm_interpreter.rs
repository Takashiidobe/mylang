use std::fmt;

use crate::{
    expr::Expr,
    lexer::{Op, Token, TokenType, Value},
    stmt::Stmt,
};

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Codegen {
    depth: i64,
    instructions: Vec<AsmInstruction>,
    label_count: u64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Reg {
    Rax,
    Rsp,
    Rsi,
    Rdi,
    Al,
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
            MoveSrc::Label(label) => f.write_fmt(format_args!("$.{}", label)),
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
            Reg::Al => "%al",
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
    Movzb(MoveSrc, Reg),
    Sete(Reg),
    Setne(Reg),
    Setl(Reg),
    Setle(Reg),
    Setg(Reg),
    Setge(Reg),
    Cqo,
    Ret,
    Test(Reg, Reg),
    Cmp(MoveSrc, Reg),
    IMul(Reg, Reg),
    IDiv(Reg, Reg),
    Add(Reg, Reg),
    Sub(Reg, Reg),
    Neg(Reg),
    Je(String),
    Jne(String),
    Jmp(String),
    Jz(String),
}

impl fmt::Display for AsmInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use AsmInstruction::*;
        match self {
            Section(section) => f.write_fmt(format_args!(".{}", section)),
            Variable(var, val) => match val {
                Some(v) => f.write_fmt(format_args!("  .{} {}", var, v)),
                None => f.write_fmt(format_args!("  .{}", var)),
            },
            Label(label) => f.write_fmt(format_args!("{}:", label)),
            Xor(left, right) => f.write_fmt(format_args!("  xor {}, {}", left, right)),
            Push(reg) => f.write_fmt(format_args!("  push {}", reg)),
            Pop(reg) => f.write_fmt(format_args!("  pop {}", reg)),
            Sete(reg) => f.write_fmt(format_args!("  sete {}", reg)),
            Setne(reg) => f.write_fmt(format_args!("  setne {}", reg)),
            Setl(reg) => f.write_fmt(format_args!("  setl {}", reg)),
            Setle(reg) => f.write_fmt(format_args!("  setle {}", reg)),
            Setg(reg) => f.write_fmt(format_args!("  setg {}", reg)),
            Setge(reg) => f.write_fmt(format_args!("  setge {}", reg)),
            Ret => f.write_fmt(format_args!("  ret")),
            Call(fn_name) => f.write_fmt(format_args!("  call {}", fn_name)),
            Test(left, right) => f.write_fmt(format_args!("  test {}, {}", left, right)),
            Cmp(left, right) => f.write_fmt(format_args!("  cmp {}, {}", left, right)),
            Mov(left, right) => f.write_fmt(format_args!("  mov {}, {}", left, right)),
            Movzb(left, right) => f.write_fmt(format_args!("  movzb {}, {}", left, right)),
            Cqo => f.write_fmt(format_args!("  cqo")),
            IMul(left, right) => f.write_fmt(format_args!("  imul {}, {}", left, right)),
            IDiv(left, right) => f.write_fmt(format_args!("  idiv {}, {}", left, right)),
            Add(left, right) => f.write_fmt(format_args!("  add {}, {}", left, right)),
            Sub(left, right) => f.write_fmt(format_args!("  sub {}, {}", left, right)),
            Neg(reg) => f.write_fmt(format_args!("  neg {}", reg)),
            Jne(reg) => f.write_fmt(format_args!("  jne {}", reg)),
            Je(reg) => f.write_fmt(format_args!("  je {}", reg)),
            Jz(reg) => f.write_fmt(format_args!("  jz {}", reg)),
            Jmp(reg) => f.write_fmt(format_args!("  jmp {}", reg)),
        }
    }
}

impl Codegen {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn program(&mut self, stmts: &[Stmt]) -> Vec<AsmInstruction> {
        self.prologue();
        for stmt in stmts {
            self.stmt(stmt);
        }
        self.epilogue();
        self.instructions.clone()
    }

    fn prologue(&mut self) {
        self.add(AsmInstruction::Variable("text".to_string(), None));
        self.add(AsmInstruction::Label(".format".to_string()));
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
            MoveSrc::Label("format".to_string()),
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

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr { expr } => self.expr(expr),
        }
    }

    fn expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal { value } => match value {
                Value::Number(val) => {
                    self.add(AsmInstruction::Mov(MoveSrc::Number(*val), Reg::Rax));
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
                Token {
                    r#type: TokenType::Op(Op::EqualEqual),
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cmp(MoveSrc::Reg(Reg::Rdi), Reg::Rax));
                    self.add(AsmInstruction::Sete(Reg::Al));
                    self.add(AsmInstruction::Movzb(MoveSrc::Reg(Reg::Al), Reg::Rax));
                }
                Token {
                    r#type: TokenType::Op(Op::BangEqual),
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cmp(MoveSrc::Reg(Reg::Rdi), Reg::Rax));
                    self.add(AsmInstruction::Setne(Reg::Al));
                    self.add(AsmInstruction::Movzb(MoveSrc::Reg(Reg::Al), Reg::Rax));
                }
                Token {
                    r#type: TokenType::Op(Op::Lt),
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cmp(MoveSrc::Reg(Reg::Rdi), Reg::Rax));
                    self.add(AsmInstruction::Setl(Reg::Al));
                    self.add(AsmInstruction::Movzb(MoveSrc::Reg(Reg::Al), Reg::Rax));
                }
                Token {
                    r#type: TokenType::Op(Op::Le),
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cmp(MoveSrc::Reg(Reg::Rdi), Reg::Rax));
                    self.add(AsmInstruction::Setle(Reg::Al));
                    self.add(AsmInstruction::Movzb(MoveSrc::Reg(Reg::Al), Reg::Rax));
                }
                Token {
                    r#type: TokenType::Op(Op::Gt),
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cmp(MoveSrc::Reg(Reg::Rdi), Reg::Rax));
                    self.add(AsmInstruction::Setg(Reg::Al));
                    self.add(AsmInstruction::Movzb(MoveSrc::Reg(Reg::Al), Reg::Rax));
                }
                Token {
                    r#type: TokenType::Op(Op::Ge),
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cmp(MoveSrc::Reg(Reg::Rdi), Reg::Rax));
                    self.add(AsmInstruction::Setge(Reg::Al));
                    self.add(AsmInstruction::Movzb(MoveSrc::Reg(Reg::Al), Reg::Rax));
                }
                _ => todo!(),
            },
            Expr::Grouping { expr } => {
                self.expr(expr);
            }
            Expr::Unary { op, expr } => {
                if let Token {
                    r#type: TokenType::Op(Op::Minus),
                    ..
                } = op
                {
                    self.expr(expr);
                    self.add(AsmInstruction::Neg(Reg::Rax));
                }
            }
            Expr::Logical { left, op, right } => match op {
                Token {
                    r#type: TokenType::Op(Op::And),
                    ..
                } => {
                    let count = self.get_count();
                    self.expr(left);
                    self.add(AsmInstruction::Cmp(MoveSrc::Number(0), Reg::Rax));
                    self.add(AsmInstruction::Je(format!(".L.false.{}", count)));
                    self.expr(right);
                    self.add(AsmInstruction::Cmp(MoveSrc::Number(0), Reg::Rax));
                    self.add(AsmInstruction::Je(format!(".L.false.{}", count)));
                    self.add(AsmInstruction::Mov(MoveSrc::Number(1), Reg::Rax));
                    self.add(AsmInstruction::Jmp(format!(".L.end.{}", count)));
                    self.add(AsmInstruction::Label(format!(".L.false.{}", count)));
                    self.add(AsmInstruction::Mov(MoveSrc::Number(0), Reg::Rax));
                    self.add(AsmInstruction::Label(format!(".L.end.{}", count)));
                }
                Token {
                    r#type: TokenType::Op(Op::Or),
                    ..
                } => {
                    let count = self.get_count();
                    self.expr(left);
                    self.add(AsmInstruction::Cmp(MoveSrc::Number(0), Reg::Rax));
                    self.add(AsmInstruction::Jne(format!(".L.true.{}", count)));
                    self.expr(right);
                    self.add(AsmInstruction::Cmp(MoveSrc::Number(0), Reg::Rax));
                    self.add(AsmInstruction::Jne(format!(".L.true.{}", count)));
                    self.add(AsmInstruction::Mov(MoveSrc::Number(0), Reg::Rax));
                    self.add(AsmInstruction::Jmp(format!(".L.end.{}", count)));
                    self.add(AsmInstruction::Label(format!(".L.true.{}", count)));
                    self.add(AsmInstruction::Mov(MoveSrc::Number(1), Reg::Rax));
                    self.add(AsmInstruction::Label(format!(".L.end.{}", count)));
                }
                _ => unreachable!(),
            },
            Expr::Stmt { expr } => self.expr(expr),
        }
    }

    fn get_count(&mut self) -> u64 {
        let res = self.label_count;
        self.label_count += 1;
        res
    }

    fn bin_op_fetch(&mut self, left: &Expr, right: &Expr) {
        self.expr(right);
        self.push();
        self.expr(left);
        self.pop(Reg::Rdi);
    }
}
