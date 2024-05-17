use std::{collections::HashMap, fmt};

use crate::{
    expr::Expr,
    stmt::Stmt,
    token::{Object, Token, TokenType},
};

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Codegen {
    depth: i64,
    instructions: Vec<AsmInstruction>,
    label_count: u64,
    stack_offset: i64,
    vars: HashMap<Token, i64>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Reg {
    Rax,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    Al,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Address {
    Reg(Reg),
    Label(String),
    Immediate(f64),
    Indirect(Reg),
    IndirectOffset(i64, Reg),
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Address::Reg(reg) => f.write_fmt(format_args!("{}", reg)),
            Address::Label(label) => f.write_fmt(format_args!("$.{}", label)),
            Address::Immediate(num) => f.write_fmt(format_args!("${}", num)),
            Address::Indirect(reg) => f.write_fmt(format_args!("({})", reg)),
            Address::IndirectOffset(offset, reg) => {
                f.write_fmt(format_args!("{}({})", offset, reg))
            }
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Reg::Rax => "%rax",
            Reg::Rsp => "%rsp",
            Reg::Rbp => "%rbp",
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
    Lea(Address, Address),
    Mov(Address, Address),
    Movzb(Address, Address),
    Sete(Reg),
    Setne(Reg),
    Setl(Reg),
    Setle(Reg),
    Setg(Reg),
    Setge(Reg),
    Cqo,
    Ret,
    Test(Reg, Reg),
    Cmp(Address, Reg),
    IMul(Reg, Reg),
    IDiv(Reg, Reg),
    Add(Reg, Reg),
    Sub(Address, Reg),
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
            Lea(left, right) => f.write_fmt(format_args!("  lea {}, {}", left, right)),
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
        for stmt in stmts {
            self.stmt(stmt);
        }
        let size = self.vars.len();
        let prog = self.instructions.clone();
        self.instructions.clear();
        self.prologue(size);
        self.instructions.extend(prog);
        self.epilogue();
        self.instructions.clone()
    }

    fn prologue(&mut self, size: usize) {
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

        // setup variable stack
        self.add(AsmInstruction::Push(Reg::Rbp));
        self.add(AsmInstruction::Mov(
            Address::Reg(Reg::Rsp),
            Address::Reg(Reg::Rbp),
        ));
        self.add(AsmInstruction::Sub(
            Address::Immediate((size * 8) as f64),
            Reg::Rsp,
        ));
    }

    fn epilogue(&mut self) {
        // tear down variable stack
        self.add(AsmInstruction::Pop(Reg::Rsp));
        self.add(AsmInstruction::Mov(
            Address::Reg(Reg::Rbp),
            Address::Reg(Reg::Rsp),
        ));
        self.add(AsmInstruction::Pop(Reg::Rbp));
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

    fn offset(&mut self) -> i64 {
        self.stack_offset -= 8;
        self.stack_offset
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr { expr } => self.expr(expr),
            Stmt::Var { name, initializer } => {
                if let Some(init) = initializer {
                    self.expr(init);
                } else {
                    self.expr(&Expr::Literal { value: Object::Nil });
                }
                let offset = self.offset();
                self.vars.insert(name.clone(), offset);
                self.add(AsmInstruction::Mov(
                    Address::Reg(Reg::Rax),
                    Address::IndirectOffset(offset, Reg::Rbp),
                ));
            }
            Stmt::Print { expr } => {
                self.expr(expr);
                self.add(AsmInstruction::Push(Reg::Rsp));
                self.add(AsmInstruction::Mov(
                    Address::Reg(Reg::Rax),
                    Address::Reg(Reg::Rsi),
                ));
                self.add(AsmInstruction::Mov(
                    Address::Label("format".to_string()),
                    Address::Reg(Reg::Rdi),
                ));
                self.add(AsmInstruction::Xor(Reg::Rax, Reg::Rax));
                self.add(AsmInstruction::Call("printf".to_string()));
                self.add(AsmInstruction::Xor(Reg::Rax, Reg::Rax));
            }
        }
    }

    fn expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal { value } => match value {
                Object::Number(val) => {
                    self.add(AsmInstruction::Mov(
                        Address::Immediate(*val),
                        Address::Reg(Reg::Rax),
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
                    r#type: TokenType::Slash,
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cqo);
                    self.add(AsmInstruction::IDiv(Reg::Rdi, Reg::Rax));
                }
                Token {
                    r#type: TokenType::Plus,
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Add(Reg::Rdi, Reg::Rax));
                }
                Token {
                    r#type: TokenType::Minus,
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Sub(Address::Reg(Reg::Rdi), Reg::Rax));
                }
                Token {
                    r#type: TokenType::Star,
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::IMul(Reg::Rdi, Reg::Rax));
                }
                Token {
                    r#type: TokenType::EqualEqual,
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cmp(Address::Reg(Reg::Rdi), Reg::Rax));
                    self.add(AsmInstruction::Sete(Reg::Al));
                    self.add(AsmInstruction::Movzb(
                        Address::Reg(Reg::Al),
                        Address::Reg(Reg::Rax),
                    ));
                }
                Token {
                    r#type: TokenType::BangEqual,
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cmp(Address::Reg(Reg::Rdi), Reg::Rax));
                    self.add(AsmInstruction::Setne(Reg::Al));
                    self.add(AsmInstruction::Movzb(
                        Address::Reg(Reg::Al),
                        Address::Reg(Reg::Rax),
                    ));
                }
                Token {
                    r#type: TokenType::Less,
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cmp(Address::Reg(Reg::Rdi), Reg::Rax));
                    self.add(AsmInstruction::Setl(Reg::Al));
                    self.add(AsmInstruction::Movzb(
                        Address::Reg(Reg::Al),
                        Address::Reg(Reg::Rax),
                    ));
                }
                Token {
                    r#type: TokenType::LessEqual,
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cmp(Address::Reg(Reg::Rdi), Reg::Rax));
                    self.add(AsmInstruction::Setle(Reg::Al));
                    self.add(AsmInstruction::Movzb(
                        Address::Reg(Reg::Al),
                        Address::Reg(Reg::Rax),
                    ));
                }
                Token {
                    r#type: TokenType::Greater,
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cmp(Address::Reg(Reg::Rdi), Reg::Rax));
                    self.add(AsmInstruction::Setg(Reg::Al));
                    self.add(AsmInstruction::Movzb(
                        Address::Reg(Reg::Al),
                        Address::Reg(Reg::Rax),
                    ));
                }
                Token {
                    r#type: TokenType::GreaterEqual,
                    ..
                } => {
                    self.bin_op_fetch(left, right);
                    self.add(AsmInstruction::Cmp(Address::Reg(Reg::Rdi), Reg::Rax));
                    self.add(AsmInstruction::Setge(Reg::Al));
                    self.add(AsmInstruction::Movzb(
                        Address::Reg(Reg::Al),
                        Address::Reg(Reg::Rax),
                    ));
                }
                _ => todo!(),
            },
            Expr::Grouping { expr } => {
                self.expr(expr);
            }
            Expr::Unary { op, expr } => {
                if let Token {
                    r#type: TokenType::Minus,
                    ..
                } = op
                {
                    self.expr(expr);
                    self.add(AsmInstruction::Neg(Reg::Rax));
                }
            }
            Expr::Logical { left, op, right } => match op {
                Token {
                    r#type: TokenType::And,
                    ..
                } => {
                    let count = self.get_count();
                    self.expr(left);
                    self.add(AsmInstruction::Cmp(Address::Immediate(0.0), Reg::Rax));
                    self.add(AsmInstruction::Je(format!(".L.false.{}", count)));
                    self.expr(right);
                    self.add(AsmInstruction::Cmp(Address::Immediate(0.0), Reg::Rax));
                    self.add(AsmInstruction::Je(format!(".L.false.{}", count)));
                    self.add(AsmInstruction::Mov(
                        Address::Immediate(1.0),
                        Address::Reg(Reg::Rax),
                    ));
                    self.add(AsmInstruction::Jmp(format!(".L.end.{}", count)));
                    self.add(AsmInstruction::Label(format!(".L.false.{}", count)));
                    self.add(AsmInstruction::Mov(
                        Address::Immediate(0.0),
                        Address::Reg(Reg::Rax),
                    ));
                    self.add(AsmInstruction::Label(format!(".L.end.{}", count)));
                }
                Token {
                    r#type: TokenType::Or,
                    ..
                } => {
                    let count = self.get_count();
                    self.expr(left);
                    self.add(AsmInstruction::Cmp(Address::Immediate(0.0), Reg::Rax));
                    self.add(AsmInstruction::Jne(format!(".L.true.{}", count)));
                    self.expr(right);
                    self.add(AsmInstruction::Cmp(Address::Immediate(0.0), Reg::Rax));
                    self.add(AsmInstruction::Jne(format!(".L.true.{}", count)));
                    self.add(AsmInstruction::Mov(
                        Address::Immediate(0.0),
                        Address::Reg(Reg::Rax),
                    ));
                    self.add(AsmInstruction::Jmp(format!(".L.end.{}", count)));
                    self.add(AsmInstruction::Label(format!(".L.true.{}", count)));
                    self.add(AsmInstruction::Mov(
                        Address::Immediate(1.0),
                        Address::Reg(Reg::Rax),
                    ));
                    self.add(AsmInstruction::Label(format!(".L.end.{}", count)));
                }
                _ => unreachable!(),
            },
            Expr::Var { name } => {
                self.add_offset(name);
                self.add(AsmInstruction::Mov(
                    Address::Indirect(Reg::Rax),
                    Address::Reg(Reg::Rax),
                ));
            }
            Expr::Assign { name, expr } => {
                self.add_offset(name);
                self.push();
                self.expr(expr);
                self.pop(Reg::Rdi);
                self.add(AsmInstruction::Mov(
                    Address::Reg(Reg::Rax),
                    Address::Indirect(Reg::Rdi),
                ));
            }
        }
    }

    fn add_offset(&mut self, name: &Token) {
        let offset = self.vars.get(name);

        if let Some(offset) = offset {
            self.add(AsmInstruction::Lea(
                Address::IndirectOffset(*offset, Reg::Rbp),
                Address::Reg(Reg::Rax),
            ));
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
