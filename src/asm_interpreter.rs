use std::borrow::Borrow;
use std::{collections::HashMap, fmt};

use crate::token::ObjType;
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
    anon_count: u64,
    stack_offset: i64,
    pub vars: HashMap<Token, (OffsetOrLabel, ObjType)>,
    pub functions: HashMap<Token, ObjType>,
    pub labels: HashMap<String, String>,
    strings: Vec<AsmInstruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Reg {
    Rax,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    Rdx,
    Rcx,
    R8,
    R9,
    Al,
    Rip,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Address {
    Reg(Reg),
    Label(String),
    Immediate(f64),
    Indirect(Reg),
    IndirectOffset(i64, Reg),
    LabelOffset(String, Reg),
}

#[derive(Debug, Clone, PartialEq)]
pub enum OffsetOrLabel {
    Offset(i64),
    Label(String),
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
            Address::LabelOffset(offset, reg) => f.write_fmt(format_args!("{}({})", offset, reg)),
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
            Reg::Rdx => "%rdx",
            Reg::Al => "%al",
            Reg::Rcx => "%rcx",
            Reg::R8 => "%r8",
            Reg::R9 => "%r9",
            Reg::Rip => "%rip",
        })
    }
}

const ARG_REGS: [Reg; 6] = [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9];

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
    Byte(u8),
    Leave,
    Comment(String),
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
            Byte(b) => f.write_fmt(format_args!("  .byte {}", b)),
            Leave => f.write_fmt(format_args!("  leave")),
            Comment(comment) => f.write_fmt(format_args!("  # {}", comment)),
        }
    }
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn program(&mut self, stmts: &[Stmt]) -> Vec<AsmInstruction> {
        let mut body = vec![];
        let mut functions = vec![];
        for stmt in stmts {
            match stmt {
                Stmt::Function { .. } => {
                    functions.extend(self.stmt(stmt));
                }
                _ => {
                    body.extend(self.stmt(stmt));
                }
            }
        }
        let size = self.vars.len();
        let mut program = vec![AsmInstruction::Variable("text".to_string(), None)];
        let prologue = self.prologue(size);
        let epilogue = self.epilogue();

        program.extend(functions);
        program.extend(prologue);
        program.extend(body);
        program.extend(epilogue);
        if !self.strings.is_empty() {
            program.push(AsmInstruction::Variable("data".to_string(), None));
            program.extend(self.strings.clone());
        }
        program
    }

    // return a 16 byte aligned stack for rsp
    // align the number to a multiple
    // eg. 3 8 byte items aligned to a 16 byte boundary would return 32.
    fn align_to(&self, size: usize, multiple: usize) -> usize {
        size.div_ceil(multiple) * multiple
    }

    fn prologue(&mut self, size: usize) -> Vec<AsmInstruction> {
        vec![
            AsmInstruction::Variable("globl".to_string(), Some("main".to_string())),
            AsmInstruction::Label("main".to_string()),
            AsmInstruction::Push(Reg::Rbp),
            AsmInstruction::Mov(Address::Reg(Reg::Rsp), Address::Reg(Reg::Rbp)),
            AsmInstruction::Sub(
                Address::Immediate(self.align_to(size * 8, 16) as f64),
                Reg::Rsp,
            ),
        ]
    }

    fn epilogue(&mut self) -> Vec<AsmInstruction> {
        vec![
            AsmInstruction::Leave,
            AsmInstruction::Xor(Reg::Rax, Reg::Rax),
            AsmInstruction::Ret,
            AsmInstruction::Variable("section".to_string(), Some(".rodata".to_string())),
            AsmInstruction::Label(".format".to_string()),
            AsmInstruction::Variable("string".to_string(), Some("\"%d\\n\"".to_string())),
            AsmInstruction::Label(".format_s".to_string()),
            AsmInstruction::Variable("string".to_string(), Some("\"%s\\n\"".to_string())),
        ]
    }

    fn push(&mut self) -> AsmInstruction {
        self.depth += 1;
        AsmInstruction::Push(Reg::Rax)
    }

    fn pop(&mut self, reg: Reg) -> AsmInstruction {
        self.depth -= 1;
        AsmInstruction::Pop(reg)
    }

    fn offset(&mut self, obj: Option<Object>, obj_type: ObjType) -> OffsetOrLabel {
        // pass in the object as well
        // if the object type is a string, we check the labels and just emit that
        if obj_type == ObjType::Number {
            self.stack_offset -= 8;
            OffsetOrLabel::Offset(self.stack_offset)
        } else if let Some(Object::String(string)) = obj {
            if let Some(label) = self.labels.get(&string) {
                OffsetOrLabel::Label(label.to_string())
            } else {
                let label = format!(".L.anon.{}", self.get_anon_count());
                OffsetOrLabel::Label(label)
            }
        } else {
            let label = format!(".L.anon.{}", self.get_anon_count());
            OffsetOrLabel::Label(label)
        }
    }

    // make it so every expr returns a vector of results
    fn stmt(&mut self, stmt: &Stmt) -> Vec<AsmInstruction> {
        match stmt {
            Stmt::Expr { expr } => self.expr(expr).0,
            Stmt::Var { name, initializer } => {
                let (mut res, r_type) = if let Some(init) = initializer {
                    self.expr(init)
                } else {
                    self.expr(&Expr::Literal { value: Object::Nil })
                };
                let offset = self.offset(None, r_type);

                self.vars.insert(name.clone(), (offset.clone(), r_type));
                match offset {
                    OffsetOrLabel::Offset(off) => {
                        res.push(AsmInstruction::Mov(
                            Address::Reg(Reg::Rax),
                            Address::IndirectOffset(off, Reg::Rbp),
                        ));
                    }
                    OffsetOrLabel::Label(_) => {}
                }
                res
            }
            Stmt::Print { expr } => {
                let (mut res, r_type) = self.expr(expr);

                if let Expr::Var { name } = expr {
                    if let Some(label) = self.vars.get(name) {
                        match &label.0 {
                            OffsetOrLabel::Label(_) => {
                                res.push(AsmInstruction::Comment("accessing string".to_string()));
                            }
                            OffsetOrLabel::Offset(_) => {
                                res.push(AsmInstruction::Comment("loading number".to_string()));
                            }
                        }
                    }
                }

                if r_type == ObjType::Number {
                    res.push(AsmInstruction::Mov(
                        Address::Label("format".to_string()),
                        Address::Reg(Reg::Rdi),
                    ));
                } else {
                    res.push(AsmInstruction::Mov(
                        Address::Label("format_s".to_string()),
                        Address::Reg(Reg::Rdi),
                    ));
                }

                res.push(AsmInstruction::Mov(
                    Address::Reg(Reg::Rax),
                    Address::Reg(Reg::Rsi),
                ));

                res.push(AsmInstruction::Xor(Reg::Rax, Reg::Rax));
                res.push(AsmInstruction::Call("printf".to_string()));
                res
            }
            Stmt::Block { stmts } => stmts.iter().flat_map(|x| self.stmt(x)).collect(),
            Stmt::If { cond, then, r#else } => {
                let count = self.get_count();
                let mut res = self.expr(cond).0;
                res.push(AsmInstruction::Cmp(Address::Immediate(0.0), Reg::Rax));
                res.push(AsmInstruction::Je(format!(".L.else.{}", count)));
                res.extend(self.stmt(then));
                res.push(AsmInstruction::Jmp(format!(".L.end.{}", count)));
                res.push(AsmInstruction::Label(format!(".L.else.{}", count)));
                if let Some(else_branch) = r#else.borrow() {
                    res.extend(self.stmt(else_branch));
                }
                res.push(AsmInstruction::Label(format!(".L.end.{}", count)));
                res
            }
            Stmt::While { cond, body } => {
                let count = self.get_count();

                let mut res = vec![];

                res.push(AsmInstruction::Label(format!(".L.begin.{}", count)));
                res.extend(self.expr(cond).0);
                res.push(AsmInstruction::Cmp(Address::Immediate(0.0), Reg::Rax));
                res.push(AsmInstruction::Je(format!(".L.end.{}", count)));
                res.extend(self.stmt(body));
                res.push(AsmInstruction::Jmp(format!(".L.begin.{}", count)));
                res.push(AsmInstruction::Label(format!(".L.end.{}", count)));

                res
            }
            Stmt::Function {
                name,
                body,
                params,
                return_type,
            } => {
                // add into functions the return type of the function
                self.functions.insert(name.clone(), *return_type);
                let mut res = vec![
                    AsmInstruction::Label(name.to_string()),
                    AsmInstruction::Push(Reg::Rbp),
                    AsmInstruction::Mov(Address::Reg(Reg::Rsp), Address::Reg(Reg::Rbp)),
                ];
                res.push(AsmInstruction::Sub(
                    Address::Immediate(self.align_to((params.len() + 2) * 8, 16) as f64),
                    Reg::Rsp,
                ));

                for (i, reg) in ARG_REGS.iter().enumerate() {
                    // we only set args if needed
                    if i < params.len() {
                        res.push(AsmInstruction::Mov(
                            Address::Reg(reg.clone()),
                            Address::IndirectOffset((i as i64 + 2) * -8, Reg::Rbp),
                        ));
                    }
                }

                for (i, param) in params.iter().enumerate() {
                    let offset = if i < 6 {
                        (i as i64 + 2) * -8
                    } else {
                        (i as i64 - 4) * 8
                    };
                    self.vars.insert(
                        param.clone(),
                        (OffsetOrLabel::Offset(offset), ObjType::Number),
                    );
                }

                for stmt in body {
                    res.extend(self.stmt(stmt));
                }

                res.push(AsmInstruction::Leave);
                res.push(AsmInstruction::Ret);
                res.extend(self.instructions.clone());
                res
            }
            Stmt::Return { value, .. } => {
                if let Some(val) = value {
                    self.expr(val).0
                } else {
                    self.expr(&Expr::Literal {
                        value: Object::Number(0.0),
                    })
                    .0
                }
            }
        }
    }

    // every expr should return a Vec<AsmInstruction>
    fn expr(&mut self, expr: &Expr) -> (Vec<AsmInstruction>, ObjType) {
        match expr {
            Expr::Literal { value } => match value {
                Object::Number(val) => (
                    vec![AsmInstruction::Mov(
                        Address::Immediate(*val),
                        Address::Reg(Reg::Rax),
                    )],
                    ObjType::Number,
                ),
                Object::String(string) => {
                    let label = match self
                        .offset(Some(Object::String(string.to_string())), ObjType::String)
                    {
                        OffsetOrLabel::Label(l) => l,
                        _ => unreachable!(),
                    };
                    let mut res = vec![
                        AsmInstruction::Variable("globl".to_string(), Some(label.clone())),
                        AsmInstruction::Label(label.clone()),
                    ];
                    // set the label here
                    self.labels.insert(string.to_string(), label.to_string());

                    for b in string.bytes() {
                        res.push(AsmInstruction::Byte(b));
                    }

                    res.push(AsmInstruction::Byte(0));

                    self.strings.extend(res);

                    (
                        vec![AsmInstruction::Lea(
                            Address::LabelOffset(label, Reg::Rip),
                            Address::Reg(Reg::Rax),
                        )],
                        ObjType::String,
                    )
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
                    let mut res = self.bin_op_fetch(left, right);
                    res.push(AsmInstruction::Cqo);
                    res.push(AsmInstruction::IDiv(Reg::Rdi, Reg::Rax));
                    (res, ObjType::Number)
                }
                Token {
                    r#type: TokenType::Plus,
                    ..
                } => {
                    let mut res = self.bin_op_fetch(left, right);
                    res.push(AsmInstruction::Add(Reg::Rdi, Reg::Rax));
                    (res, ObjType::Number)
                }
                Token {
                    r#type: TokenType::Minus,
                    ..
                } => {
                    let mut res = self.bin_op_fetch(left, right);
                    res.push(AsmInstruction::Sub(Address::Reg(Reg::Rdi), Reg::Rax));
                    (res, ObjType::Number)
                }
                Token {
                    r#type: TokenType::Star,
                    ..
                } => {
                    let mut res = self.bin_op_fetch(left, right);
                    res.push(AsmInstruction::IMul(Reg::Rdi, Reg::Rax));
                    (res, ObjType::Number)
                }
                Token {
                    r#type: TokenType::EqualEqual,
                    ..
                } => {
                    let mut res = self.bin_op_fetch(left, right);
                    res.push(AsmInstruction::Cmp(Address::Reg(Reg::Rdi), Reg::Rax));
                    res.push(AsmInstruction::Sete(Reg::Al));
                    res.push(AsmInstruction::Movzb(
                        Address::Reg(Reg::Al),
                        Address::Reg(Reg::Rax),
                    ));
                    (res, ObjType::Bool)
                }
                Token {
                    r#type: TokenType::BangEqual,
                    ..
                } => {
                    let mut res = self.bin_op_fetch(left, right);
                    res.push(AsmInstruction::Cmp(Address::Reg(Reg::Rdi), Reg::Rax));
                    res.push(AsmInstruction::Setne(Reg::Al));
                    res.push(AsmInstruction::Movzb(
                        Address::Reg(Reg::Al),
                        Address::Reg(Reg::Rax),
                    ));
                    (res, ObjType::Bool)
                }
                Token {
                    r#type: TokenType::Less,
                    ..
                } => {
                    let mut res = self.bin_op_fetch(left, right);
                    res.push(AsmInstruction::Cmp(Address::Reg(Reg::Rdi), Reg::Rax));
                    res.push(AsmInstruction::Setl(Reg::Al));
                    res.push(AsmInstruction::Movzb(
                        Address::Reg(Reg::Al),
                        Address::Reg(Reg::Rax),
                    ));
                    (res, ObjType::Bool)
                }
                Token {
                    r#type: TokenType::LessEqual,
                    ..
                } => {
                    let mut res = self.bin_op_fetch(left, right);
                    res.push(AsmInstruction::Cmp(Address::Reg(Reg::Rdi), Reg::Rax));
                    res.push(AsmInstruction::Setle(Reg::Al));
                    res.push(AsmInstruction::Movzb(
                        Address::Reg(Reg::Al),
                        Address::Reg(Reg::Rax),
                    ));
                    (res, ObjType::Bool)
                }
                Token {
                    r#type: TokenType::Greater,
                    ..
                } => {
                    let mut res = self.bin_op_fetch(left, right);
                    res.push(AsmInstruction::Cmp(Address::Reg(Reg::Rdi), Reg::Rax));
                    res.push(AsmInstruction::Setg(Reg::Al));
                    res.push(AsmInstruction::Movzb(
                        Address::Reg(Reg::Al),
                        Address::Reg(Reg::Rax),
                    ));
                    (res, ObjType::Bool)
                }
                Token {
                    r#type: TokenType::GreaterEqual,
                    ..
                } => {
                    let mut res = self.bin_op_fetch(left, right);
                    res.push(AsmInstruction::Cmp(Address::Reg(Reg::Rdi), Reg::Rax));
                    res.push(AsmInstruction::Setge(Reg::Al));
                    res.push(AsmInstruction::Movzb(
                        Address::Reg(Reg::Al),
                        Address::Reg(Reg::Rax),
                    ));
                    (res, ObjType::Bool)
                }
                _ => todo!(),
            },
            Expr::Grouping { expr } => self.expr(expr),
            Expr::Unary { op, expr } => {
                if let Token {
                    r#type: TokenType::Minus,
                    ..
                } = op
                {
                    let (mut res, r_type) = self.expr(expr);
                    res.push(AsmInstruction::Neg(Reg::Rax));
                    return (res, r_type);
                }
                todo!()
            }
            Expr::Logical { left, op, right } => match op {
                Token {
                    r#type: TokenType::And,
                    ..
                } => {
                    let count = self.get_count();
                    let (mut res, r_type) = self.expr(left);
                    res.push(AsmInstruction::Cmp(Address::Immediate(0.0), Reg::Rax));
                    res.push(AsmInstruction::Je(format!(".L.false.{}", count)));
                    res.extend(self.expr(right).0);
                    res.push(AsmInstruction::Cmp(Address::Immediate(0.0), Reg::Rax));
                    res.push(AsmInstruction::Je(format!(".L.false.{}", count)));
                    res.push(AsmInstruction::Mov(
                        Address::Immediate(1.0),
                        Address::Reg(Reg::Rax),
                    ));
                    res.push(AsmInstruction::Jmp(format!(".L.end.{}", count)));
                    res.push(AsmInstruction::Label(format!(".L.false.{}", count)));
                    res.push(AsmInstruction::Mov(
                        Address::Immediate(0.0),
                        Address::Reg(Reg::Rax),
                    ));
                    res.push(AsmInstruction::Label(format!(".L.end.{}", count)));
                    (res, r_type)
                }
                Token {
                    r#type: TokenType::Or,
                    ..
                } => {
                    let count = self.get_count();
                    let (mut res, r_type) = self.expr(left);
                    res.push(AsmInstruction::Cmp(Address::Immediate(0.0), Reg::Rax));
                    res.push(AsmInstruction::Jne(format!(".L.true.{}", count)));
                    res.extend(self.expr(right).0);
                    res.push(AsmInstruction::Cmp(Address::Immediate(0.0), Reg::Rax));
                    res.push(AsmInstruction::Jne(format!(".L.true.{}", count)));
                    res.push(AsmInstruction::Mov(
                        Address::Immediate(0.0),
                        Address::Reg(Reg::Rax),
                    ));
                    res.push(AsmInstruction::Jmp(format!(".L.end.{}", count)));
                    res.push(AsmInstruction::Label(format!(".L.true.{}", count)));
                    res.push(AsmInstruction::Mov(
                        Address::Immediate(1.0),
                        Address::Reg(Reg::Rax),
                    ));
                    res.push(AsmInstruction::Label(format!(".L.end.{}", count)));
                    (res, r_type)
                }
                _ => unreachable!(),
            },
            Expr::Var { name } => {
                let (res, r_type) = self
                    .vars
                    .get(name)
                    .unwrap_or_else(|| panic!("could not find var {name}"));
                match res {
                    OffsetOrLabel::Offset(offset) => (
                        vec![AsmInstruction::Mov(
                            Address::IndirectOffset(*offset, Reg::Rbp),
                            Address::Reg(Reg::Rax),
                        )],
                        *r_type,
                    ),
                    OffsetOrLabel::Label(_) => (vec![], *r_type),
                }
            }
            Expr::Assign { name, expr } => {
                let mut res = self.add_offset(name);
                res.push(self.push());
                let (extend, r_type) = self.expr(expr);
                res.extend(extend);
                res.push(self.pop(Reg::Rdi));
                res.push(AsmInstruction::Mov(
                    Address::Reg(Reg::Rax),
                    Address::Indirect(Reg::Rdi),
                ));
                (res, r_type)
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                let mut res = vec![];
                for arg in arguments {
                    res.extend(self.expr(arg).0);
                    res.push(self.push());
                }
                for i in (0..arguments.len()).rev() {
                    if i < 6 {
                        res.push(self.pop(ARG_REGS[i].clone()));
                    }
                }
                if let Expr::Var { name } = callee.borrow() {
                    res.push(AsmInstruction::Call(name.to_string()));
                    let r_type = self.functions.get(name).unwrap();
                    return (res, *r_type);
                }
                todo!()
            }
        }
    }

    fn add_offset(&mut self, name: &Token) -> Vec<AsmInstruction> {
        let offset = self.vars.get(name);

        if let Some((offset, _)) = offset {
            match offset {
                OffsetOrLabel::Offset(offset) => {
                    vec![AsmInstruction::Mov(
                        Address::IndirectOffset(*offset, Reg::Rbp),
                        Address::Reg(Reg::Rax),
                    )]
                }
                OffsetOrLabel::Label(label) => {
                    vec![AsmInstruction::Lea(
                        Address::LabelOffset(label.to_string(), Reg::Rip),
                        Address::Reg(Reg::Rax),
                    )]
                }
            }
        } else {
            vec![]
        }
    }

    fn get_anon_count(&mut self) -> u64 {
        self.anon_count += 1;
        self.anon_count - 1
    }

    fn get_count(&mut self) -> u64 {
        self.label_count += 1;
        self.label_count - 1
    }

    fn bin_op_fetch(&mut self, left: &Expr, right: &Expr) -> Vec<AsmInstruction> {
        let mut res = self.expr(right).0;
        res.push(self.push());
        res.extend(self.expr(left).0);
        res.push(self.pop(Reg::Rdi));
        res
    }
}
