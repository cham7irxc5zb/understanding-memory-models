use crate::index::*;
use crate::memory::Loc;
use crate::program;
use std::{error, fmt, io};

define_index_type! {
    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Reg(usize);
}

impl fmt::Debug for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}

#[derive(Debug)]
pub struct Program {
    locs: Array<Loc, (String, Value)>,
    procs: Vec<Proc>,
    run: Vec<(usize, Array<Reg, Value>)>,
}

impl Program {
    pub fn compile(
        &self,
        opt: program::OptFlags,
    ) -> (
        program::Program,
        program::ValueDecoder<Value>,
        Vec<program::Pctr>,
    ) {
        let mut comp = program::compiler::Compiler::new();
        let mut r = Vec::new();
        for (i, init) in &self.run {
            r.push(comp.state(self.procs[*i].start(init)));
        }
        let (rawprog, dec) = comp.compile(self.locs.clone());
        let prog = rawprog.optimize(opt, &mut r);
        (prog, dec, r)
    }
}

#[derive(Debug)]
struct Proc {
    args: Length<Reg>,
    regs: Length<Reg>,
    code: Vec<Inst>,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Value(i64);

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <i64 as fmt::Display>::fmt(&self.0, f)
    }
}

impl From<i64> for Value {
    #[inline]
    fn from(v: i64) -> Self {
        Self(v)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Span(pub usize, pub usize);

impl Span {
    #[inline]
    pub fn union(vals: &[Self]) -> Self {
        Self(
            vals.iter().map(|a| a.0).min().unwrap(),
            vals.iter().map(|a| a.1).max().unwrap(),
        )
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct SyntaxError {
    pub span: Span,
    pub msg: &'static str,
}

impl error::Error for SyntaxError {}

impl fmt::Display for SyntaxError {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(
            fmt,
            "parse error at {}-{}: {}",
            self.span.0, self.span.1, self.msg
        )
    }
}

impl From<SyntaxError> for io::Error {
    #[inline]
    fn from(v: SyntaxError) -> Self {
        Self::new(io::ErrorKind::InvalidData, v)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
}

impl Op {
    fn is_monop(self) -> bool {
        match self {
            Op::LogicalNot => true,
            Op::Sub => true,
            _ => false,
        }
    }

    fn prec(self) -> Option<u32> {
        Some(match self {
            Self::LogicalOr => 0,
            Self::LogicalAnd => 1,
            Self::Eq | Self::Ne | Self::Lt | Self::Le | Self::Gt | Self::Ge => 2,
            Self::Add | Self::Sub => 3,
            Self::Mul | Self::Div | Self::Mod => 4,
            Self::LogicalNot => return None,
        })
    }
}

impl Value {
    fn binop(op: Op, v1: Self, v2: Self) -> Self {
        match op {
            Op::Add => Value(v1.0.checked_add(v2.0).expect("overflow in +")),
            Op::Sub => Value(v1.0.checked_sub(v2.0).expect("overflow in -")),
            Op::Mul => Value(v1.0.checked_mul(v2.0).expect("overflow in *")),
            Op::Div => Value(v1.0.checked_div(v2.0).expect("overflow in /")),
            Op::Mod => Value(v1.0.checked_rem(v2.0).expect("overflow in %")),
            Op::Eq => Value((v1.0 == v2.0) as i64),
            Op::Ne => Value((v1.0 != v2.0) as i64),
            Op::Lt => Value((v1.0 < v2.0) as i64),
            Op::Le => Value((v1.0 <= v2.0) as i64),
            Op::Gt => Value((v1.0 > v2.0) as i64),
            Op::Ge => Value((v1.0 >= v2.0) as i64),
            _ => unreachable!(),
        }
    }

    fn monop(op: Op, v1: Self) -> Self {
        match op {
            Op::Sub => Value(v1.0.checked_neg().expect("overflow in -")),
            Op::LogicalNot => Value((v1.0 == 0) as i64),
            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Inst {
    Return,
    Ignore,
    Pop,
    ConstInt(i64),
    Get(Reg),
    Set(Reg),
    Fence(program::Fence),
    Monop(Op),
    Binop(Op),
    Jump(usize),
    JumpIfZero(usize),
    Read(Loc, bool),
    Write(Loc, bool),
    Update(Loc, bool),
    CommitUpdate(bool),
    CancelUpdate,
}

mod ast;
mod lex;
mod parse;
pub use parse::parse;

pub mod eval;
