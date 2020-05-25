use super::{Inst, Proc, Reg, Value};
use crate::index::*;
use crate::memory::Loc;
use crate::program::{compiler, Fence};
use std::{hash, ptr};

#[derive(Clone)]
struct InState<'a> {
    prog: &'a Proc,
    inst: usize,
    updating: Option<Loc>,
    regs: Array<Reg, Value>,
    stack: Vec<Value>,
}

impl<'a> PartialEq for InState<'a> {
    fn eq(&self, other: &InState<'a>) -> bool {
        ptr::eq(self.prog, other.prog)
            && self.inst == other.inst
            && self.regs == other.regs
            && self.stack == other.stack
    }
}

impl<'a> Eq for InState<'a> {}

impl<'a> hash::Hash for InState<'a> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (self.prog as *const Proc).hash(state);
        self.inst.hash(state);
        self.regs.hash(state);
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct State<'a> {
    op: MemOp,
    then: InState<'a>,
}

impl Proc {
    pub fn start(&self, init: &Array<Reg, Value>) -> State {
        assert!(init.len() == self.args);
        InState {
            prog: self,
            inst: 0,
            updating: None,
            regs: Array::new(self.regs, |i| {
                if init.len() > i {
                    init[i].clone()
                } else {
                    Value(0)
                }
            }),
            stack: Vec::new(),
        }
        .stable()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ReadState<'a> {
    then: InState<'a>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct UpdateState<'a> {
    then: InState<'a>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum MemOp {
    Return(Value),
    Ignore,
    Fence(Fence),
    Read(Loc, bool),
    Write(Loc, bool, Value),
    Update(Loc, bool),
    CommitUpdate(Loc, bool, Value),
    CancelUpdate(Loc),
}

impl<'a> InState<'a> {
    fn push(&mut self, v: Value) {
        self.stack.push(v)
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("empty stack")
    }

    fn stable(mut self) -> State<'a> {
        loop {
            let inst = self.prog.code[self.inst];
            self.inst += 1;
            match inst {
                Inst::Pop => {
                    self.pop();
                }
                Inst::Ignore => {
                    return State {
                        op: MemOp::Ignore,
                        then: self,
                    };
                }
                Inst::ConstInt(v) => {
                    self.push(Value(v));
                }
                Inst::Return => {
                    assert!(self.updating.is_none());
                    let val = self.pop();
                    return State {
                        op: MemOp::Return(val),
                        then: self,
                    };
                }
                Inst::Fence(kind) => {
                    assert!(self.updating.is_none());
                    return State {
                        op: MemOp::Fence(kind),
                        then: self,
                    };
                }
                Inst::Read(loc, is_acquire) => {
                    assert!(self.updating.is_none());
                    return State {
                        op: MemOp::Read(loc, is_acquire),
                        then: self,
                    };
                }
                Inst::Write(loc, is_release) => {
                    assert!(self.updating.is_none());
                    let val = self.pop();
                    return State {
                        op: MemOp::Write(loc, is_release, val),
                        then: self,
                    };
                }
                Inst::Update(loc, is_acquire) => {
                    assert!(self.updating.is_none());
                    self.updating = Some(loc);
                    return State {
                        op: MemOp::Update(loc, is_acquire),
                        then: self,
                    };
                }
                Inst::CommitUpdate(is_release) => {
                    let loc = self.updating.take().unwrap();
                    let val = self.pop();
                    return State {
                        op: MemOp::CommitUpdate(loc, is_release, val),
                        then: self,
                    };
                }
                Inst::CancelUpdate => {
                    let loc = self.updating.take().unwrap();
                    return State {
                        op: MemOp::CancelUpdate(loc),
                        then: self,
                    };
                }
                Inst::Set(reg) => {
                    self.regs[reg] = self.pop();
                }
                Inst::Get(reg) => {
                    self.push(self.regs[reg].clone());
                }
                Inst::Monop(op) => {
                    let val = self.pop();
                    self.push(Value::monop(op, val));
                }
                Inst::Binop(op) => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Value::binop(op, lhs, rhs));
                }
                Inst::Jump(inst) => {
                    self.inst = inst;
                }
                Inst::JumpIfZero(inst) => {
                    let val = self.pop();
                    if val.0 == 0 {
                        self.inst = inst;
                    }
                }
            }
        }
    }
}

impl<'a> compiler::State for State<'a> {
    type Value = Value;
    type Read = ReadState<'a>;

    fn next(&self) -> compiler::Step<Self> {
        match &self.op {
            MemOp::Return(val) => compiler::Step::Return { val: val.clone() },
            MemOp::Ignore => compiler::Step::Ignore,
            MemOp::Fence(kind) => {
                let kind = *kind;
                compiler::Step::Fence {
                    kind,
                    next: self.then.clone().stable(),
                }
            }
            MemOp::Read(loc, is_acquire) => {
                let loc = *loc;
                let is_acquire = *is_acquire;
                compiler::Step::Read {
                    loc,
                    is_acquire,
                    is_update: false,
                    next: ReadState {
                        then: self.then.clone(),
                    },
                }
            }
            MemOp::Write(loc, is_release, val) => {
                let loc = *loc;
                let is_release = *is_release;
                compiler::Step::Write {
                    loc,
                    val: val.clone(),
                    is_release,
                    is_update: false,
                    next: self.then.clone().stable(),
                }
            }
            MemOp::Update(loc, is_acquire) => {
                let loc = *loc;
                let is_acquire = *is_acquire;
                compiler::Step::Read {
                    loc,
                    is_acquire,
                    is_update: true,
                    next: ReadState {
                        then: self.then.clone(),
                    },
                }
            }
            MemOp::CommitUpdate(loc, is_release, val) => {
                let loc = *loc;
                let is_release = *is_release;
                compiler::Step::Write {
                    loc,
                    val: val.clone(),
                    is_release,
                    is_update: true,
                    next: self.then.clone().stable(),
                }
            }
            MemOp::CancelUpdate(loc) => {
                let loc = *loc;
                compiler::Step::CancelUpdate {
                    loc,
                    next: self.then.clone().stable(),
                }
            }
        }
    }
}

impl<'a> compiler::ReadStep<State<'a>> for ReadState<'a> {
    fn reads(&self, v: &Value) -> State<'a> {
        let mut st = self.then.clone();
        st.stack.push(v.clone());
        st.stable()
    }
}
