use super::{Fence, Op, Pctr, RawProgram, ValueDecoder};
use crate::index::*;
use crate::memory::{Loc, LocArray, Val};
use std::hash::Hash;

pub enum Step<P: State> {
    Return {
        val: P::Value,
    },
    Ignore,
    Fence {
        kind: Fence,
        next: P,
    },
    Read {
        loc: Loc,
        is_acquire: bool,
        is_update: bool,
        next: P::Read,
    },
    Write {
        loc: Loc,
        val: P::Value,
        is_release: bool,
        is_update: bool,
        next: P,
    },
    CancelUpdate {
        loc: Loc,
        next: P,
    },
}

pub trait State: Sized + Clone + Eq + Hash {
    type Value: Clone + Eq + Ord + Hash;
    type Read: ReadStep<Self>;

    fn next(&self) -> Step<Self>;
}

pub trait ReadStep<P: State> {
    fn reads(&self, v: &P::Value) -> P;
}

pub struct Compiler<P: State> {
    states: ISet<Pctr, P>,
    ops: Vec<Step<P>>,
}

impl<P: State> Compiler<P> {
    pub fn new() -> Self {
        Self {
            states: ISet::new(),
            ops: Vec::new(),
        }
    }

    pub fn state(&mut self, q: P) -> Pctr {
        match self.states.entry(&q) {
            ISetEntry::Occupied(e) => e.index(),
            ISetEntry::Vacant(e) => {
                let step = q.next();
                self.ops.push(step);
                e.insert(q).index()
            }
        }
    }

    fn new_value(&mut self, opn: usize, loc: Loc, vals: &mut ISet<Val, P::Value>, val: P::Value) {
        let mut vali = vals.len();
        if vals.check_insert(val).is_err() {
            return;
        }
        let ops: Vec<&P::Read> = self.ops[0..opn]
            .iter()
            .filter_map(|op| {
                if let Step::Read {
                    loc: loc2, next, ..
                } = op
                {
                    if *loc2 != loc {
                        return None;
                    }
                    Some(next)
                } else {
                    None
                }
            })
            .collect();
        let mut states = Vec::new();
        while vali < vals.len() {
            let i = vali.grow();
            let val = vals.at(i).clone();
            for next in ops.iter() {
                states.push(next.reads(&val));
            }
        }
        for st in states {
            self.state(st);
        }
    }

    pub fn compile(
        mut self,
        locs: Array<Loc, (String, P::Value)>,
    ) -> (RawProgram, ValueDecoder<P::Value>) {
        let mut ret_vals = ISet::<Val, P::Value>::new();
        let mut loc_vals = locs.map(|_, (_, v)| {
            let mut r = ISet::<Val, P::Value>::new();
            let ins = r.insert(v.clone());
            assert!(ins == Val::ZERO);
            r
        });
        let loc_names = locs.map_into(|_, (n, _)| n);

        let mut opi = 0;
        while opi < self.ops.len() {
            match &self.ops[opi] {
                Step::Return { .. } => {}
                Step::Ignore => {}
                Step::Fence { next, .. } | Step::CancelUpdate { next, .. } => {
                    let next = P::clone(next);
                    self.state(next);
                }
                Step::Read { loc, next, .. } => {
                    let loc = *loc;
                    for next in loc_vals[loc].values().map(|_, val| next.reads(val)) {
                        self.state(next);
                    }
                }
                Step::Write { loc, val, next, .. } => {
                    let loc = *loc;
                    let val = val.clone();
                    let next = P::clone(next);
                    let _ = self.state(next);
                    self.new_value(opi, loc, &mut loc_vals[loc], val);
                }
            }
            opi += 1;
        }

        let Self { ops, states } = self;

        let code: Vec<_> = ops
            .into_iter()
            .map(|op| match op {
                Step::Return { val } => Op::Return {
                    val: ret_vals.insert(val),
                },
                Step::Ignore => Op::Ignore,
                Step::Fence { kind, next } => Op::Fence {
                    kind,
                    next: states.get(&next).unwrap(),
                },
                Step::Read {
                    loc,
                    is_acquire,
                    is_update,
                    next,
                } => {
                    let vals = loc_vals[loc].values();
                    Op::Read {
                        loc,
                        is_acquire,
                        is_update,
                        next: vals.map(|_, v| states.get(&next.reads(v)).unwrap()),
                    }
                }
                Step::Write {
                    loc,
                    is_update,
                    is_release,
                    val,
                    next,
                } => {
                    let next = states.get(&next).unwrap();
                    let val = loc_vals[loc].get(&val).unwrap();
                    Op::Write {
                        loc,
                        is_update,
                        is_release,
                        val,
                        next,
                    }
                }
                Step::CancelUpdate { loc, next } => {
                    let next = states.get(&next).unwrap();
                    Op::CancelUpdate { loc, next }
                }
            })
            .collect();

        let prog = RawProgram {
            code: Array::new_vec(code),
            loc_range: LocArray::new(|i| {
                if loc_vals.len() > i {
                    loc_vals[i].len()
                } else {
                    Length::ZERO
                }
            }),
        };

        let dec = ValueDecoder {
            ret: ret_vals.into_values(),
            loc_names,
            loc: loc_vals.map_into(|_, v| v.into_values()),
        };
        (prog, dec)
    }
}
