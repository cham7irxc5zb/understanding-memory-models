use crate::index::*;
use crate::memory::{Loc, LocArray, Val};
use std::fmt;

define_index_type! {
    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Pctr(u32);
}

define_index_type! {
    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct InnerEquiv(Pctr);
}

impl fmt::Debug for Pctr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "@{}", self.0)
    }
}

pub struct OptFlags {
    pub compress: bool,
    pub equiv: bool,
    pub local: bool,
    pub promise: bool,
    pub cc: bool,
}

impl Default for OptFlags {
    #[inline]
    fn default() -> Self {
        Self {
            compress: true,
            equiv: true,
            local: true,
            promise: true,
            cc: true,
        }
    }
}

type Code = Array<Pctr, Op>;

fn compress(c: &Slice<Pctr, Op>) -> (Code, Array<Pctr, Pctr>) {
    let mut map = Array::new(c.len(), |_| Pctr(0));
    let mut old_states = Length::<Pctr>::new(1);
    loop {
        let mut states = ISet::<Pctr, Op>::new();
        let remap = c.map(|_, op| {
            let mut op = op.clone();
            match &mut op {
                Op::Return { .. } => {}
                Op::Ignore => {}
                Op::Fence { next, .. } => {
                    *next = map[*next];
                }
                Op::Read { next, .. } => {
                    for next in next.iter_mut() {
                        *next = map[*next];
                    }
                }
                Op::Write { next, .. } => {
                    *next = map[*next];
                }
                Op::CancelUpdate { next, .. } => {
                    *next = map[*next];
                }
            };
            states.insert(op)
        });
        if states.len() == old_states {
            assert!(map == remap);
            return (states.into_values(), remap);
        }
        assert!(states.len() > old_states);
        map = remap;
        old_states = states.len();
    }
}

fn code_graph(c: &Slice<Pctr, Op>) -> Array<Pctr, ISet<usize, Pctr>> {
    c.map(|_, op| match op {
        Op::Return { .. } | Op::Ignore => ISet::new(),
        Op::Fence { next, .. } | Op::Write { next, .. } | Op::CancelUpdate { next, .. } => {
            ISet::singleton(*next)
        }
        Op::Read { next, .. } => next.iter().copied().collect(),
    })
}

fn reachable<I: Index>(c: &Slice<I, ISet<usize, I>>) -> Array<I, ISet<usize, I>> {
    Array::new(c.len(), |i| {
        let mut r = ISet::singleton(i);
        let mut ji = 0;
        while let Some(j) = r.values().get(ji).copied() {
            ji += 1;
            for v in c[j].values().iter().copied() {
                r.insert(v);
            }
        }
        r
    })
}

pub struct ValueDecoder<V> {
    pub ret: Array<Val, V>,
    pub loc_names: Array<Loc, String>,
    pub loc: Array<Loc, Array<Val, V>>,
}

pub struct RawProgram {
    code: Code,
    loc_range: LocArray<Length<Val>>,
}

pub struct Program {
    code: Code,
    loc_range: LocArray<Length<Val>>,
    inner_equiv: Array<Pctr, InnerEquiv>,
    promise_stat: Array<InnerEquiv, PromiseStat>,
    opt: OptFlags,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Fence {
    Sc,
    Acq,
    Rel,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Op {
    Return {
        val: Val,
    },
    Ignore,
    Fence {
        kind: Fence,
        next: Pctr,
    },
    Read {
        loc: Loc,
        is_acquire: bool,
        is_update: bool,
        next: Array<Val, Pctr>,
    },
    Write {
        loc: Loc,
        is_release: bool,
        is_update: bool,
        val: Val,
        next: Pctr,
    },
    CancelUpdate {
        loc: Loc,
        next: Pctr,
    },
}

pub struct PromiseStat {
    pub may_promise: LocArray<LocPromiseStat>,
}

pub struct LocPromiseStat {
    pub values: ISet<usize, Val>,
    pub write: bool,
    pub update: bool,
}

impl LocPromiseStat {
    #[inline]
    fn new() -> Self {
        Self {
            values: ISet::new(),
            write: false,
            update: false,
        }
    }

    #[inline(always)]
    pub fn iter_values<'a>(&'a self) -> impl 'a + Iterator<Item = Val> {
        self.values.values().iter().copied()
    }
}

impl Program {
    #[inline(always)]
    pub fn loc_range(&self, loc: Loc) -> Length<Val> {
        self.loc_range[loc]
    }

    #[inline]
    pub fn op(&self, p: Pctr) -> &Op {
        &self.code[p]
    }

    #[inline]
    pub fn inner_equiv(&self, p: Pctr) -> InnerEquiv {
        self.inner_equiv[p]
    }

    #[inline]
    pub fn promise_stat(&self, p: Pctr) -> &PromiseStat {
        &self.promise_stat[self.inner_equiv[p]]
    }

    #[inline]
    pub fn n_states(&self) -> u64 {
        self.code.len().as_usize() as u64
    }

    #[inline]
    pub fn n_inner_equiv(&self) -> u64 {
        self.promise_stat.len().as_usize() as u64
    }

    #[inline(always)]
    pub fn opt(&self) -> &OptFlags {
        &self.opt
    }
}

impl RawProgram {
    pub fn optimize(mut self, opt: OptFlags, start: &mut [Pctr]) -> Program {
        if opt.compress {
            let (c2, remap) = compress(&self.code);
            self.code = c2;
            for p in start.iter_mut() {
                *p = remap[*p];
            }
        }

        let (inner_code, inner_equiv) = if opt.equiv {
            compress(&self.code.map(|_, op| match op {
                Op::Return { .. }
                | Op::Ignore
                | Op::Fence {
                    kind: Fence::Sc, ..
                }
                | Op::Fence {
                    kind: Fence::Rel, ..
                }
                | Op::Write {
                    is_release: true, ..
                } => Op::Ignore,
                _ => op.clone(),
            }))
        } else {
            (self.code.clone(), Array::new(self.code.len(), |i| i))
        };

        let inner_graph = reachable(&code_graph(&inner_code));

        let promise_stat = inner_graph.map(|_, js| {
            let mut r = PromiseStat {
                may_promise: LocArray::new(|_| LocPromiseStat::new()),
            };
            for j in js.values().iter().copied() {
                if let Op::Write {
                    loc,
                    val,
                    is_update,
                    ..
                } = inner_code[j]
                {
                    let rl = &mut r.may_promise[loc];
                    rl.values.insert(val);
                    if is_update {
                        rl.update = true;
                    } else {
                        rl.write = true;
                    }
                }
            }
            r
        });

        Program {
            code: self.code,
            loc_range: self.loc_range,
            inner_equiv: inner_equiv.map_into(|_, v| InnerEquiv(v)),
            promise_stat: Array::new_box(promise_stat.into_boxed_slice()),
            opt,
        }
    }
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut d = f.debug_map();
        for (i, op) in self.code.enum_iter() {
            d.entry(&i, op);
        }
        d.finish()
    }
}

pub mod compiler;
