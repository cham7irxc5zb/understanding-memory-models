use crate::concmap::*;
use crate::memory::*;
use crate::program::Program;
use crate::thread;

use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};

pub struct Cache {
    r: ConcurrentMap<Box<[u32]>, Future<bool>>,
    n_hits: AtomicU64,
    n_consistent: AtomicUsize,
}

impl Cache {
    #[inline]
    pub fn new() -> Self {
        Self {
            r: ConcurrentMap::new(),
            n_hits: AtomicU64::new(0),
            n_consistent: AtomicUsize::new(0),
        }
    }

    pub fn check_full(&self, p: &Program, mem: &Memory, ts: &thread::State) -> bool {
        assert!(ts.updating.is_none());
        self._check(p, mem, ts, &thread::ConsistencyState::most_restrictive())
    }

    pub fn check_semi(&self, p: &Program, mem: &Memory, ts: &thread::State) -> bool {
        assert!(ts.updating.is_none());
        self._check(p, mem, ts, &thread::ConsistencyState::semi())
    }

    fn _check(
        &self,
        p: &Program,
        mem: &Memory,
        ts: &thread::State,
        cs: &thread::ConsistencyState,
    ) -> bool {
        if ts.no_promises() {
            return true;
        }

        if p.opt().cc {
            let info = p.promise_stat(ts.pc);
            for loc in Loc::iter() {
                let vm = &mem[loc][ts.cur.get(loc)..];
                let loc_info = &info.may_promise[loc];
                for pr in ts.pr[loc].iter() {
                    let msg = &vm[pr];
                    if loc_info.values.get(&msg.val()).is_none() {
                        return false;
                    }
                }
            }
        }

        let mut ser = Vec::new();
        ts.serialize_for_consistency(&mut ser, p, mem, cs);
        let ser = ser.into_boxed_slice();
        let (ins, index) = self.r.insert(ser, Future::Pending);
        if !ins {
            self.n_hits.fetch_add(1, Ordering::Relaxed);
            return self.r.poll(index, |_, v| *v);
        }

        let mut vis = Vis {
            cache: self,
            p,
            mem,
            cs,
            ts,
        };
        let r = thread::visit_transitions(&mut vis, p, mem, ts).is_err();
        self.r.with(index, |_, v| {
            v.set(r);
        });
        if r {
            self.n_consistent.fetch_add(1, Ordering::Relaxed);
        }
        r
    }

    #[inline(always)]
    pub fn n_hits(&self) -> u64 {
        self.n_hits.load(Ordering::Relaxed)
    }

    #[inline(always)]
    pub fn n_misses(&self) -> u64 {
        self.r.len() as u64
    }

    #[inline(always)]
    pub fn n_consistent(&self) -> u64 {
        self.n_consistent.load(Ordering::Relaxed) as u64
    }

    #[inline(always)]
    pub fn n_inconsistent(&self) -> u64 {
        self.n_misses().saturating_sub(self.n_consistent())
    }

    #[inline(always)]
    pub fn n_total(&self) -> u64 {
        self.n_hits() + self.n_misses()
    }
}

struct Vis<'a> {
    cache: &'a Cache,
    p: &'a Program,
    mem: &'a Memory,
    cs: &'a thread::ConsistencyState,
    ts: &'a thread::State,
}

impl thread::TransitionVisitor for Vis<'_> {
    // `Ok` means inconsistent, `Err` means consistent.
    type Err = ();

    fn ret(&mut self, _: Val) -> Result<(), ()> {
        unreachable!()
    }

    fn pure(&mut self, s: thread::Pure) -> Result<(), Self::Err> {
        let ts2 = self.ts.with_pure(&s);
        if self.cache._check(self.p, self.mem, &ts2, self.cs) {
            Err(())
        } else {
            Ok(())
        }
    }

    fn write(&mut self, w: thread::Write) -> Result<(), ()> {
        let mut cs2 = self.cs.clone();
        if !self.ts.valid_in_future(self.mem, &w, &mut cs2) {
            return Ok(());
        }

        let (ts2, mem2, _) = self.ts.with_write(self.mem, &w);

        if self.cache._check(self.p, &mem2, &ts2, &cs2) {
            Err(())
        } else {
            Ok(())
        }
    }

    fn sc_fence(&mut self, _: thread::ScFence) -> Result<(), ()> {
        unreachable!()
    }

    fn rel_fence(&mut self, _: thread::RelFence) -> Result<(), ()> {
        unreachable!()
    }

    fn rel_write(&mut self, _: thread::RelWrite) -> Result<(), ()> {
        unreachable!()
    }
}
