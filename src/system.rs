use crate::concmap::*;
use crate::index::*;
use crate::memory::*;
use crate::program::*;
use crate::{checkpoint, consistency, promise, thread};
use std::convert::Infallible;

define_index_type! {
    #[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
    pub struct Tid(pub usize);
}

define_index_type! {
    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct MemoryId(usize);
}

define_index_type! {
    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ThreadStateId(u32);
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct State {
    mem: MemoryId,
    sc: View,
    tss: Array<Tid, ThreadStateId>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct CompactThread {
    pc: Pctr,
    rel: LocArray<View>,
    cur: View,
    acq: View,
    pr: thread::PromiseSet,
}

struct Dedup {
    mems: ConcurrentMap<Memory, ()>,
    thrs: ConcurrentMap<CompactThread, ()>,
}

impl Dedup {
    #[inline]
    fn new() -> Self {
        Self {
            mems: ConcurrentMap::new(),
            thrs: ConcurrentMap::new(),
        }
    }

    fn report(&self) {
        eprintln!("    distinct memories: {}", self.mems.len());
        eprintln!("    distinct thread states: {}", self.thrs.len());
    }

    #[inline]
    fn new_state(&self, start: &[Pctr]) -> State {
        let mem = self.compact_mem(Memory::new());
        State {
            mem,
            sc: View::ZERO,
            tss: Slice::at(start).map(|_, pc| {
                self.compact_thread(thread::State {
                    pc: *pc,
                    rel: LocArray::new(|_| View::ZERO),
                    cur: View::ZERO,
                    acq: View::ZERO,
                    pr: thread::PromiseSet::new(),
                    updating: None,
                })
            }),
        }
    }

    fn expand_thread(&self, id: ThreadStateId) -> thread::State {
        let id = id.0 as usize;
        self.thrs.with(id, |ts, ()| thread::State {
            pc: ts.pc,
            updating: None,
            rel: ts.rel.clone(),
            cur: ts.cur.clone(),
            acq: ts.acq.clone(),
            pr: ts.pr.clone(),
        })
    }

    fn compact_thread(&self, ts: thread::State) -> ThreadStateId {
        assert!(ts.updating.is_none());
        let id: usize = self
            .thrs
            .insert(
                CompactThread {
                    pc: ts.pc,
                    rel: ts.rel.clone(),
                    cur: ts.cur,
                    acq: ts.acq,
                    pr: ts.pr,
                },
                (),
            )
            .1;
        ThreadStateId(id as u32)
    }

    fn thread_shift(&self, ts: ThreadStateId, s: Shift) -> ThreadStateId {
        self.compact_thread(self.expand_thread(ts).with_shift(s))
    }

    fn expand_mem(&self, m: MemoryId) -> Memory {
        self.mems.with(m.0, |m, ()| m.clone())
    }

    fn compact_mem(&self, m: Memory) -> MemoryId {
        MemoryId(self.mems.insert(m, ()).1)
    }
}

pub struct System {
    program: Program,
    cc: consistency::Cache,
    hc: checkpoint::Cache,
    pc: promise::Cache,
    dedup: Dedup,
}

impl System {
    #[inline]
    pub fn new(program: Program) -> Self {
        Self {
            program,
            cc: consistency::Cache::new(),
            hc: checkpoint::Cache::new(),
            pc: promise::Cache::new(),
            dedup: Dedup::new(),
        }
    }

    pub fn report(&self) {
        eprintln!(
            "    consistency::Cache {} + {} / {}",
            self.cc.n_consistent(),
            self.cc.n_inconsistent(),
            self.cc.n_total()
        );
        eprintln!(
            "    checkpoint::Cache {} / {}",
            self.hc.n_misses(),
            self.hc.n_total()
        );
        eprintln!(
            "    promise::Cache {} / {}",
            self.pc.n_misses(),
            self.pc.n_total()
        );
        self.dedup.report();
    }

    #[inline(always)]
    pub fn program(&self) -> &Program {
        &self.program
    }

    pub fn new_state(&self, start: &[Pctr]) -> State {
        self.dedup.new_state(start)
    }
}

trait CheckpointVisitor {
    type Error;

    fn rets(&mut self, ti: Tid, vals: &[Val]) -> Result<(), Self::Error>;
    fn promise(
        &mut self,
        st2: State,
        ti: Tid,
        ts: &thread::State,
        pr: &thread::Promise,
        shift: Shift,
    ) -> Result<(), Self::Error>;
    fn sc(&mut self, st2: State, ti: Tid) -> Result<(), Self::Error>;
    fn rel(&mut self, st2: State, ti: Tid) -> Result<(), Self::Error>;
    fn rel_write(
        &mut self,
        st2: State,
        ti: Tid,
        ts: &thread::State,
        w: &thread::RelWrite,
        shift: Shift,
    ) -> Result<(), Self::Error>;
}

impl System {
    fn visit_thread<V: CheckpointVisitor>(
        &self,
        vis: &mut V,
        st: &State,
        mem: &Memory,
        ti: Tid,
        ts: &thread::State,
        full: bool,
    ) -> Result<(), V::Error> {
        let q_promise = self.pc.get(&self.program, &self.cc, &mem, &ts);

        for pr in q_promise {
            let (ts2, mem2, shift) = ts.with_promise(&mem, pr);
            debug_assert!(self.cc.check_semi(&self.program, &mem2, &ts2));
            let dedup = &self.dedup;
            let ts2_id = dedup.compact_thread(ts2);
            let mem2_id = dedup.compact_mem(mem2);
            vis.promise(
                State {
                    mem: mem2_id,
                    sc: st.sc.with_shift(shift),
                    tss: st.tss.map_update(
                        ti,
                        |_| ts2_id,
                        |_, ots| dedup.thread_shift(*ots, shift),
                    ),
                },
                ti,
                &ts,
                &pr,
                shift,
            )?;
        }

        if !full {
            return Ok(());
        }

        let q = self.hc.get(&self.program, &self.cc, &mem, &ts);
        vis.rets(ti, q.ret)?;

        for f in q.sc_fence {
            let (ts2, sc2) = ts.with_sc_fence(&mem, st.sc, f.clone());
            debug_assert!(self.cc.check_full(&self.program, &mem, &ts2));
            let ts2_id = self.dedup.compact_thread(ts2);
            vis.sc(
                State {
                    mem: st.mem,
                    sc: sc2,
                    tss: st.tss.map_update(ti, |_| ts2_id, |_, ots| *ots),
                },
                ti,
            )?;
        }

        for f in q.rel_fence {
            let ts2 = ts.with_rel_fence(&mem, f.clone());
            debug_assert!(self.cc.check_full(&self.program, &mem, &ts2));
            let ts2_id = self.dedup.compact_thread(ts2);
            vis.rel(
                State {
                    mem: st.mem,
                    sc: st.sc,
                    tss: st.tss.map_update(ti, |_| ts2_id, |_, ots| *ots),
                },
                ti,
            )?;
        }

        for w in q.rel_write {
            let (ts2, mem2, shift) = ts.with_rel_write(&mem, w);
            debug_assert!(self.cc.check_full(&self.program, &mem2, &ts2));
            let dedup = &self.dedup;
            let ts2_id = dedup.compact_thread(ts2);
            let mem2_id = dedup.compact_mem(mem2);
            vis.rel_write(
                State {
                    mem: mem2_id,
                    sc: st.sc.with_shift(shift),
                    tss: st.tss.map_update(
                        ti,
                        |_| ts2_id,
                        |_, ots| dedup.thread_shift(*ots, shift),
                    ),
                },
                ti,
                &ts,
                &w,
                shift,
            )?;
        }

        Ok(())
    }

    fn visit_checkpoints<V: CheckpointVisitor>(
        &self,
        vis: &mut V,
        st: &State,
    ) -> Result<(), V::Error> {
        let mem = self.dedup.expand_mem(st.mem);

        let tss = st.tss.map(|_, ts| self.dedup.expand_thread(*ts));

        let mut it = tss
            .enum_iter()
            .filter(|(_, ts)| !self.cc.check_full(&self.program, &mem, &ts));
        if let Some((ti, ts)) = it.next() {
            assert!(it.next().is_none());
            for (_, ts) in tss.enum_iter() {
                debug_assert!(self.cc.check_semi(&self.program, &mem, ts));
            }
            return self.visit_thread(vis, st, &mem, ti, &ts, false);
        }

        for (ti, ts) in tss.enum_iter() {
            self.visit_thread(vis, st, &mem, ti, ts, true)?;
        }
        Ok(())
    }
}

struct NewStateVisitor {
    per_thread_rets: Array<Tid, Box<[Val]>>,
    new_states: Vec<State>,
}

impl CheckpointVisitor for NewStateVisitor {
    type Error = Infallible;

    #[inline(always)]
    fn rets(&mut self, ti: Tid, vals: &[Val]) -> Result<(), Infallible> {
        self.per_thread_rets[ti] = Box::from(vals);
        Ok(())
    }

    #[inline(always)]
    fn promise(
        &mut self,
        st2: State,
        _: Tid,
        _: &thread::State,
        _: &thread::Promise,
        _: Shift,
    ) -> Result<(), Infallible> {
        self.new_states.push(st2);
        Ok(())
    }

    #[inline(always)]
    fn sc(&mut self, st2: State, _: Tid) -> Result<(), Infallible> {
        self.new_states.push(st2);
        Ok(())
    }

    #[inline(always)]
    fn rel(&mut self, st2: State, _: Tid) -> Result<(), Infallible> {
        self.new_states.push(st2);
        Ok(())
    }

    #[inline(always)]
    fn rel_write(
        &mut self,
        st2: State,
        _: Tid,
        _: &thread::State,
        _: &thread::RelWrite,
        _: Shift,
    ) -> Result<(), Infallible> {
        self.new_states.push(st2);
        Ok(())
    }
}

impl System {
    pub fn checkpoint(&self, st: &State) -> (Vec<State>, Vec<Array<Tid, Val>>) {
        let mut v = NewStateVisitor {
            new_states: Vec::new(),
            per_thread_rets: st.tss.map(|_, _| Box::default()),
        };

        match self.visit_checkpoints(&mut v, st) {
            Ok(()) => {}
            Err(e) => match e {},
        }

        // Compute cartesian product of per_thread.
        let mut rets = Vec::new();
        if v.per_thread_rets.iter().all(|r| !r.is_empty()) {
            let mut ctrs = v.per_thread_rets.map(|_, _| 0usize);
            'a: loop {
                rets.push(ctrs.map(|i, ctr| v.per_thread_rets[i][*ctr]));

                let mut at = Length::ZERO;
                loop {
                    let inc = at.grow();
                    ctrs[inc] += 1;
                    if ctrs[inc] < v.per_thread_rets[inc].len() {
                        break;
                    }
                    ctrs[inc] = 0;
                    if at == ctrs.len() {
                        break 'a;
                    }
                }
            }
        }

        (v.new_states, rets)
    }
}

#[derive(Clone, Debug)]
pub struct ExplainStep {
    pub thread: Tid,
    pub action: ExplainAction,
}

#[derive(Clone, Debug)]
pub enum ExplainAction {
    Promise(thread::ExplainPromise),
    Sc,
    Rel,
    RelWrite(thread::ExplainRelWrite),
}

pub struct Explained {
    steps: Vec<ExplainStep>,
    shifts: Vec<Shift>,
}

impl Explained {
    pub fn new() -> Self {
        Self {
            steps: Vec::new(),
            shifts: Vec::new(),
        }
    }

    pub fn done(mut self) -> Vec<ExplainStep> {
        self.steps.reverse();
        self.steps
    }
}

struct ExplainerVisitor<'a> {
    st2: &'a State,
    e: &'a mut Explained,
}

impl CheckpointVisitor for ExplainerVisitor<'_> {
    type Error = ();

    #[inline(always)]
    fn rets(&mut self, _ti: Tid, _vals: &[Val]) -> Result<(), ()> {
        Ok(())
    }

    #[inline(always)]
    fn promise(
        &mut self,
        st2: State,
        ti: Tid,
        ts: &thread::State,
        pr: &thread::Promise,
        shift: Shift,
    ) -> Result<(), ()> {
        if st2 != *self.st2 {
            return Ok(());
        }

        let mut pr = pr.explain(ts);
        for s in self.e.shifts.iter().rev() {
            if pr.loc == s.loc && pr.t >= s.t {
                pr.t = pr.t + TsDiff::ONE;
            }
        }
        self.e.shifts.push(shift);
        self.e.steps.push(ExplainStep {
            thread: ti,
            action: ExplainAction::Promise(pr),
        });
        Err(())
    }

    #[inline(always)]
    fn sc(&mut self, st2: State, ti: Tid) -> Result<(), ()> {
        if st2 != *self.st2 {
            return Ok(());
        }

        self.e.steps.push(ExplainStep {
            thread: ti,
            action: ExplainAction::Sc,
        });

        Err(())
    }

    #[inline(always)]
    fn rel(&mut self, st2: State, ti: Tid) -> Result<(), ()> {
        if st2 != *self.st2 {
            return Ok(());
        }

        self.e.steps.push(ExplainStep {
            thread: ti,
            action: ExplainAction::Rel,
        });

        Err(())
    }

    #[inline(always)]
    fn rel_write(
        &mut self,
        st2: State,
        ti: Tid,
        ts: &thread::State,
        w: &thread::RelWrite,
        shift: Shift,
    ) -> Result<(), ()> {
        if st2 != *self.st2 {
            return Ok(());
        }

        let mut w = w.explain(ts);
        for s in self.e.shifts.iter().rev() {
            if w.loc == s.loc && w.t >= s.t {
                w.t = w.t + TsDiff::ONE;
            }
        }
        self.e.shifts.push(shift);
        self.e.steps.push(ExplainStep {
            thread: ti,
            action: ExplainAction::RelWrite(w),
        });

        Err(())
    }
}

impl System {
    pub fn explain(&self, e: &mut Explained, st: &State, st2: &State) {
        match self.visit_checkpoints(&mut ExplainerVisitor { st2, e }, st) {
            Ok(()) => panic!("could not explain step"),
            Err(()) => {}
        }
    }
}
