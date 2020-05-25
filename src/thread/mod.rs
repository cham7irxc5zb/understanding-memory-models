use crate::index::*;
use crate::memory::*;
use crate::program::*;
use crate::util::*;

mod promise_set;
pub use promise_set::*;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct State {
    pub pc: Pctr,
    pub updating: Option<Loc>,
    pub rel: LocArray<View>,
    pub cur: View,
    pub acq: View,
    pub pr: PromiseSet,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct ConsistencyState {
    pub may_write: LocArray<bool>,
}

impl ConsistencyState {
    #[inline]
    pub fn most_restrictive() -> Self {
        Self {
            may_write: LocArray::new(|_| false),
        }
    }

    #[inline]
    pub fn semi() -> Self {
        Self {
            may_write: LocArray::new(|_| true),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Promise {
    loc: Loc,
    val: Val,
    td: TsDiff,
    is_update: bool,
    kind: PromiseKind,
}

#[derive(Clone, Debug)]
pub struct ExplainPromise {
    pub loc: Loc,
    pub val: Val,
    pub t: Ts,
    pub is_update: bool,
    pub kind: PromiseKind,
}

impl Promise {
    #[inline]
    pub fn explain(&self, ts: &State) -> ExplainPromise {
        ExplainPromise {
            loc: self.loc,
            val: self.val,
            t: ts.cur.get(self.loc) + self.td,
            is_update: self.is_update,
            kind: self.kind,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Write {
    next: Pctr,
    p: Promise,
}

impl Write {
    #[inline]
    pub fn into_promise(self) -> Promise {
        self.p
    }

    #[inline]
    pub fn loc(&self) -> Loc {
        self.p.loc
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct RelWrite {
    next: Pctr,
    loc: Loc,
    val: Val,
    vd: ViewDiff,
    vd_acq: ViewDiff,
    is_update: bool,
}

impl RelWrite {
    #[inline]
    pub fn back_through(&mut self, s: &Pure) {
        self.vd = s.step.fix_vd(self.vd);
    }
}

#[derive(Clone, Debug)]
pub struct ExplainRelWrite {
    pub loc: Loc,
    pub val: Val,
    pub t: Ts,
    pub is_update: bool,
}

impl RelWrite {
    #[inline]
    pub fn explain(&self, ts: &State) -> ExplainRelWrite {
        ExplainRelWrite {
            loc: self.loc,
            val: self.val,
            t: ts.cur.get(self.loc) + self.vd.get(self.loc),
            is_update: self.is_update,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Pure {
    next: Pctr,
    step: PureStep,
}

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Debug)]
pub struct ScFence {
    pc: Pctr,
    vd: ViewDiff,
}

impl ScFence {
    pub fn back_through(&mut self, s: &Pure) {
        self.vd = s.step.fix_vd(self.vd);
    }
}

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Debug)]
pub struct RelFence {
    pc: Pctr,
    vd: ViewDiff,
    vd_acq: ViewDiff,
}

impl RelFence {
    pub fn back_through(&mut self, s: &Pure) {
        self.vd = s.step.fix_vd(self.vd);
    }
}

#[derive(Copy, Clone, Debug)]
enum PureStep {
    View {
        loc: Loc,
        vd: ViewDiff,
        is_acquire: bool,
        is_fulfill: bool,
        is_update: bool,
    },
    Acq(ViewDiff),
    CancelUpdate(Loc),
}

impl PureStep {
    fn fix_vd(&self, vd: ViewDiff) -> ViewDiff {
        match *self {
            PureStep::View {
                loc,
                vd: v_vd,
                is_acquire,
                ..
            } => {
                if is_acquire {
                    vd + v_vd
                } else {
                    vd.with_bump(loc, v_vd.get(loc))
                }
            }
            PureStep::Acq(avd) => avd + vd,
            PureStep::CancelUpdate(_) => vd,
        }
    }
}

impl State {
    #[inline]
    pub fn new(_prog: &Program, pc: Pctr) -> Self {
        Self {
            pc,
            updating: None,
            rel: LocArray::new(|_| View::ZERO),
            cur: View::ZERO,
            acq: View::ZERO,
            pr: PromiseSet::new(),
        }
    }

    #[inline]
    pub fn valid_in_future(&self, mem: &Memory, w: &Write, cs: &mut ConsistencyState) -> bool {
        let p = &w.p;
        if p.kind != PromiseKind::New {
            return true;
        }

        if cs.may_write[p.loc] {
            return true;
        }

        // TODO: somehow permit updates.
        if p.is_update {
            return false;
        }

        if mem[p.loc].len() != self.cur.get(p.loc) + p.td {
            return false;
        }

        cs.may_write[p.loc] = true;
        true
    }

    pub fn with_shift(&self, s: Shift) -> Self {
        assert!(self.updating.is_none());

        let Shift { loc, t } = s;
        let view = self.cur.get(loc);
        if view < t {
            Self {
                pc: self.pc,
                updating: self.updating,
                rel: self.rel.clone(),
                cur: self.cur,
                acq: self.acq.with_shift(s),
                pr: self.pr.with_shift(loc, t - view),
            }
        } else {
            Self {
                pc: self.pc,
                updating: self.updating,
                rel: self.rel.map(|_, rel| rel.with_shift(s)),
                cur: self.cur.with_bump(loc, TsDiff::ONE),
                acq: self.acq.with_bump(loc, TsDiff::ONE),
                pr: self.pr.clone(),
            }
        }
    }

    fn _promise_mem(&self, mem: &Memory, p: &Promise) -> (Memory, Shift) {
        let loc = p.loc;
        let t = self.cur.get(loc) + p.td;
        let msg = Message::new(p.val, p.is_update, self.rel[loc]);
        mem.with_write(loc, t, msg, p.kind)
    }

    pub fn with_promise(&self, mem: &Memory, p: &Promise) -> (Self, Memory, Shift) {
        let (mem2, shift) = self._promise_mem(mem, p);

        let loc = p.loc;
        let td = p.td;

        let ts2 = Self {
            pc: self.pc,
            updating: self.updating,
            rel: self.rel.clone(),
            cur: self.cur,
            acq: self.acq.with_shift(shift),
            pr: self.pr.with_promise(loc, td),
        };

        (ts2, mem2, shift)
    }

    pub fn with_write(&self, mem: &Memory, w: &Write) -> (Self, Memory, Shift) {
        let (mem, shift) = self._promise_mem(mem, &w.p);

        let loc = w.p.loc;
        let td = w.p.td;

        let new_cur = self.cur.with_bump(loc, td);
        let new_acq = self.acq.with_max(new_cur);

        let ts2 = Self {
            pc: w.next,
            updating: None,
            rel: self.rel.clone(),
            cur: new_cur,
            acq: new_acq,
            pr: self.pr.with_view(loc, td - TsDiff::ONE, false),
        };
        (ts2, mem, shift)
    }

    pub fn with_rel_write(&self, mem: &Memory, w: &RelWrite) -> (Self, Memory, Shift) {
        let loc = w.loc;

        let new_cur = self.cur + w.vd;
        let new_rel = new_cur.with_clear(loc);
        let new_acq = new_cur + w.vd_acq;

        let ts2 = Self {
            pc: w.next,
            updating: None,
            rel: self.rel.map_update(loc, |_| new_rel, |_, rel| *rel),
            cur: new_cur,
            acq: new_acq,
            pr: PromiseSet::new(),
        };

        let (mem, shift) = ts2._promise_mem(
            mem,
            &Promise {
                loc,
                td: TsDiff::ZERO,
                val: w.val,
                is_update: w.is_update,
                kind: PromiseKind::New,
            },
        );

        (ts2, mem, shift)
    }

    #[inline]
    pub fn with_pure(&self, s: &Pure) -> Self {
        match s.step {
            PureStep::View {
                loc,
                vd,
                is_acquire,
                is_fulfill,
                is_update,
            } => {
                let view = self.cur + vd;
                let new_acq = self.acq.with_max(view);
                let new_cur;
                let new_pr;
                if is_acquire {
                    assert!(!is_fulfill);
                    new_cur = view;
                    new_pr = self.pr.with_viewdiff(vd);
                } else {
                    new_cur = self.cur.with_set(loc, view.get(loc));
                    new_pr = self.pr.with_view(loc, vd.get(loc), is_fulfill);
                }
                let new_updating = if is_update {
                    if is_fulfill {
                        assert_eq!(self.updating, Some(loc));
                        None
                    } else {
                        assert_eq!(self.updating, None);
                        Some(loc)
                    }
                } else {
                    self.updating
                };
                Self {
                    pc: s.next,
                    updating: new_updating,
                    rel: self.rel.clone(),
                    cur: new_cur,
                    acq: new_acq,
                    pr: new_pr,
                }
            }
            PureStep::Acq(vd) => {
                assert_eq!(self.acq, self.cur + vd);
                Self {
                    pc: s.next,
                    updating: self.updating,
                    rel: self.rel.clone(),
                    cur: self.acq,
                    acq: self.acq,
                    pr: self.pr.with_viewdiff(vd),
                }
            }
            PureStep::CancelUpdate(loc) => {
                assert_eq!(self.updating, Some(loc));
                Self {
                    pc: s.next,
                    updating: None,
                    rel: self.rel.clone(),
                    cur: self.cur,
                    acq: self.acq,
                    pr: self.pr.clone(),
                }
            }
        }
    }

    #[inline]
    pub fn with_sc_fence(&self, _mem: &Memory, sc: View, f: ScFence) -> (Self, View) {
        assert_eq!(self.updating, None);
        let sc = sc.with_max(self.cur + f.vd);

        let ts2 = Self {
            pc: f.pc,
            updating: None,
            rel: LocArray::new(|loc| sc.with_clear(loc)),
            cur: sc,
            acq: sc,
            pr: PromiseSet::new(),
        };

        (ts2, sc)
    }

    #[inline]
    pub fn with_rel_fence(&self, _mem: &Memory, f: RelFence) -> Self {
        assert_eq!(self.updating, None);
        let new_cur = self.cur + f.vd;
        let new_acq = new_cur + f.vd_acq;

        Self {
            pc: f.pc,
            updating: None,
            rel: LocArray::new(|loc| new_cur.with_clear(loc)),
            cur: new_cur,
            acq: new_acq,
            pr: PromiseSet::new(),
        }
    }

    #[inline]
    pub fn no_promises(&self) -> bool {
        self.pr.is_empty()
    }

    fn serialize_full(&self, r: &mut Vec<u32>, _p: &Program, mem: &Memory, extra: usize) {
        let mut expect = extra + 2 + 2 * ViewDiff::SER;
        for loc in Loc::iter() {
            let n_mem = mem[loc].len().as_usize();
            expect += ViewDiff::SER + LocPromises::SER + 1 + n_mem * Message::SER;
        }
        r.reserve_exact(expect);

        r.push_fast(self.pc.as_usize() as u32);
        r.push_fast(self.updating.map(|l| l.as_usize() as u32).unwrap_or(!0));
        (self.acq - View::ZERO).serialize_into(r);
        (self.cur - View::ZERO).serialize_into(r);
        for loc in Loc::iter() {
            (self.rel[loc] - View::ZERO).serialize_into(r);
            self.pr[loc].serialize_into(r);
            let m = &mem[loc];
            r.push_fast(m.len().as_usize() as u32);
            for msg in m.iter() {
                msg.serialize_into(r, self.cur);
            }
        }
    }

    fn serialize_local(
        &self,
        r: &mut Vec<u32>,
        p: &Program,
        mem: &Memory,
        for_checkpoint: bool,
        extra: usize,
    ) {
        if !p.opt().local {
            self.serialize_full(r, p, mem, extra);
            return;
        }

        let _ = for_checkpoint;
        let mut expect = extra + 2 + ViewDiff::SER;
        for loc in Loc::iter() {
            let n_mem = mem[loc].len().as_usize() - self.cur.get(loc).as_usize();
            expect += LocPromises::SER + 1 + n_mem * Message::SER;
        }
        r.reserve_exact(expect);

        r.push_fast(if for_checkpoint {
            self.pc.as_usize() as u32
        } else {
            p.inner_equiv(self.pc).as_usize() as u32
        });
        r.push_fast(self.updating.map(|l| l.as_usize() as u32).unwrap_or(!0));
        (self.acq - self.cur).serialize_into(r);
        for loc in Loc::iter() {
            self.pr[loc].serialize_into(r);
            let m = &mem[loc][self.cur.get(loc)..];
            r.push_fast(m.len().as_usize() as u32);
            for msg in m.iter() {
                msg.serialize_into(r, self.cur);
            }
        }
    }

    pub fn serialize_for_promises(&self, r: &mut Vec<u32>, p: &Program, mem: &Memory) {
        self.serialize_local(r, p, mem, false, 0);
    }

    pub fn serialize_for_consistency(
        &self,
        r: &mut Vec<u32>,
        p: &Program,
        mem: &Memory,
        cs: &ConsistencyState,
    ) {
        self.serialize_local(r, p, mem, false, Loc::N);
        for loc in Loc::iter() {
            r.push_fast(cs.may_write[loc] as u32);
        }
    }

    pub fn serialize_for_checkpoints(&self, r: &mut Vec<u32>, p: &Program, mem: &Memory) {
        self.serialize_local(r, p, mem, true, 0);
    }
}

pub trait TransitionVisitor {
    type Err;

    fn ret(&mut self, v: Val) -> Result<(), Self::Err>;
    fn pure(&mut self, s: Pure) -> Result<(), Self::Err>;
    fn write(&mut self, p: Write) -> Result<(), Self::Err>;
    fn sc_fence(&mut self, f: ScFence) -> Result<(), Self::Err>;
    fn rel_fence(&mut self, f: RelFence) -> Result<(), Self::Err>;
    fn rel_write(&mut self, p: RelWrite) -> Result<(), Self::Err>;
}

#[inline]
fn can_write(vm: &Slice<TsDiff, Message>, t: TsDiff) -> bool {
    vm.get(t).map(|msg| !msg.is_update()).unwrap_or(true)
}

fn can_acquire(_mem: &Memory, ts: &State, vd: ViewDiff) -> bool {
    for loc in Loc::iter() {
        let pr = &ts.pr[loc];
        if let Some(t) = pr.first() {
            if t <= vd.get(loc) {
                return false;
            }
        }
    }
    true
}

fn valid(mem: &Memory, ts: &State) -> bool {
    if !ts.acq.is_after(ts.cur) {
        return false;
    }
    for loc in Loc::iter() {
        let rel = ts.rel[loc];
        if rel.get(loc) != Ts::ZERO
            || !ts.cur.with_clear(loc).is_after(rel)
            || mem[loc].len() <= ts.acq.get(loc)
        {
            return false;
        }
        let vm = &mem[loc][ts.cur.get(loc)..];
        if ts.pr[loc].iter().any(|pr| !vm[pr].view().is_after(rel)) {
            return false;
        }
    }
    true
}

#[inline]
pub fn visit_transitions<V: ?Sized + TransitionVisitor>(
    vis: &mut V,
    p: &Program,
    mem: &Memory,
    ts: &State,
) -> Result<(), V::Err> {
    if cfg!(debug_assertions) && !valid(mem, ts) {
        panic!("Invalid state!\nts = {:#?}\nmem = {:#?}", ts, mem);
    }

    match p.op(ts.pc) {
        Op::Return { val } => {
            assert_eq!(ts.updating, None);
            let val = *val;

            if ts.no_promises() {
                vis.ret(val)?;
            }
        }
        Op::Ignore => {}
        Op::Fence { kind, next } => {
            let kind = *kind;
            let next = *next;

            if kind != Fence::Acq {
                assert_eq!(ts.updating, None);
            }

            match kind {
                Fence::Acq => {
                    let vd = ts.acq - ts.cur;
                    if can_acquire(mem, ts, vd) {
                        vis.pure(Pure {
                            next,
                            step: PureStep::Acq(vd),
                        })?;
                    }
                }
                Fence::Sc => {
                    if ts.no_promises() {
                        vis.sc_fence(ScFence {
                            pc: next,
                            vd: ts.acq - ts.cur,
                        })?;
                    }
                }
                Fence::Rel => {
                    if ts.no_promises() {
                        vis.rel_fence(RelFence {
                            pc: next,
                            vd: ViewDiff::ZERO,
                            vd_acq: ts.acq - ts.cur,
                        })?;
                    }
                }
            }
        }
        Op::Read {
            loc,
            is_acquire,
            is_update,
            next,
        } => {
            let loc = *loc;
            let is_acquire = *is_acquire;
            let is_update = *is_update;

            if is_update {
                assert_eq!(ts.updating, None);
            }

            let view = ts.cur.get(loc);
            let pr = &ts.pr[loc];
            let vm = &mem[loc][view..];
            assert!(!vm.is_empty());

            // We must see something at or after the current view, but strictly
            // before the first promise we have to fulfill.
            for t in pr.first().map(Index::as_len).unwrap_or(vm.len()) {
                let msg = &vm[t];
                let next = next[msg.val()];
                let read_vd = ViewDiff::saturating_sub(msg.view(), ts.cur);
                if is_acquire && !can_acquire(mem, ts, read_vd) {
                    continue;
                }

                vis.pure(Pure {
                    next,
                    step: PureStep::View {
                        is_acquire,
                        is_fulfill: false,
                        is_update,
                        loc,
                        vd: read_vd.with_set(loc, t),
                    },
                })?;
            }
        }
        Op::Write {
            loc,
            is_release,
            is_update,
            val,
            next,
        } => {
            let loc = *loc;
            let is_update = *is_update;
            let val = *val;
            let next = *next;

            let cur_t = ts.cur.get(loc);
            let pr = &ts.pr[loc];
            let vm = &mem[loc][cur_t..];
            assert!(!vm.is_empty());

            let first_promise = pr.first();

            if is_update {
                assert_eq!(ts.updating, Some(loc));
            } else {
                assert_eq!(ts.updating, None);
            }

            if !is_release {
                if let Some(td) = first_promise {
                    let msg = &vm[td];
                    if !is_update || td == TsDiff::ONE && msg.is_update() {
                        if msg.val() == val {
                            // Fulfill a promise
                            vis.pure(Pure {
                                next,
                                step: PureStep::View {
                                    loc,
                                    vd: ViewDiff::ZERO.with_set(loc, td),
                                    is_acquire: false,
                                    is_fulfill: true,
                                    is_update,
                                },
                            })?;
                        }

                        // Split promise
                        vis.write(Write {
                            next,
                            p: Promise {
                                loc,
                                val,
                                td,
                                is_update: msg.is_update(),
                                kind: PromiseKind::Split,
                            },
                        })?;
                    }
                }

                // Add write promise
                let max_td = if is_update {
                    TsDiff::ONE.as_len()
                } else {
                    first_promise.map(Index::as_len).unwrap_or(vm.len())
                };
                for td in max_td {
                    let td = td + TsDiff::ONE;
                    if can_write(vm, td) {
                        vis.write(Write {
                            next,
                            p: Promise {
                                loc,
                                val,
                                td,
                                is_update,
                                kind: PromiseKind::New,
                            },
                        })?;
                    }
                }
            } else if ts.no_promises() {
                // Add a release write
                let max_td = if is_update {
                    TsDiff::ONE.as_len()
                } else {
                    vm.len()
                };
                for td in max_td {
                    let td = td + TsDiff::ONE;
                    if can_write(vm, td) {
                        let vd = ViewDiff::ZERO.with_set(loc, td);
                        vis.rel_write(RelWrite {
                            next,
                            loc,
                            val,
                            vd,
                            vd_acq: ViewDiff::saturating_sub(ts.acq, ts.cur + vd),
                            is_update,
                        })?;
                    }
                }
            }
        }
        Op::CancelUpdate { loc, next } => {
            let loc = *loc;
            let next = *next;

            let view = ts.cur.get(loc);
            let vm = &mem[loc][view..];
            assert!(!vm.is_empty());

            assert_eq!(ts.updating, Some(loc));

            vis.pure(Pure {
                next,
                step: PureStep::CancelUpdate(loc),
            })?;
        }
    }

    Ok(())
}

pub fn for_all_imaginable_promises(
    p: &Program,
    mem: &Memory,
    ts: &State,
    mut f: impl FnMut(Promise) -> (),
) {
    let stat = p.promise_stat(ts.pc);
    for loc in Loc::iter() {
        let vm = &mem[loc][ts.cur.get(loc)..];
        for td in vm.len() {
            let td = td + TsDiff::ONE;
            if !can_write(vm, td) {
                continue;
            }
            if !p.opt().promise || stat.may_promise[loc].write {
                for val in stat.may_promise[loc].iter_values() {
                    f(Promise {
                        loc,
                        val,
                        td,
                        is_update: false,
                        kind: PromiseKind::New,
                    });
                }
            }
            if !p.opt().promise || stat.may_promise[loc].update {
                for val in stat.may_promise[loc].iter_values() {
                    f(Promise {
                        loc,
                        val,
                        td,
                        is_update: true,
                        kind: PromiseKind::New,
                    });
                }
            }
        }
        for td in ts.pr[loc].iter() {
            let is_update = vm[td].is_update();
            for val in stat.may_promise[loc].iter_values() {
                f(Promise {
                    loc,
                    val,
                    td,
                    is_update,
                    kind: PromiseKind::Split,
                });
            }
        }
    }
}
