use crate::concmap::*;
use crate::memory::*;
use crate::program::Program;
use crate::{consistency, thread};
use std::convert::Infallible;

use std::sync::atomic::{AtomicU64, Ordering};

pub struct Cache {
    r: ConcurrentMap<Box<[u32]>, Future<Data>>,
    n_hits: AtomicU64,
}

#[derive(Debug)]
struct Data {
    ret: Box<[Val]>,
    sc_fence: Box<[thread::ScFence]>,
    rel_fence: Box<[thread::RelFence]>,
    rel_write: Box<[thread::RelWrite]>,
}

impl Data {
    #[inline(always)]
    fn as_ref(&self) -> DataRef {
        DataRef {
            ret: &*self.ret,
            sc_fence: &*self.sc_fence,
            rel_fence: &*self.rel_fence,
            rel_write: &*self.rel_write,
        }
    }
}

pub struct DataRef<'a> {
    pub ret: &'a [Val],
    pub sc_fence: &'a [thread::ScFence],
    pub rel_fence: &'a [thread::RelFence],
    pub rel_write: &'a [thread::RelWrite],
}

impl Cache {
    #[inline]
    pub fn new() -> Self {
        Self {
            r: ConcurrentMap::new(),
            n_hits: AtomicU64::new(0),
        }
    }

    #[inline(always)]
    unsafe fn refcast<'a, 'b>(&'a self, r: &'b Data) -> DataRef<'a> {
        (*(r as *const Data)).as_ref()
    }

    pub fn get(
        &self,
        p: &Program,
        cc: &consistency::Cache,
        mem: &Memory,
        ts: &thread::State,
    ) -> DataRef {
        debug_assert!(ts.updating.is_none() && cc.check_full(p, mem, ts));
        self._get(p, cc, mem, ts)
    }

    fn _get(
        &self,
        p: &Program,
        cc: &consistency::Cache,
        mem: &Memory,
        ts: &thread::State,
    ) -> DataRef {
        debug_assert!(ts.updating.is_some() || cc.check_full(p, mem, ts));

        let mut ser = Vec::new();
        ts.serialize_for_checkpoints(&mut ser, p, mem);
        let ser = ser.into_boxed_slice();
        let (ins, index) = self.r.insert(ser, Future::Pending);
        if !ins {
            self.n_hits.fetch_add(1, Ordering::Relaxed);
            return self.r.poll(index, |_, v| unsafe { self.refcast(v) });
        }

        let mut vis = Vis {
            ret: None,
            step: Vec::new(),
            sc_fence: Vec::new(),
            rel_fence: Vec::new(),
            release: Vec::new(),
        };
        match thread::visit_transitions(&mut vis, p, mem, ts) {
            Ok(_) => {}
            Err(e) => match e {},
        }

        let mut r_ret = vis.ret.into_iter().collect::<Vec<_>>();
        let mut r_sc_fence = vis.sc_fence;
        let mut r_rel_fence = vis.rel_fence;
        let mut r_rel_write = vis.release;

        for s in vis.step {
            let q = {
                let ts2 = ts.with_pure(&s);
                if ts2.updating.is_none() && !cc.check_full(p, mem, &ts2) {
                    continue;
                }
                self._get(p, cc, mem, &ts2)
            };

            r_ret.extend_from_slice(q.ret);

            {
                let n = r_sc_fence.len();
                r_sc_fence.extend_from_slice(q.sc_fence);
                for f in &mut r_sc_fence[n..] {
                    f.back_through(&s);
                }
            }

            {
                let n = r_rel_fence.len();
                r_rel_fence.extend_from_slice(q.rel_fence);
                for f in &mut r_rel_fence[n..] {
                    f.back_through(&s);
                }
            }

            {
                let n = r_rel_write.len();
                r_rel_write.extend_from_slice(q.rel_write);
                for w in &mut r_rel_write[n..] {
                    w.back_through(&s);
                }
            }
        }

        r_ret.sort_unstable();
        r_ret.dedup();

        r_sc_fence.sort_unstable();
        r_sc_fence.dedup();

        r_rel_fence.sort_unstable();
        r_rel_fence.dedup();

        r_rel_write.sort_unstable();
        r_rel_write.dedup();

        let r = Data {
            ret: r_ret.into_boxed_slice(),
            sc_fence: r_sc_fence.into_boxed_slice(),
            rel_fence: r_rel_fence.into_boxed_slice(),
            rel_write: r_rel_write.into_boxed_slice(),
        };
        self.r.with(index, |_, v| unsafe { self.refcast(v.set(r)) })
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
    pub fn n_total(&self) -> u64 {
        self.n_hits() + self.n_misses()
    }
}

#[derive(Debug)]
struct Vis {
    ret: Option<Val>,
    step: Vec<thread::Pure>,
    sc_fence: Vec<thread::ScFence>,
    rel_fence: Vec<thread::RelFence>,
    release: Vec<thread::RelWrite>,
}

impl thread::TransitionVisitor for Vis {
    type Err = Infallible;

    fn ret(&mut self, v: Val) -> Result<(), Infallible> {
        assert!(self.ret.is_none());
        self.ret = Some(v);
        Ok(())
    }

    fn pure(&mut self, s: thread::Pure) -> Result<(), Infallible> {
        self.step.push(s);
        Ok(())
    }

    fn write(&mut self, _: thread::Write) -> Result<(), Infallible> {
        Ok(())
    }

    fn sc_fence(&mut self, f: thread::ScFence) -> Result<(), Infallible> {
        self.sc_fence.push(f);
        Ok(())
    }

    fn rel_fence(&mut self, f: thread::RelFence) -> Result<(), Infallible> {
        self.rel_fence.push(f);
        Ok(())
    }

    fn rel_write(&mut self, p: thread::RelWrite) -> Result<(), Infallible> {
        self.release.push(p);
        Ok(())
    }
}
