use crate::concmap::*;
use crate::memory::*;
use crate::program::Program;
use crate::{consistency, thread};

use std::sync::atomic::{AtomicU64, Ordering};

pub struct Cache {
    r: ConcurrentMap<Box<[u32]>, Future<Box<[thread::Promise]>>>,
    n_hits: AtomicU64,
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
    unsafe fn refcast<'a, 'b>(&'a self, r: &'b Box<[thread::Promise]>) -> &'a [thread::Promise] {
        &*(&**r as *const [_])
    }

    pub fn get(
        &self,
        p: &Program,
        cc: &consistency::Cache,
        mem: &Memory,
        ts: &thread::State,
    ) -> &[thread::Promise] {
        debug_assert!(ts.updating.is_none() && cc.check_semi(p, mem, ts));

        let mut ser = Vec::new();
        ts.serialize_for_promises(&mut ser, p, mem);
        let ser = ser.into_boxed_slice();
        let (ins, index) = self.r.insert(ser, Future::Pending);
        if !ins {
            self.n_hits.fetch_add(1, Ordering::Relaxed);
            return self.r.poll(index, |_, v| unsafe { self.refcast(v) });
        }

        let mut r_promise = Vec::new();
        thread::for_all_imaginable_promises(p, &mem, &ts, |pr| {
            let (ts2, mem2, _) = ts.with_promise(&mem, &pr);
            assert!(ts2.updating.is_none());
            if cc.check_semi(p, &mem2, &ts2) {
                r_promise.push(pr);
            }
        });

        r_promise.sort_unstable();
        r_promise.dedup();
        let r_promise = r_promise.into_boxed_slice();

        self.r
            .with(index, |_, v| unsafe { self.refcast(v.set(r_promise)) })
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
