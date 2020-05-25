use crate::concmap::*;
use crate::index::*;
use crate::memory::*;
use crate::program::*;
use crate::system;

use std::sync::atomic::{AtomicU64, Ordering};

pub struct Explorer {
    out: ConcurrentMap<Array<system::Tid, Val>, usize>,
    sys: system::System,
    st: ConcurrentMap<system::State, usize>,
    q: ConcurrentBag<usize>,
    n_hits: AtomicU64,
}

impl Explorer {
    pub fn new(program: Program, n_workers: usize) -> Self {
        Self {
            out: ConcurrentMap::new(),
            sys: system::System::new(program),
            st: ConcurrentMap::new(),
            q: ConcurrentBag::new(n_workers),
            n_hits: AtomicU64::new(0),
        }
    }

    pub fn add(&self, st: system::State, prev: usize) {
        let (did, i) = self.st.insert(st, prev);
        if !did {
            self.n_hits.fetch_add(1, Ordering::Relaxed);
            return;
        }
        self.q.push(i);
    }

    pub fn explore(&self) {
        while let Some(i) = self.q.pop() {
            let st = self.st.with(i, |st, _| st.clone());
            let (sts, rets) = self.sys.checkpoint(&st);
            for r in rets {
                self.out.insert(r, i);
            }
            for st2 in sts {
                self.add(st2, i);
            }
            if i % 1000000 == 0 {
                self.report_progress();
            }
        }
    }

    pub fn explain(&self, i: usize) -> Vec<system::ExplainStep> {
        let (mut next_st, mut i) = self.st.with(i, |k, v| (k.clone(), *v));
        if i == !0 {
            return Vec::new();
        }
        let mut ex = system::Explained::new();
        loop {
            let (st, prev_i) = self.st.with(i, |k, v| (k.clone(), *v));
            self.sys.explain(&mut ex, &st, &next_st);
            if prev_i == !0 {
                break;
            }
            next_st = st;
            i = prev_i;
        }
        ex.done()
    }

    pub fn new_state(&self, thrs: &[Pctr]) {
        let q = self.sys.new_state(thrs);
        self.add(q, !0);
    }

    pub fn out(&self) -> Vec<(Array<system::Tid, Val>, usize)> {
        self.out.pairs()
    }

    pub fn report_progress(&self) {
        let un = self.q.len();
        let total = self.st.len();
        let vis = total - un;
        eprintln!(
            "Visited {} states, {} unexplored, {} hits",
            vis,
            un,
            self.n_hits()
        );
        self.sys.report();
    }

    pub fn report(&self) {
        eprintln!("Explorer: {} / {}", self.n_misses(), self.n_total());
        self.sys.report();
    }

    #[inline]
    fn n_hits(&self) -> u64 {
        self.n_hits.load(Ordering::Relaxed)
    }

    #[inline]
    fn n_misses(&self) -> u64 {
        self.st.len().as_usize() as u64
    }

    #[inline]
    fn n_total(&self) -> u64 {
        self.n_hits() + self.n_misses()
    }
}
