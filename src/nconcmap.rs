use crate::htab;

use std::collections::hash_map::RandomState;
use std::collections::VecDeque;
use std::cell::UnsafeCell;
use std::{hash, mem};

const B: usize = 5;
const N: usize = 1 << B;

struct Tab<V> {
    raw: htab::Raw,
    kvs: Vec<V>,
}

impl<V> Default for Tab<V> {
    fn default() -> Self {
        Tab {
            raw: htab::Raw::new(),
            kvs: Vec::new(),
        }
    }
}

pub struct ConcurrentMap<K, V, S = RandomState> {
    tabs: [UnsafeCell<Tab<(K, V)>>; N],
    hs: S,
}

impl<K, V, S: Default> ConcurrentMap<K, V, S> {
    #[inline]
    pub fn new() -> Self {
        ConcurrentMap {
            tabs: Default::default(),
            hs: S::default(),
        }
    }
}

impl<K: PartialEq + Eq + hash::Hash, V, S: hash::BuildHasher> ConcurrentMap<K, V, S> {
    pub fn insert(&self, k: K, v: V) -> (bool, usize) {
        let h = htab::hash(&self.hs, &k);
        let ti = (h >> (64 - B)) as usize;

        let tab = unsafe { &mut *self.tabs[ti].get() };
        let kvs = &tab.kvs[..];
        let (did_ins, i) = match tab.raw.lookup_by(
            h,
            #[inline(always)]
            |i| {
                if unsafe { kvs.get_unchecked(i) }.0 == k {
                    Some(i)
                } else {
                    None
                }
            },
        ) {
            Ok(i) => (false, i),
            Err(mut s) => {
                let i = kvs.len();
                tab.raw.maybe_grow(i, h, &mut s);
                unsafe {
                    tab.raw.insert(s, h, i);
                }
                tab.kvs.push((k, v));
                (true, i)
            }
        };

        mem::drop(tab);
        (did_ins, i * N + ti)
    }

    pub fn with<R>(&self, mut i: usize, f: impl FnOnce(&K, &mut V) -> R) -> R {
        let ti = i % N;
        i /= N;
        let tab = unsafe { &mut *self.tabs[ti].get() };
        let kv = &mut tab.kvs[i];
        f(&kv.0, &mut kv.1)
    }

    pub fn try_with<R>(&self, mut i: usize, f: impl FnOnce(&K, &mut V) -> R) -> Option<R> {
        let ti = i % N;
        i /= N;
        let tab = unsafe { &mut *self.tabs[ti].get() };
        let kv = tab.kvs.get_mut(i)?;
        Some(f(&kv.0, &mut kv.1))
    }

    pub fn len(&self) -> usize {
        let mut r = 0;
        for ti in 0..N {
            let tab = unsafe { &mut *self.tabs[ti].get() };
            r += tab.kvs.len();
        }
        r
    }

    pub fn pairs(&self) -> Vec<(K, V)> where K: Clone, V: Clone {
        let mut r = Vec::new();
        for ti in 0..N {
            let tab = unsafe { &mut *self.tabs[ti].get() };
            r.extend_from_slice(&*tab.kvs);
        }
        r
    }
}

impl<K: PartialEq + Eq + hash::Hash, V, S: hash::BuildHasher> ConcurrentMap<K, Future<V>, S> {
    pub fn poll<R>(&self, i: usize, f: impl FnOnce(&K, &V) -> R) -> R {
        self.with(i, |k, v| match v {
            Future::Pending => panic!(),
            Future::Ready(v) => f(k, v),
        })
    }
}

pub enum Future<T> {
    Pending,
    Ready(T),
}

impl<T> Future<T> {
    #[inline(always)]
    pub fn set(&mut self, v: T) -> &mut T {
        match self {
            Self::Pending => {
                *self = Self::Ready(v);
                match self {
                    Self::Ready(v) => v,
                    _ => panic!(),
                }
            }
            Self::Ready(_) => panic!(),
        }
    }
}

pub struct ConcurrentBag<T> {
    q: UnsafeCell<VecDeque<T>>,
}

impl<T> ConcurrentBag<T> {
    pub fn new(n: usize) -> Self {
        assert!(n == 1);
        Self {
            q: UnsafeCell::new(VecDeque::new()),
        }
    }

    pub fn push(&self, v: T) {
        let q = unsafe { &mut *self.q.get() };
        q.push_back(v);
    }

    pub fn pop(&self) -> Option<T> {
        let q = unsafe { &mut *self.q.get() };
        q.pop_front()
    }

    pub fn len(&self) -> usize {
        let q = unsafe { &mut *self.q.get() };
        q.len()
    }
}
