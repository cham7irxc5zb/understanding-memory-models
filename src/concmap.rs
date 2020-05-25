use crate::htab;

use std::collections::hash_map::RandomState;
use std::collections::VecDeque;
use std::sync::mpsc;
use std::sync::{Condvar, Mutex};
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
    tabs: [Mutex<Tab<(K, V)>>; N],
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

        let mut tab = self.tabs[ti].lock().unwrap();
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
        let mut tab = self.tabs[ti].lock().unwrap();
        let kv = &mut tab.kvs[i];
        f(&kv.0, &mut kv.1)
    }

    pub fn try_with<R>(&self, mut i: usize, f: impl FnOnce(&K, &mut V) -> R) -> Option<R> {
        let ti = i % N;
        i /= N;
        let mut tab = self.tabs[ti].lock().unwrap();
        let kv = tab.kvs.get_mut(i)?;
        Some(f(&kv.0, &mut kv.1))
    }

    pub fn len(&self) -> usize {
        let mut r = 0;
        for ti in 0..N {
            let tab = self.tabs[ti].lock().unwrap();
            r += tab.kvs.len();
        }
        r
    }

    pub fn pairs(&self) -> Vec<(K, V)>
    where
        K: Clone,
        V: Clone,
    {
        let mut r = Vec::new();
        for ti in 0..N {
            let tab = self.tabs[ti].lock().unwrap();
            r.extend_from_slice(&*tab.kvs);
        }
        r
    }
}

impl<K: PartialEq + Eq + hash::Hash, V, S: hash::BuildHasher> ConcurrentMap<K, Future<V>, S> {
    pub fn poll<R>(&self, i: usize, f: impl FnOnce(&K, &V) -> R) -> R {
        let mut f = Some(f);
        let r = self.with(i, |k, v| match v {
            Future::Pending => {
                let mut ev = Event::new();
                let r = ev.poll();
                *v = Future::Wait(ev);
                Err(r)
            }
            Future::Wait(ev) => Err(ev.poll()),
            Future::Ready(v) => Ok(f.take().unwrap()(k, v)),
        });

        let w = match r {
            Ok(r) => return r,
            Err(w) => w,
        };
        let f = f.take().unwrap();
        w.wait();

        self.with(i, |k, v| match v {
            Future::Ready(v) => f(k, v),
            _ => panic!(),
        })
    }
}

pub struct Event(Box<(mpsc::SyncSender<()>, Option<mpsc::Receiver<()>>)>);

pub struct EventPoll(mpsc::SyncSender<()>);

impl Event {
    pub fn new() -> Self {
        let (send, recv) = mpsc::sync_channel(0);
        Self(Box::new((send, Some(recv))))
    }

    pub fn poll(&mut self) -> EventPoll {
        EventPoll((self.0).0.clone())
    }

    pub fn wake(&mut self) {
        (self.0).1 = None;
    }
}

impl EventPoll {
    fn wait(&self) {
        match self.0.send(()) {
            Ok(_) => panic!(),
            Err(mpsc::SendError(())) => {}
        }
    }
}

pub enum Future<T> {
    Pending,
    Wait(Event),
    Ready(T),
}

impl<T> Future<T> {
    #[inline(always)]
    pub fn set(&mut self, v: T) -> &mut T {
        match self {
            Self::Pending | Self::Wait(_) => {}
            Self::Ready(_) => panic!(),
        }
        *self = Self::Ready(v);
        match self {
            Self::Ready(v) => v,
            _ => panic!(),
        }
    }
}

pub struct ConcurrentBag<T> {
    q: Mutex<(usize, usize, VecDeque<T>)>,
    cv: Condvar,
}

impl<T> ConcurrentBag<T> {
    pub fn new(n: usize) -> Self {
        Self {
            q: Mutex::new((n, 0, VecDeque::new())),
            cv: Condvar::new(),
        }
    }

    pub fn push(&self, v: T) {
        let mut q = self.q.lock().unwrap();
        q.2.push_back(v);
        if q.1 > 0 {
            self.cv.notify_one();
        }
    }

    pub fn pop(&self) -> Option<T> {
        let mut q = self.q.lock().unwrap();
        if let Some(v) = q.2.pop_front() {
            return Some(v);
        }
        q.1 += 1;
        loop {
            if q.1 == q.0 {
                self.cv.notify_all();
                return None;
            }
            q = self.cv.wait(q).unwrap();
            if let Some(v) = q.2.pop_front() {
                q.1 -= 1;
                return Some(v);
            }
        }
    }

    pub fn len(&self) -> usize {
        self.q.lock().unwrap().2.len()
    }
}
