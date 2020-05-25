use std::collections::hash_map::RandomState;
use std::{hash, mem};

mod raw;
pub use raw::*;

pub trait Field<Container: ?Sized> {
    type Type: ?Sized;
    fn eq(c: &Container, v: &Self::Type) -> bool;
    fn hash<S: hash::BuildHasher>(hs: &S, v: &Self::Type) -> u64;
}

pub fn hash<S: hash::BuildHasher, V: ?Sized + hash::Hash>(hs: &S, v: &V) -> u64 {
    use std::hash::Hasher;
    let mut h = hs.build_hasher();
    v.hash(&mut h);
    let qh = h.finish();
    if qh == Raw::SENTINEL {
        Raw::NOT_SENTINEL
    } else {
        qh
    }
}

pub struct Identity;

impl<T: ?Sized + Eq + hash::Hash> Field<T> for Identity {
    type Type = T;

    #[inline(always)]
    fn eq(c: &T, v: &T) -> bool {
        *c == *v
    }

    #[inline(always)]
    fn hash<S: hash::BuildHasher>(hs: &S, v: &T) -> u64 {
        self::hash(hs, v)
    }
}

#[derive(Clone)]
pub struct HTab<V, S = RandomState> {
    vals: Vec<V>,
    raw: Raw,
    hs: S,
}

pub enum Entry<'a, V, S> {
    Vacant(Vacant<'a, V, S>),
    Occupied(Occupied<'a, V, S>),
}

pub struct Occupied<'a, V, S> {
    s: &'a mut HTab<V, S>,
    ix: usize,
}

impl<'a, V, S> Occupied<'a, V, S> {
    #[inline(always)]
    pub fn htab(&self) -> &HTab<V, S> {
        self.s
    }

    #[inline(always)]
    pub fn index(&self) -> usize {
        self.ix
    }

    #[inline(always)]
    pub fn get(&self) -> &V {
        unsafe { self.s.vals.get_unchecked(self.ix) }
    }

    #[inline(always)]
    pub fn get_mut(&mut self) -> &mut V {
        unsafe { self.s.vals.get_unchecked_mut(self.ix) }
    }
}

pub struct Vacant<'a, V, S> {
    s: &'a mut HTab<V, S>,
    hash: u64,
    slot: Slot,
}

impl<'a, V, S> Vacant<'a, V, S> {
    #[inline(always)]
    pub fn htab(&self) -> &HTab<V, S> {
        self.s
    }

    #[inline]
    pub fn insert(self, v: V) -> Occupied<'a, V, S> {
        let Vacant { s, slot, hash } = self;
        let ix = unsafe { s.raw_insert(slot, hash, v) };
        Occupied { s, ix }
    }
}

impl<V, S: Default> HTab<V, S> {
    #[inline]
    pub fn new() -> Self {
        Self {
            vals: Vec::new(),
            raw: Raw::new(),
            hs: S::default(),
        }
    }
}

impl<V, S: Default> Default for HTab<V, S> {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

impl<V, S> HTab<V, S> {
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.vals.len()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.vals.is_empty()
    }

    #[inline(always)]
    pub fn values(&self) -> &[V] {
        &*self.vals
    }

    #[inline(always)]
    pub fn values_mut(&mut self) -> &mut [V] {
        &mut *self.vals
    }

    #[inline(always)]
    pub fn into_values(self) -> Vec<V> {
        self.vals
    }

    unsafe fn raw_insert(&mut self, mut slot: Slot, qh: u64, v: V) -> usize {
        let qi = self.vals.len();
        self.vals.push(v);

        self.raw.maybe_grow(qi, qh, &mut slot);
        self.raw.insert(slot, qh, qi);

        qi
    }
}

impl<V, S: hash::BuildHasher> HTab<V, S> {
    fn raw_lookup<F: Field<V>>(&self, k: &F::Type) -> (u64, usize) {
        let qh = F::hash(&self.hs, k);

        let vals = &self.vals[..];
        match self.raw.lookup_by(
            qh,
            #[inline(always)]
            |i| {
                if F::eq(unsafe { vals.get_unchecked(i) }, k) {
                    Some(i)
                } else {
                    None
                }
            },
        ) {
            Ok(i) => (Raw::SENTINEL, i),
            Err(s) => (qh, unsafe { mem::transmute::<Slot, usize>(s) }),
        }
    }

    #[inline(always)]
    fn lookup<F: Field<V>>(&self, k: &F::Type) -> Result<usize, (u64, Slot)> {
        let (qh, i) = self.raw_lookup::<F>(k);
        if qh == Raw::SENTINEL {
            Ok(i)
        } else {
            Err((qh, unsafe { mem::transmute::<usize, Slot>(i) }))
        }
    }

    #[inline(always)]
    pub fn entry<F: Field<V>>(&mut self, k: &F::Type) -> Entry<V, S> {
        match self.lookup::<F>(k) {
            Ok(i) => Entry::Occupied(Occupied { s: self, ix: i }),
            Err((qh, slot)) => Entry::Vacant(Vacant {
                s: self,
                hash: qh,
                slot,
            }),
        }
    }

    #[inline(always)]
    pub fn find<F: Field<V>>(&self, k: &F::Type) -> Option<(usize, &V)> {
        match self.lookup::<F>(k) {
            Ok(i) => Some((i, unsafe { self.vals.get_unchecked(i) })),
            Err(_) => None,
        }
    }

    #[inline(always)]
    pub fn find_mut<F: Field<V>>(&mut self, k: &F::Type) -> Option<(usize, &mut V)> {
        match self.lookup::<F>(k) {
            Ok(i) => Some((i, unsafe { self.vals.get_unchecked_mut(i) })),
            Err(_) => None,
        }
    }
}
