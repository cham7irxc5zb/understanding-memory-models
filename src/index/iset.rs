use super::{Array, Index, Length, Slice};
use crate::htab;
use std::borrow::Borrow;
use std::collections::hash_map::RandomState;
use std::marker::PhantomData;
use std::{hash, iter};

struct Field<Q: ?Sized>(PhantomData<fn(&Q) -> &Q>);

impl<K: Borrow<Q>, Q: ?Sized + Eq + hash::Hash> htab::Field<K> for Field<Q> {
    type Type = Q;

    #[inline]
    fn eq(c: &K, v: &Q) -> bool {
        *c.borrow() == *v
    }

    #[inline]
    fn hash<S: hash::BuildHasher>(hs: &S, v: &Q) -> u64 {
        htab::hash(hs, v)
    }
}

#[derive(Clone)]
pub struct ISet<I: Index, K, S = RandomState>(htab::HTab<K, S>, PhantomData<fn() -> I>);

pub enum ISetEntry<'a, I: Index, K, S> {
    Vacant(ISetVacant<'a, I, K, S>),
    Occupied(ISetOccupied<'a, I, K, S>),
}

pub struct ISetOccupied<'a, I: Index, K, S>(htab::Occupied<'a, K, S>, PhantomData<fn() -> I>);

pub struct ISetVacant<'a, I: Index, K, S>(htab::Vacant<'a, K, S>, PhantomData<fn() -> I>);

impl<'a, I: Index, K, S> ISetOccupied<'a, I, K, S> {
    #[inline]
    pub fn get(&self) -> &K {
        self.0.get()
    }

    #[inline]
    pub fn index(&self) -> I {
        unsafe { I::from_usize_unchecked(self.0.index()) }
    }
}

impl<'a, I: Index, K: Eq + hash::Hash, S: hash::BuildHasher + Default> ISetVacant<'a, I, K, S> {
    #[inline]
    pub fn insert(self, k: K) -> ISetOccupied<'a, I, K, S> {
        let _ = I::from_usize(self.0.htab().len() + 1);
        ISetOccupied(self.0.insert(k), PhantomData)
    }
}

impl<I: Index, K, S: Default> ISet<I, K, S> {
    #[inline]
    pub fn new() -> Self {
        ISet(htab::HTab::new(), PhantomData)
    }
}

impl<I: Index, K, S: Default> Default for ISet<I, K, S> {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Index, K, S> ISet<I, K, S> {
    #[inline(always)]
    pub fn len(&self) -> Length<I> {
        Length::new(self.0.len())
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline(always)]
    pub fn values(&self) -> &Slice<I, K> {
        Slice::at(&*self.0.values())
    }

    #[inline(always)]
    pub fn at(&self, i: I) -> &K {
        &self.values()[i]
    }

    #[inline(always)]
    pub fn into_values(self) -> Array<I, K> {
        Array::new_vec(self.0.into_values())
    }
}

impl<I: Index, K: Eq + hash::Hash, S: hash::BuildHasher> ISet<I, K, S> {
    #[inline]
    pub fn get<'a, Q: ?Sized + Eq + hash::Hash>(&'a self, k: &Q) -> Option<I>
    where
        K: Borrow<Q>,
    {
        Some(unsafe { I::from_usize_unchecked(self.0.find::<Field<Q>>(k)?.0) })
    }

    #[inline]
    pub fn entry<'a, Q: ?Sized + Eq + hash::Hash>(&'a mut self, k: &Q) -> ISetEntry<'a, I, K, S>
    where
        K: Borrow<Q>,
    {
        match self.0.entry::<Field<Q>>(k) {
            htab::Entry::Vacant(e) => ISetEntry::Vacant(ISetVacant(e, PhantomData)),
            htab::Entry::Occupied(e) => ISetEntry::Occupied(ISetOccupied(e, PhantomData)),
        }
    }
}

impl<I: Index, K: Eq + hash::Hash, S: hash::BuildHasher + Default> ISet<I, K, S> {
    #[inline]
    pub fn check_insert(&mut self, k: K) -> Result<I, I> {
        match self.entry(&k) {
            ISetEntry::Vacant(e) => Ok(e.insert(k).index()),
            ISetEntry::Occupied(e) => Err(e.index()),
        }
    }

    #[inline]
    pub fn check_clone_insert(&mut self, k: &K) -> Result<I, I>
    where
        K: Clone,
    {
        match self.entry(k) {
            ISetEntry::Vacant(e) => Ok(e.insert(k.clone()).index()),
            ISetEntry::Occupied(e) => Err(e.index()),
        }
    }

    #[inline]
    pub fn insert(&mut self, k: K) -> I {
        match self.check_insert(k) {
            Ok(i) => i,
            Err(i) => i,
        }
    }

    pub fn singleton(k: K) -> Self {
        let mut r = ISet::new();
        r.insert(k);
        r
    }
}

impl<I: Index, K: Eq + hash::Hash, S: hash::BuildHasher + Default> iter::FromIterator<K>
    for ISet<I, K, S>
{
    fn from_iter<T: IntoIterator<Item = K>>(iter: T) -> Self {
        let mut r = ISet::new();
        for v in iter {
            r.insert(v);
        }
        r
    }
}
