use super::{Array, Index, Length, Slice};
use crate::htab;
use std::borrow::Borrow;
use std::collections::hash_map::RandomState;
use std::hash;
use std::marker::PhantomData;

struct Field<Q: ?Sized>(PhantomData<fn(&Q) -> &Q>);

impl<K: Borrow<Q>, V, Q: ?Sized + Eq + hash::Hash> htab::Field<(K, V)> for Field<Q> {
    type Type = Q;

    #[inline]
    fn eq(c: &(K, V), v: &Q) -> bool {
        *c.0.borrow() == *v
    }

    #[inline]
    fn hash<S: hash::BuildHasher>(hs: &S, v: &Q) -> u64 {
        htab::hash(hs, v)
    }
}

#[derive(Clone)]
pub struct IMap<I: Index, K, V, S = RandomState>(htab::HTab<(K, V), S>, PhantomData<fn() -> I>);

pub enum IMapEntry<'a, I: Index, K, V, S> {
    Vacant(IMapVacant<'a, I, K, V, S>),
    Occupied(IMapOccupied<'a, I, K, V, S>),
}

pub struct IMapOccupied<'a, I: Index, K, V, S>(
    htab::Occupied<'a, (K, V), S>,
    PhantomData<fn() -> I>,
);

pub struct IMapVacant<'a, I: Index, K, V, S>(htab::Vacant<'a, (K, V), S>, PhantomData<fn() -> I>);

impl<'a, I: Index, K, V, S> IMapOccupied<'a, I, K, V, S> {
    #[inline]
    pub fn key(&self) -> &K {
        &self.0.get().0
    }

    #[inline]
    pub fn value(&self) -> &V {
        &self.0.get().1
    }

    #[inline]
    pub fn value_mut(&mut self) -> &mut V {
        &mut self.0.get_mut().1
    }

    #[inline]
    pub fn index(&self) -> I {
        unsafe { I::from_usize_unchecked(self.0.index()) }
    }
}

impl<'a, I: Index, K: Eq + hash::Hash, V, S: hash::BuildHasher + Default>
    IMapVacant<'a, I, K, V, S>
{
    #[inline]
    pub fn insert(self, k: K, v: V) -> IMapOccupied<'a, I, K, V, S> {
        let _ = I::from_usize(self.0.htab().len());
        IMapOccupied(self.0.insert((k, v)), PhantomData)
    }
}

impl<I: Index, K, V, S: Default> IMap<I, K, V, S> {
    #[inline]
    pub fn new() -> Self {
        IMap(htab::HTab::new(), PhantomData)
    }
}

impl<I: Index, K, V, S: Default> Default for IMap<I, K, V, S> {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Index, K, V, S> IMap<I, K, V, S> {
    #[inline(always)]
    pub fn len(&self) -> Length<I> {
        Length::new(self.0.len())
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline(always)]
    pub fn pairs(&self) -> &Slice<I, (K, V)> {
        Slice::at(&*self.0.values())
    }

    #[inline(always)]
    pub fn at(&self, i: I) -> &(K, V) {
        &self.pairs()[i]
    }

    #[inline(always)]
    pub fn into_pairs(self) -> Array<I, (K, V)> {
        Array::new_vec(self.0.into_values())
    }
}

impl<I: Index, K: Eq + hash::Hash, V, S: hash::BuildHasher> IMap<I, K, V, S> {
    #[inline]
    pub fn get<'a, Q: ?Sized + Eq + hash::Hash>(&'a self, v: &Q) -> Option<I>
    where
        K: Borrow<Q>,
    {
        let i = self.0.find::<Field<Q>>(v)?.0;
        let i = unsafe { I::from_usize_unchecked(i) };
        Some(i)
    }

    #[inline]
    pub fn find<Q: ?Sized + Eq + hash::Hash>(&self, k: &Q) -> Option<(I, &K, &V)>
    where
        K: Borrow<Q>,
    {
        let (i, v) = self.0.find::<Field<Q>>(k)?;
        let i = unsafe { I::from_usize_unchecked(i) };
        Some((i, &v.0, &v.1))
    }

    #[inline]
    pub fn entry<'a, Q: ?Sized + Eq + hash::Hash>(&'a mut self, k: &Q) -> IMapEntry<'a, I, K, V, S>
    where
        K: Borrow<Q>,
    {
        match self.0.entry::<Field<Q>>(k) {
            htab::Entry::Vacant(e) => IMapEntry::Vacant(IMapVacant(e, PhantomData)),
            htab::Entry::Occupied(e) => IMapEntry::Occupied(IMapOccupied(e, PhantomData)),
        }
    }
}

impl<I: Index, K: Eq + hash::Hash, V, S: hash::BuildHasher + Default> IMap<I, K, V, S> {
    #[inline]
    pub fn check_insert(&mut self, k: K, v: V) -> Result<I, I> {
        match self.entry(&k) {
            IMapEntry::Vacant(e) => Ok(e.insert(k, v).index()),
            IMapEntry::Occupied(e) => Err(e.index()),
        }
    }

    #[inline]
    pub fn check_clone_insert(&mut self, k: &K, v: V) -> Result<I, I>
    where
        K: Clone,
    {
        match self.entry(k) {
            IMapEntry::Vacant(e) => Ok(e.insert(k.clone(), v).index()),
            IMapEntry::Occupied(e) => Err(e.index()),
        }
    }

    #[inline]
    pub fn insert(&mut self, k: K, v: V) -> I {
        match self.check_insert(k, v) {
            Ok(i) => i,
            Err(i) => i,
        }
    }
}
