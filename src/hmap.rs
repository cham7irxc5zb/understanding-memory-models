use crate::htab;
use std::borrow::Borrow;
use std::collections::hash_map::RandomState;
use std::hash;
use std::marker::PhantomData;

#[derive(Clone)]
pub struct HMap<K, V, S = RandomState>(htab::HTab<(K, V), S>);

struct Field<K, V, Q: ?Sized>(PhantomData<fn(&(K, V)) -> &Q>);

impl<K: Borrow<Q>, V, Q: ?Sized + Eq + hash::Hash> htab::Field<(K, V)> for Field<K, V, Q> {
    type Type = Q;

    #[inline(always)]
    fn eq(c: &(K, V), v: &Q) -> bool {
        *c.0.borrow() == *v
    }

    #[inline(always)]
    fn hash<S: hash::BuildHasher>(hs: &S, v: &Q) -> u64 {
        htab::hash(hs, v)
    }
}

pub enum HMapEntry<'a, K, V, S> {
    Vacant(HMapVacant<'a, K, V, S>),
    Occupied(HMapOccupied<'a, K, V, S>),
}

pub struct HMapVacant<'a, K, V, S>(htab::Vacant<'a, (K, V), S>);

pub struct HMapOccupied<'a, K, V, S>(htab::Occupied<'a, (K, V), S>);

impl<'a, K, V, S> HMapOccupied<'a, K, V, S> {
    #[inline(always)]
    pub fn index(&self) -> usize {
        self.0.index()
    }

    #[inline(always)]
    pub fn key(&self) -> &K {
        &self.0.get().0
    }

    #[inline(always)]
    pub fn value(&self) -> &V {
        &self.0.get().1
    }

    #[inline(always)]
    pub fn key_value_mut(&mut self) -> (&K, &mut V) {
        let v = self.0.get_mut();
        (&v.0, &mut v.1)
    }

    #[inline(always)]
    pub fn value_mut(&mut self) -> &mut V {
        &mut self.0.get_mut().1
    }
}

impl<'a, K, V, S> HMapVacant<'a, K, V, S> {
    #[inline(always)]
    pub fn insert(self, k: K, v: V) -> HMapOccupied<'a, K, V, S> {
        HMapOccupied(self.0.insert((k, v)))
    }
}

impl<K, V, S: Default> HMap<K, V, S> {
    #[inline(always)]
    pub fn new() -> Self {
        HMap(htab::HTab::new())
    }
}

impl<K, V, S: Default> Default for HMap<K, V, S> {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V, S> HMap<K, V, S> {
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline(always)]
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.0.values().iter().map(|kv| (&kv.0, &kv.1))
    }

    #[inline(always)]
    pub fn into_pairs(self) -> Vec<(K, V)> {
        self.0.into_values()
    }
}

impl<K: Eq + hash::Hash, V, S: hash::BuildHasher> HMap<K, V, S> {
    #[inline]
    pub fn find<Q: ?Sized + Eq + hash::Hash>(&self, k: &Q) -> Option<(usize, &K, &V)>
    where
        K: Borrow<Q>,
    {
        let (i, v) = self.0.find::<Field<K, V, Q>>(k)?;
        Some((i, &v.0, &v.1))
    }

    #[inline]
    pub fn find_mut<Q: ?Sized + Eq + hash::Hash>(&mut self, k: &Q) -> Option<(usize, &K, &mut V)>
    where
        K: Borrow<Q>,
    {
        let (i, v) = self.0.find_mut::<Field<K, V, Q>>(k)?;
        Some((i, &v.0, &mut v.1))
    }

    #[inline]
    pub fn get<Q: ?Sized + Eq + hash::Hash>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
    {
        self.find(k).map(|(_, _, v)| v)
    }

    #[inline]
    pub fn get_mut<Q: ?Sized + Eq + hash::Hash>(&mut self, k: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
    {
        self.find_mut(k).map(|(_, _, v)| v)
    }

    #[inline]
    pub fn at(&self, ix: usize) -> (&K, &V) {
        let v = &self.0.values()[ix];
        (&v.0, &v.1)
    }

    #[inline]
    pub fn at_mut(&mut self, ix: usize) -> (&K, &mut V) {
        let v = &mut self.0.values_mut()[ix];
        (&v.0, &mut v.1)
    }

    #[inline]
    pub fn entry<Q: ?Sized + Eq + hash::Hash>(&mut self, k: &Q) -> HMapEntry<K, V, S>
    where
        K: Borrow<Q>,
    {
        match self.0.entry::<Field<K, V, Q>>(k) {
            htab::Entry::Vacant(e) => HMapEntry::Vacant(HMapVacant(e)),
            htab::Entry::Occupied(e) => HMapEntry::Occupied(HMapOccupied(e)),
        }
    }
}

impl<K: Eq + hash::Hash, V, S: hash::BuildHasher + Default> HMap<K, V, S> {
    pub fn insert(&mut self, k: K, v: V) -> bool {
        match self.entry(&k) {
            HMapEntry::Vacant(e) => {
                e.insert(k, v);
                true
            }
            HMapEntry::Occupied(_) => false,
        }
    }
}
