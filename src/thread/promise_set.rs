use crate::index::*;
use crate::memory::*;
use crate::util::*;
use std::sync::Arc;
use std::{fmt, ops};

#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct LocPromises(u128);

impl LocPromises {
    #[inline]
    fn add_promise(&mut self, t: TsDiff) {
        assert!(t != TsDiff::ZERO);
        let pr = 1u128 << t.as_usize();
        let mask = pr - 1;
        self.0 = (self.0 & mask) | pr | ((self.0 & !mask) << 1);
    }

    #[inline]
    fn with_shift(&self, t: TsDiff) -> Self {
        let mask = (1u128 << t.as_usize()) - 1;
        Self((self.0 & mask) | ((self.0 & !mask) << 1))
    }

    #[inline]
    fn with_view(&self, t: TsDiff) -> Self {
        let mask = (1u128 << (t.as_usize() + 1)) - 1;
        assert!(self.0 & mask == 0);
        Self(self.0 >> t.as_usize())
    }

    #[inline]
    fn view(&mut self, t: TsDiff) {
        *self = self.with_view(t);
    }

    #[inline]
    fn remove(&mut self, t: TsDiff) {
        let pr = 1u128 << t.as_usize();
        assert!(self.0 & pr != 0);
        self.0 &= !pr;
    }

    #[inline]
    pub fn first(&self) -> Option<TsDiff> {
        if self.0 == 0 {
            None
        } else {
            Some(unsafe { TsDiff::from_usize_unchecked(self.0.trailing_zeros() as usize) })
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.count_ones() as usize
    }

    #[inline]
    pub fn iter(&self) -> LocPromisesIter {
        LocPromisesIter(self.clone())
    }
}

impl LocPromises {
    pub const SER: usize = 4;

    #[inline]
    pub fn serialize_into(&self, r: &mut Vec<u32>) {
        r.push_fast(self.0 as u32);
        r.push_fast((self.0 >> 32) as u32);
        r.push_fast((self.0 >> 64) as u32);
        r.push_fast((self.0 >> 96) as u32);
    }
}

struct DebugLocPromises(LocPromises);

impl fmt::Debug for DebugLocPromises {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_set().entries(self.0.iter()).finish()
    }
}

impl fmt::Debug for LocPromises {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", DebugLocPromises(self.clone()))
    }
}

pub struct LocPromisesIter(LocPromises);

impl Iterator for LocPromisesIter {
    type Item = TsDiff;
    #[inline]
    fn next(&mut self) -> Option<TsDiff> {
        let r = self.0.first();
        if let Some(t) = r {
            self.0.remove(t);
        }
        r
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PromiseSet(Option<Arc<LocArray<LocPromises>>>);

impl PromiseSet {
    #[inline]
    pub fn new() -> Self {
        Self(None)
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_none()
    }

    #[inline]
    pub fn with_promise(&self, loc: Loc, t: TsDiff) -> Self {
        let mut new_data = match self.0.as_ref() {
            None => LocArray::new(|_| LocPromises::default()),
            Some(d) => (**d).clone(),
        };
        new_data[loc].add_promise(t);
        Self(Some(Arc::new(new_data)))
    }

    pub fn with_shift(&self, loc: Loc, t: TsDiff) -> Self {
        match self.0.as_ref() {
            None => Self(None),
            Some(d) => {
                let prs = &**d;
                let p = &prs[loc];
                if t == TsDiff::ZERO || p.is_empty() {
                    return Self(Some(Arc::clone(d)));
                }
                let mut r = prs.clone();
                r[loc] = p.with_shift(t);
                Self(Some(Arc::new(r)))
            }
        }
    }

    pub fn with_viewdiff(&self, vd: ViewDiff) -> Self {
        match self.0.as_ref() {
            None => Self(None),
            Some(d) => {
                if vd == ViewDiff::ZERO {
                    return Self(Some(Arc::clone(d)));
                }
                let prs = &**d;
                let mut new_prs = LocArray::new(|_| LocPromises::default());
                for loc in Loc::iter() {
                    new_prs[loc] = prs[loc].with_view(vd.get(loc));
                }
                if new_prs == *prs {
                    Self(Some(Arc::clone(d)))
                } else {
                    Self(Some(Arc::new(new_prs)))
                }
            }
        }
    }

    pub fn with_view(&self, loc: Loc, td: TsDiff, is_fulfill: bool) -> Self {
        match self.0.as_ref() {
            None => {
                assert!(!is_fulfill);
                Self(None)
            }
            Some(d) => {
                if td == TsDiff::ZERO || d[loc].is_empty() {
                    assert!(!is_fulfill);
                    return Self(Some(Arc::clone(d)));
                }
                let mut new_prs = (**d).clone();
                if is_fulfill {
                    new_prs[loc].remove(td);
                    if Loc::iter().all(|l| new_prs[l].is_empty()) {
                        return Self(None);
                    }
                }
                new_prs[loc].view(td);
                Self(Some(Arc::new(new_prs)))
            }
        }
    }
}

impl ops::Index<Loc> for PromiseSet {
    type Output = LocPromises;

    #[inline(always)]
    fn index(&self, l: Loc) -> &Self::Output {
        match self.0.as_ref() {
            None => &LocPromises(0),
            Some(d) => &d[l],
        }
    }
}

impl fmt::Debug for PromiseSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut f = f.debug_map();
        if let Some(d) = self.0.as_ref() {
            let prs = &**d;
            for loc in Loc::iter() {
                let p = &prs[loc];
                if p.is_empty() {
                    continue;
                }
                f.entry(&loc, p);
            }
        }
        f.finish()
    }
}
