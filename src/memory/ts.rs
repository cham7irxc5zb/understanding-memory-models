use super::overflow;
use crate::index::Index;
use std::hint::unreachable_unchecked;
use std::{fmt, ops};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct TsDiff(usize);

unsafe impl Index for TsDiff {
    const ZERO: Self = Self(0);

    #[inline(always)]
    fn as_usize(self) -> usize {
        let v = self.0;
        if v > Self::MAX {
            unsafe {
                unreachable_unchecked();
            }
        }
        v
    }

    #[inline(always)]
    unsafe fn from_usize_unchecked(v: usize) -> Self {
        if v > Self::MAX {
            unreachable_unchecked();
        }
        Self(v)
    }

    #[inline(always)]
    fn from_usize(v: usize) -> Self {
        if v > Self::MAX {
            overflow();
        }
        Self(v)
    }

    #[inline]
    unsafe fn unchecked_add(self, d: usize) -> Self {
        let Self(d) = Self::from_usize_unchecked(d);
        match self.0.checked_add(d) {
            Some(v) => Self::from_usize_unchecked(v),
            None => unreachable_unchecked(),
        }
    }

    #[inline]
    unsafe fn unchecked_sub(self, d: usize) -> Self {
        let Self(d) = Self::from_usize_unchecked(d);
        match self.0.checked_sub(d) {
            Some(v) => Self::from_usize_unchecked(v),
            None => unreachable_unchecked(),
        }
    }

    #[inline]
    unsafe fn unchecked_diff(a: Self, b: Self) -> usize {
        match b.0.checked_sub(a.0) {
            Some(v) => Self::from_usize_unchecked(v).as_usize(),
            None => unreachable_unchecked(),
        }
    }
}

define_index_type! {
    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Ts(pub TsDiff);
}

impl TsDiff {
    pub const MAX: usize = 127;
    pub const ZERO: Self = Self(0);
    pub const ONE: Self = Self(1);
}

impl ops::Add for TsDiff {
    type Output = Self;

    #[inline]
    fn add(self, other: Self) -> Self {
        Self::from_usize(self.as_usize() + other.as_usize())
    }
}

impl ops::Sub for TsDiff {
    type Output = Self;

    #[inline]
    fn sub(self, other: Self) -> Self {
        let a = self.as_usize();
        let b = other.as_usize();
        if a < b {
            overflow();
        }
        unsafe { Self::from_usize_unchecked(a - b) }
    }
}

impl fmt::Display for TsDiff {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl fmt::Debug for TsDiff {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl ops::Add<TsDiff> for Ts {
    type Output = Ts;

    #[inline(always)]
    fn add(self, other: TsDiff) -> Self {
        Self(self.0 + other)
    }
}

impl ops::Sub for Ts {
    type Output = TsDiff;

    #[inline(always)]
    fn sub(self, other: Self) -> TsDiff {
        self.0 - other.0
    }
}

impl fmt::Display for Ts {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl fmt::Debug for Ts {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
