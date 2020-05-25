use super::{overflow, Loc, Shift, Ts, TsDiff};
use crate::index::Index;
use crate::util::*;
use std::{fmt, ops};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ViewDiff(u64);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct View(pub ViewDiff);

impl ViewDiff {
    pub const ZERO: ViewDiff = Self(0);

    #[inline(always)]
    fn from_u64(v: u64) -> Self {
        Self(v)
    }

    #[inline(always)]
    fn as_u64(self) -> u64 {
        self.0
    }

    #[inline]
    pub fn saturating_sub(a: View, b: View) -> Self {
        let a = a.0.as_u64();
        let b = b.0.as_u64();
        let mask = ((b.wrapping_sub(a) >> 7) & 0x01010101_01010101) * 255;
        Self::from_u64((a & mask) - (b & mask))
    }

    #[inline]
    pub fn checked_sub(a: View, b: View) -> Option<Self> {
        let a = a.0.as_u64();
        let b = b.0.as_u64();
        let r = a.wrapping_sub(b);
        if r & 0x80808080_80808080 != 0 {
            return None;
        }
        Some(Self::from_u64(r))
    }

    #[inline]
    pub fn is_after(self, other: Self) -> bool {
        let a = self.as_u64();
        let b = other.as_u64();
        let diff = a.wrapping_sub(b);
        (diff >> 7) & 0x80808080_80808080 == 0
    }

    #[inline]
    pub fn get(&self, l: Loc) -> TsDiff {
        let l = l.as_usize();
        if l >= 8 {
            overflow();
        }
        let shift = l * 8;
        unsafe { TsDiff::from_usize_unchecked(((self.as_u64() >> shift) & 255) as usize) }
    }

    #[inline]
    pub fn set(&mut self, l: Loc, t: TsDiff) {
        *self = self.with_set(l, t);
    }

    #[inline(always)]
    pub fn with_set(self, l: Loc, t: TsDiff) -> Self {
        let l = l.as_usize();
        if l >= 8 {
            overflow();
        }
        let shift = l * 8;
        let mask = 255 << shift;
        Self::from_u64(self.as_u64() & !mask | ((t.as_usize() as u64) << shift))
    }

    pub fn with_shift(self, l: Loc, t: TsDiff) -> Self {
        if self.get(l) < t {
            self.clone()
        } else {
            self.with_bump(l, TsDiff::ONE)
        }
    }

    #[inline]
    pub fn bump(&mut self, l: Loc, dt: TsDiff) {
        *self = self.with_bump(l, dt);
    }

    #[inline]
    pub fn with_bump(self, l: Loc, dt: TsDiff) -> Self {
        let l = l.as_usize();
        if l >= 8 {
            overflow();
        }
        let shift = l * 8;
        let r = self.as_u64() + ((dt.as_usize() as u64) << shift);
        if r & 0x80808080_80808080 != 0 {
            overflow();
        }
        Self::from_u64(r)
    }

    #[inline]
    pub fn with_max(&self, other: Self) -> Self {
        let a = self.as_u64();
        let b = other.as_u64();
        let mask = ((b.wrapping_sub(a) >> 7) & 0x01010101_01010101) * 255;
        Self::from_u64((a & mask) | (b & !mask))
    }
}

impl ViewDiff {
    pub const SER: usize = 2;

    pub fn serialize_into(self, out: &mut Vec<u32>) {
        let v = self.as_u64();
        out.push_fast(v as u32);
        out.push_fast((v >> 32) as u32);
    }
}

impl ops::Add for ViewDiff {
    type Output = Self;

    #[inline]
    fn add(self, other: Self) -> Self {
        let a = self.as_u64();
        let b = other.as_u64();
        let r = a + b;
        if r & 0x80808080_80808080 != 0 {
            overflow()
        }
        Self::from_u64(r)
    }
}

impl ops::Sub for ViewDiff {
    type Output = Self;

    #[inline]
    fn sub(self, other: Self) -> Self {
        let a = self.as_u64();
        let b = other.as_u64();
        let r = a.wrapping_sub(b);
        if r & 0x80808080_80808080 != 0 {
            overflow()
        }
        Self::from_u64(r)
    }
}

impl fmt::Debug for ViewDiff {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let as_ts = self.as_u64().to_le_bytes();
        write!(f, "{:?}", as_ts)
    }
}

impl View {
    pub const ZERO: View = Self(ViewDiff::ZERO);

    #[inline(always)]
    pub fn with_bump(self, l: Loc, dt: TsDiff) -> Self {
        Self(self.0.with_bump(l, dt))
    }

    #[inline(always)]
    pub fn with_max(self, other: Self) -> Self {
        Self(self.0.with_max(other.0))
    }

    #[inline(always)]
    pub fn with_shift(self, s: Shift) -> Self {
        Self(self.0.with_shift(s.loc, s.t.0))
    }

    #[inline(always)]
    pub fn get(self, l: Loc) -> Ts {
        Ts(self.0.get(l))
    }

    #[inline(always)]
    pub fn set(&mut self, l: Loc, t: Ts) {
        self.0.set(l, t.0);
    }

    #[inline(always)]
    pub fn with_set(self, l: Loc, t: Ts) -> Self {
        Self(self.0.with_set(l, t.0))
    }

    #[inline(always)]
    pub fn with_clear(self, l: Loc) -> Self {
        self.with_set(l, Ts(TsDiff::ZERO))
    }

    #[inline]
    pub fn is_after(self, other: Self) -> bool {
        self.0.is_after(other.0)
    }
}

impl ops::Add<ViewDiff> for View {
    type Output = View;

    #[inline(always)]
    fn add(self, other: ViewDiff) -> Self {
        Self(self.0 + other)
    }
}

impl ops::Sub for View {
    type Output = ViewDiff;

    #[inline(always)]
    fn sub(self, other: Self) -> ViewDiff {
        self.0 - other.0
    }
}

impl fmt::Debug for View {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
