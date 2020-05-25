use std::marker::PhantomData;
use std::{cmp, fmt, hash};

pub unsafe trait Index: Sized + Copy + Eq + Ord + hash::Hash {
    const ZERO: Self;

    fn from_usize(v: usize) -> Self;
    fn as_usize(self) -> usize;

    fn as_len(self) -> Length<Self> {
        unsafe { Length::new_unchecked(self.as_usize()) }
    }

    unsafe fn from_usize_unchecked(v: usize) -> Self;

    unsafe fn unchecked_add(self, d: usize) -> Self;
    unsafe fn unchecked_sub(self, d: usize) -> Self;
    unsafe fn unchecked_diff(a: Self, b: Self) -> usize;
}

unsafe impl Index for usize {
    const ZERO: Self = 0;

    #[inline(always)]
    fn from_usize(v: usize) -> Self {
        v
    }

    #[inline(always)]
    fn as_usize(self) -> usize {
        self
    }

    #[inline(always)]
    unsafe fn from_usize_unchecked(v: usize) -> Self {
        v
    }

    #[inline(always)]
    unsafe fn unchecked_add(self, d: usize) -> Self {
        match self.checked_add(d) {
            Some(v) => v,
            None => ::core::hint::unreachable_unchecked(),
        }
    }

    #[inline(always)]
    unsafe fn unchecked_sub(self, d: usize) -> Self {
        match self.checked_sub(d) {
            Some(v) => v,
            None => ::core::hint::unreachable_unchecked(),
        }
    }

    #[inline(always)]
    unsafe fn unchecked_diff(a: Self, b: Self) -> usize {
        match b.checked_sub(a) {
            Some(v) => v,
            None => ::core::hint::unreachable_unchecked(),
        }
    }
}

unsafe impl Index for u32 {
    const ZERO: u32 = 0;

    #[inline(always)]
    fn from_usize(v: usize) -> Self {
        ::core::convert::TryFrom::<usize>::try_from(v).expect("index type overflow")
    }

    #[inline(always)]
    fn as_usize(self) -> usize {
        self as usize
    }

    #[inline(always)]
    unsafe fn from_usize_unchecked(v: usize) -> Self {
        v as u32
    }

    #[inline(always)]
    unsafe fn unchecked_add(self, d: usize) -> Self {
        let d = Self::from_usize_unchecked(d);
        match self.checked_add(d) {
            Some(v) => v,
            None => ::core::hint::unreachable_unchecked(),
        }
    }

    #[inline(always)]
    unsafe fn unchecked_sub(self, d: usize) -> Self {
        let d = Self::from_usize_unchecked(d);
        match self.checked_sub(d) {
            Some(v) => v,
            None => ::core::hint::unreachable_unchecked(),
        }
    }

    #[inline(always)]
    unsafe fn unchecked_diff(a: Self, b: Self) -> usize {
        match b.checked_sub(a) {
            Some(v) => v as usize,
            None => ::core::hint::unreachable_unchecked(),
        }
    }
}

macro_rules! define_index_type {
    ($(#[$attr:meta])* $vis:vis struct $name:ident($tvis:vis $ty:ty);) => {
        $(#[$attr])*
        #[repr(transparent)]
        $vis struct $name($tvis $ty);

        unsafe impl $crate::index::Index for $name {
            const ZERO: Self = Self(<$ty as $crate::index::Index>::ZERO);

            #[inline]
            fn from_usize(v: usize) -> Self {
                Self(<$ty as $crate::index::Index>::from_usize(v))
            }

            #[inline]
            unsafe fn from_usize_unchecked(v: usize) -> Self {
                Self(<$ty as $crate::index::Index>::from_usize_unchecked(v))
            }

            #[inline]
            fn as_usize(self) -> usize {
                <$ty as $crate::index::Index>::as_usize(self.0)
            }

            #[inline]
            unsafe fn unchecked_add(self, d: usize) -> Self {
                Self(<$ty as $crate::index::Index>::unchecked_add(self.0, d))
            }

            #[inline]
            unsafe fn unchecked_sub(self, d: usize) -> Self {
                Self(<$ty as $crate::index::Index>::unchecked_sub(self.0, d))
            }

            #[inline]
            unsafe fn unchecked_diff(a: Self, b: Self) -> usize {
                <$ty as $crate::index::Index>::unchecked_diff(a.0, b.0)
            }
        }
    };
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Length<I: Index>(usize, PhantomData<I>);

impl<I: Index> Length<I> {
    pub const ZERO: Self = Self(0, PhantomData);

    #[inline]
    pub fn new(n: usize) -> Self {
        if n != 0 {
            let _ = I::from_usize(n - 1);
        }
        Self(n, PhantomData)
    }

    #[inline]
    pub unsafe fn new_unchecked(n: usize) -> Self {
        Self(n, PhantomData)
    }

    #[inline(always)]
    pub fn as_usize(self) -> usize {
        self.0
    }

    #[inline]
    pub fn grow(&mut self) -> I {
        let r = I::from_usize(self.0);
        self.0 += 1;
        r
    }
}

impl<I: Index + fmt::Debug> fmt::Debug for Length<I> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <usize as fmt::Debug>::fmt(&self.0, f)
    }
}

impl<I: Index> IntoIterator for Length<I> {
    type Item = I;
    type IntoIter = IndexIter<I>;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        unsafe { IndexIter::new_unchecked(0, self.0) }
    }
}

impl<I: Index> PartialEq<I> for Length<I> {
    #[inline(always)]
    fn eq(&self, other: &I) -> bool {
        self.0 == other.as_usize()
    }
}

impl<I: Index> PartialOrd<I> for Length<I> {
    #[inline(always)]
    fn partial_cmp(&self, other: &I) -> Option<cmp::Ordering> {
        self.0.partial_cmp(&other.as_usize())
    }

    #[inline(always)]
    fn lt(&self, other: &I) -> bool {
        self.0 < other.as_usize()
    }

    #[inline(always)]
    fn le(&self, other: &I) -> bool {
        self.0 <= other.as_usize()
    }

    #[inline(always)]
    fn gt(&self, other: &I) -> bool {
        self.0 > other.as_usize()
    }

    #[inline(always)]
    fn ge(&self, other: &I) -> bool {
        self.0 >= other.as_usize()
    }
}

mod iter;
pub use iter::*;

mod array;
pub use array::*;

mod iset;
pub use iset::*;

mod imap;
pub use imap::*;
