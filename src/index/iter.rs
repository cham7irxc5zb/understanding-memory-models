use super::Index;
use std::iter;
use std::marker::PhantomData;

#[derive(Clone)]
pub struct IndexIter<I: Index>(usize, usize, PhantomData<I>);

impl<I: Index> IndexIter<I> {
    #[inline(always)]
    pub(super) unsafe fn new_unchecked(i: usize, n: usize) -> IndexIter<I> {
        IndexIter(i, n, PhantomData)
    }
}

impl<I: Index> Iterator for IndexIter<I> {
    type Item = I;

    #[inline]
    fn next(&mut self) -> Option<I> {
        let v = self.0;
        if v == self.1 {
            None
        } else {
            self.0 = v + 1;
            Some(unsafe { I::from_usize_unchecked(v) })
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.1 - self.0;
        (n, Some(n))
    }

    #[inline]
    fn nth(&mut self, k: usize) -> Option<I> {
        let v = self.0;
        if self.1 - v <= k {
            self.1 = v;
            None
        } else {
            self.0 = v + k + 1;
            Some(unsafe { I::from_usize_unchecked(v) })
        }
    }

    #[inline]
    fn count(self) -> usize {
        self.1 - self.0
    }

    #[inline]
    fn last(self) -> Option<I> {
        let v = self.1;
        if self.0 == v {
            None
        } else {
            Some(unsafe { I::from_usize_unchecked(v - 1) })
        }
    }
}

impl<I: Index> DoubleEndedIterator for IndexIter<I> {
    #[inline]
    fn next_back(&mut self) -> Option<I> {
        let v = self.1;
        if self.0 == v {
            None
        } else {
            let v = v - 1;
            self.1 = v;
            Some(unsafe { I::from_usize_unchecked(v) })
        }
    }

    #[inline]
    fn nth_back(&mut self, k: usize) -> Option<I> {
        let v = self.1;
        if v - self.0 <= k {
            self.0 = v;
            None
        } else {
            let v = v - k - 1;
            self.1 = v;
            Some(unsafe { I::from_usize_unchecked(v) })
        }
    }
}

impl<I: Index> ExactSizeIterator for IndexIter<I> {}

impl<I: Index> iter::FusedIterator for IndexIter<I> {}
