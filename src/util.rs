pub trait VecPushFastExt<T> {
    fn push_fast(&mut self, value: T);
    fn extend_from_slice_fast(&mut self, v: &[T])
    where
        T: Clone;
}

impl<T> VecPushFastExt<T> for Vec<T> {
    #[inline(always)]
    fn push_fast(&mut self, v: T) {
        let n = self.len();
        if n >= self.capacity() {
            unreachable!();
        }
        unsafe {
            self.as_mut_ptr().add(n).write(v);
            self.set_len(n + 1);
        }
    }

    #[inline(always)]
    fn extend_from_slice_fast(&mut self, v: &[T])
    where
        T: Clone,
    {
        let n = self.len();
        if self.capacity() < v.len() || self.capacity() - v.len() < n {
            unreachable!();
        }
        unsafe {
            let mut out = self.as_mut_ptr().add(n);
            for v in v {
                out.write(v.clone());
                out = out.add(1);
            }
            self.set_len(n + v.len());
        }
    }
}
