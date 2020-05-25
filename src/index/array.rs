use super::{Index, Length};
use std::marker::PhantomData;
use std::mem::MaybeUninit;
use std::{cmp, fmt, hash, mem, ops, slice, vec};

#[repr(transparent)]
pub struct Slice<I: Index, T>(PhantomData<fn(I) -> I>, [T]);

impl<I: Index, T: fmt::Debug> fmt::Debug for Slice<I, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.1, f)
    }
}

impl<I: Index, T> Slice<I, T> {
    #[inline(always)]
    pub unsafe fn from_raw_parts<'a>(p: *const T, len: Length<I>) -> &'a Slice<I, T> {
        &*(slice::from_raw_parts(p, len.as_usize()) as *const [T] as *const Self)
    }

    #[inline(always)]
    pub fn at(s: &[T]) -> &Slice<I, T> {
        let _ = Length::<I>::new(s.len());
        unsafe { Self::at_unchecked(s) }
    }

    #[inline(always)]
    pub fn at_mut(s: &mut [T]) -> &mut Slice<I, T> {
        let _ = Length::<I>::new(s.len());
        unsafe { Self::at_unchecked_mut(s) }
    }

    #[inline(always)]
    pub unsafe fn at_unchecked(s: &[T]) -> &Slice<I, T> {
        &*(s as *const [T] as *const Self)
    }

    #[inline(always)]
    pub unsafe fn at_unchecked_mut(s: &mut [T]) -> &mut Slice<I, T> {
        &mut *(s as *mut [T] as *mut Self)
    }

    #[inline(always)]
    pub fn len(&self) -> Length<I> {
        unsafe { Length::new_unchecked(self.1.len()) }
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.1.is_empty()
    }

    #[inline(always)]
    pub fn as_ui(&self) -> &[T] {
        &self.1
    }

    #[inline(always)]
    pub fn as_mut_ui(&mut self) -> &mut [T] {
        &mut self.1
    }

    #[inline(always)]
    pub unsafe fn get_unchecked(&self, i: I) -> &T {
        self.1.get_unchecked(i.as_usize())
    }

    #[inline(always)]
    pub unsafe fn get_unchecked_mut(&mut self, i: I) -> &mut T {
        self.1.get_unchecked_mut(i.as_usize())
    }

    #[inline(always)]
    pub fn get(&self, i: I) -> Option<&T> {
        self.1.get(i.as_usize())
    }

    #[inline(always)]
    pub fn get_mut(&mut self, i: I) -> Option<&mut T> {
        self.1.get_mut(i.as_usize())
    }

    #[inline]
    pub fn split_around(&self, i: I) -> (&[T], &T, &[T]) {
        let i = i.as_usize();
        let p = self.1.as_ptr();
        let n = self.1.len();
        assert!(i < n);
        unsafe {
            (
                slice::from_raw_parts(p, i),
                &*p.add(i),
                slice::from_raw_parts(p.add(i + 1), n - i - 1),
            )
        }
    }

    #[inline]
    pub fn split_around_mut(&mut self, i: I) -> (&mut [T], &mut T, &mut [T]) {
        let i = i.as_usize();
        let p = self.1.as_mut_ptr();
        let n = self.1.len();
        assert!(i < n);
        unsafe {
            (
                slice::from_raw_parts_mut(p, i),
                &mut *p.add(i),
                slice::from_raw_parts_mut(p.add(i + 1), n - i - 1),
            )
        }
    }

    #[inline(always)]
    pub fn iter(&self) -> slice::Iter<T> {
        self.as_ui().iter()
    }

    #[inline(always)]
    pub fn enum_iter(&self) -> impl Iterator<Item = (I, &T)> {
        self.iter()
            .enumerate()
            .map(|(i, v)| (unsafe { I::from_usize_unchecked(i) }, v))
    }

    #[inline(always)]
    pub fn iter_mut(&mut self) -> slice::IterMut<T> {
        self.as_mut_ui().iter_mut()
    }

    #[inline(always)]
    pub fn enum_iter_mut(&mut self) -> impl Iterator<Item = (I, &mut T)> {
        self.iter_mut()
            .enumerate()
            .map(|(i, v)| (unsafe { I::from_usize_unchecked(i) }, v))
    }

    #[inline(always)]
    pub fn map<U>(&self, mut f: impl FnMut(I, &T) -> U) -> Array<I, U> {
        Array::new(self.len(), |i| f(i, unsafe { self.get_unchecked(i) }))
    }

    #[inline(always)]
    pub fn as_array(&self) -> Array<I, T>
    where
        T: Copy,
    {
        Array(Box::<[T]>::from(self.as_ui()), PhantomData)
    }

    #[inline(always)]
    pub fn map_update<U>(
        &self,
        ix: I,
        fi: impl FnOnce(&T) -> U,
        mut fr: impl FnMut(I, &T) -> U,
    ) -> Array<I, U> {
        assert!(self.len() > ix);
        let fi = mem::MaybeUninit::new(fi);
        self.map(move |i, v| {
            if i == ix {
                (unsafe { fi.as_ptr().read() })(v)
            } else {
                fr(i, v)
            }
        })
    }
}

impl<I: Index, T: PartialEq<U>, U> PartialEq<Slice<I, U>> for Slice<I, T> {
    #[inline(always)]
    fn eq(&self, other: &Slice<I, U>) -> bool {
        self.1 == other.1
    }
}

impl<I: Index, T: Eq> Eq for Slice<I, T> {}

impl<I: Index, T: Ord> PartialOrd for Slice<I, T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        let n = self.len();
        match n.cmp(&other.len()) {
            cmp::Ordering::Equal => {}
            r => {
                return Some(r);
            }
        }
        cmp::PartialOrd::partial_cmp(&self.1, &other.1)
    }
}

impl<I: Index, T: Ord> Ord for Slice<I, T> {
    #[inline]
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let n = self.len();
        match n.cmp(&other.len()) {
            cmp::Ordering::Equal => {}
            r => {
                return r;
            }
        }
        cmp::Ord::cmp(&self.1, &other.1)
    }
}

impl<I: Index, T: hash::Hash> hash::Hash for Slice<I, T> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, h: &mut H) {
        hash::Hash::hash_slice(self.as_ui(), h)
    }
}

impl<I: Index, T> ops::Index<I> for Slice<I, T> {
    type Output = T;
    #[inline(always)]
    fn index(&self, i: I) -> &T {
        &self.as_ui()[i.as_usize()]
    }
}

impl<I: Index, T> ops::IndexMut<I> for Slice<I, T> {
    #[inline(always)]
    fn index_mut(&mut self, i: I) -> &mut T {
        &mut self.as_mut_ui()[i.as_usize()]
    }
}

pub struct Array<I: Index, T>(Box<[T]>, PhantomData<fn(I) -> I>);

impl<I: Index, T: Clone> Clone for Array<I, T> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1)
    }

    #[inline]
    fn clone_from(&mut self, other: &Self) {
        self.0.clone_from(&other.0)
    }
}

impl<I: Index, T: fmt::Debug> fmt::Debug for Array<I, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl<I: Index, T> Array<I, T> {
    fn alloc(n: Length<I>) -> Box<[MaybeUninit<T>]> {
        let ni = n.as_usize();
        let mut v = Vec::with_capacity(ni);
        unsafe {
            v.set_len(ni);
        }
        v.into_boxed_slice()
    }

    #[inline]
    unsafe fn new_init(v: Box<[MaybeUninit<T>]>) -> Self {
        Self(Box::from_raw(Box::into_raw(v) as *mut _), PhantomData)
    }

    #[inline]
    fn into_init(self) -> Box<[MaybeUninit<T>]> {
        unsafe { Box::from_raw(Box::into_raw(self.0) as *mut _) }
    }

    #[inline]
    pub fn new_empty() -> Self {
        Self::new_box(Box::<[T]>::default())
    }

    #[inline]
    pub fn new(n: Length<I>, mut f: impl FnMut(I) -> T) -> Array<I, T> {
        let mut p = Self::alloc(n);
        unsafe {
            for (i, v) in p.iter_mut().enumerate() {
                v.as_mut_ptr().write(f(I::from_usize_unchecked(i)));
            }
            Self::new_init(p)
        }
    }

    #[inline]
    pub fn new_vec(v: Vec<T>) -> Array<I, T> {
        Self::new_box(v.into_boxed_slice())
    }

    #[inline]
    pub fn new_box(v: Box<[T]>) -> Array<I, T> {
        let _ = Length::<I>::new(v.len());
        Self(v, PhantomData)
    }

    #[inline]
    pub fn into_boxed_slice(self) -> Box<[T]> {
        self.0
    }

    #[inline(always)]
    pub fn map_into<U>(self, mut f: impl FnMut(I, T) -> U) -> Array<I, U> {
        if mem::size_of::<T>() != mem::size_of::<U>() {
            let p0 = self.into_init();
            let n = p0.len();
            unsafe {
                let mut p = Array::<I, U>::alloc(Length::new_unchecked(n));
                for i in 0..n {
                    p.get_unchecked_mut(i).as_mut_ptr().write(f(
                        I::from_usize_unchecked(i),
                        p0.get_unchecked(i).as_ptr().read(),
                    ))
                }
                Array::new_init(p)
            }
        } else {
            let mut p = self.into_init();
            unsafe {
                for i in 0..p.len() {
                    (p.get_unchecked_mut(i).as_mut_ptr() as *mut U).write(f(
                        I::from_usize_unchecked(i),
                        p.get_unchecked(i).as_ptr().read(),
                    ))
                }
                Array::new_init(Box::from_raw(Box::into_raw(p) as *mut [MaybeUninit<U>]))
            }
        }
    }
}

impl<I: Index, T> ops::Deref for Array<I, T> {
    type Target = Slice<I, T>;

    #[inline(always)]
    fn deref(&self) -> &Slice<I, T> {
        unsafe { Slice::at_unchecked(&*self.0) }
    }
}

impl<I: Index, T> ops::DerefMut for Array<I, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Slice<I, T> {
        unsafe { Slice::at_unchecked_mut(&mut *self.0) }
    }
}

impl<I: Index, T: PartialEq<U>, U> PartialEq<Array<I, U>> for Array<I, T> {
    #[inline(always)]
    fn eq(&self, other: &Array<I, U>) -> bool {
        **self == **other
    }

    #[inline(always)]
    fn ne(&self, other: &Array<I, U>) -> bool {
        **self != **other
    }
}

impl<I: Index, T: Eq> Eq for Array<I, T> {}

impl<I: Index, T: Ord> PartialOrd for Array<I, T> {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        cmp::PartialOrd::partial_cmp(&**self, &**other)
    }
}

impl<I: Index, T: Ord> Ord for Array<I, T> {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        cmp::Ord::cmp(&**self, &**other)
    }
}

impl<I: Index, T: hash::Hash> hash::Hash for Array<I, T> {
    #[inline(always)]
    fn hash<H: hash::Hasher>(&self, h: &mut H) {
        hash::Hash::hash_slice(&*self.0, h)
    }
}

impl<I: Index, T, X> ops::Index<X> for Array<I, T>
where
    Slice<I, T>: ops::Index<X>,
{
    type Output = <Slice<I, T> as ops::Index<X>>::Output;
    #[inline(always)]
    fn index(&self, i: X) -> &Self::Output {
        &(**self)[i]
    }
}

impl<I: Index, T, X> ops::IndexMut<X> for Array<I, T>
where
    Slice<I, T>: ops::IndexMut<X>,
{
    #[inline(always)]
    fn index_mut(&mut self, i: X) -> &mut Self::Output {
        &mut (**self)[i]
    }
}

impl<I: Index, T> IntoIterator for Array<I, T> {
    type Item = T;
    type IntoIter = vec::IntoIter<T>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        Vec::from(self.into_boxed_slice()).into_iter()
    }
}
