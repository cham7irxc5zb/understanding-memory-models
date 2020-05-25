use crate::index::*;
use crate::util::*;
use std::hint::unreachable_unchecked;
use std::{fmt, mem, ops};

#[cold]
fn overflow() -> ! {
    panic!("view overflow")
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loc(usize);

impl Loc {
    const MAX: usize = 7;
    pub const N: usize = Self::MAX + 1;

    #[inline(always)]
    pub fn iter() -> impl Iterator<Item = Self> {
        (0..=Self::MAX).map(|v| Self(v))
    }
}

unsafe impl Index for Loc {
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

impl fmt::Debug for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocArray<T>([T; Loc::N]);

impl<T: fmt::Debug> fmt::Debug for LocArray<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T> ops::Index<Loc> for LocArray<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, ix: Loc) -> &T {
        unsafe { self.0.get_unchecked(ix.as_usize()) }
    }
}

impl<T> ops::IndexMut<Loc> for LocArray<T> {
    #[inline(always)]
    fn index_mut(&mut self, ix: Loc) -> &mut T {
        unsafe { self.0.get_unchecked_mut(ix.as_usize()) }
    }
}

impl<T> LocArray<T> {
    #[inline]
    pub fn new(mut f: impl FnMut(Loc) -> T) -> Self {
        let mut r = mem::MaybeUninit::uninit();
        let p = r.as_mut_ptr() as *mut T;
        for i in 0..=Loc::MAX {
            unsafe { p.add(i as usize).write(f(Loc(i))) };
        }
        Self(unsafe { r.assume_init() })
    }

    #[inline]
    pub fn map<U>(&self, mut f: impl FnMut(Loc, &T) -> U) -> LocArray<U> {
        LocArray::<U>::new(|i| f(i, &self[i]))
    }

    #[inline]
    pub fn map_update<U>(
        &self,
        ix: Loc,
        f: impl FnOnce(&T) -> U,
        mut g: impl FnMut(Loc, &T) -> U,
    ) -> LocArray<U> {
        let f = mem::MaybeUninit::new(f);
        LocArray::new(|i| {
            let v = &self[i];
            if i == ix {
                (unsafe { f.as_ptr().read() })(v)
            } else {
                g(i, v)
            }
        })
    }
}

define_index_type! {
    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub struct Val(u32);
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "={}", self.0)
    }
}

mod ts;
pub use ts::*;

mod view;
pub use view::*;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Message(Val, u32, View);

impl Message {
    #[inline]
    pub fn new(val: Val, is_update: bool, view: View) -> Self {
        Self(val, is_update as u32, view)
    }

    #[inline]
    pub fn val(self) -> Val {
        self.0
    }

    #[inline]
    pub fn is_update(self) -> bool {
        self.1 != 0
    }

    #[inline]
    pub fn set_update(&mut self) {
        self.1 = 1;
    }

    #[inline]
    pub fn set_not_update(&mut self) {
        self.1 = 0;
    }

    #[inline]
    pub fn view(self) -> View {
        self.2
    }

    #[inline]
    pub fn shift(&mut self, s: Shift) {
        self.2 = self.2.with_shift(s);
    }
}

impl Message {
    pub const SER: usize = 2 + ViewDiff::SER;

    #[inline]
    pub fn serialize_into(self, out: &mut Vec<u32>, base_view: View) {
        out.push_fast(self.val().0);
        out.push_fast(self.is_update() as u32);
        ViewDiff::saturating_sub(self.view(), base_view).serialize_into(out);
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum PromiseKind {
    New,
    Split,
}

#[derive(Copy, Clone, Debug)]
pub struct Shift {
    pub loc: Loc,
    pub t: Ts,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Memory {
    ixs: LocArray<usize>,
    all_msgs: Box<[Message]>,
}

impl Memory {
    pub fn new() -> Self {
        let mut all_msgs = Vec::with_capacity(Loc::N);
        for _ in Loc::iter() {
            all_msgs.push_fast(Message::new(Val(0), false, View::ZERO));
        }
        let all_msgs = all_msgs.into_boxed_slice();
        Self {
            ixs: LocArray::new(|i| i.as_usize()),
            all_msgs,
        }
    }

    fn _with_write(&self, loc: Loc, t: Ts, mut msg: Message, kind: PromiseKind) -> Self {
        let shift = Shift { loc, t };

        let mut ixs = LocArray::new(|_| 0);
        let mut all_msgs = Vec::with_capacity(self.all_msgs.len() + 1);

        for l in Loc::iter() {
            ixs[l] = all_msgs.len();
            let msgs = self[l].as_ui();
            if l == loc {
                let t = t.0.as_usize();
                let (left, mut right) = msgs.split_at(t);
                all_msgs.extend_from_slice_fast(left);
                if msg.is_update() {
                    if let Some(m) = left.last() {
                        msg =
                            Message::new(msg.val(), msg.is_update(), msg.view().with_max(m.view()));
                    }
                }
                all_msgs.push(msg);
                match kind {
                    PromiseKind::New => {
                        assert!(right.first().map(|m| !m.is_update()).unwrap_or(true));
                    }
                    PromiseKind::Split => {
                        let (m, r) = right.split_at(1);
                        right = r;
                        let mut m = m[0];
                        assert!(m.is_update() == msg.is_update() && m.view() == msg.view());
                        m.set_update();
                        all_msgs.push(m);
                    }
                }
                if let Some(m) = right.first() {
                    assert!(!m.is_update() || m.view().is_after(msg.view()));
                }
                all_msgs.extend_from_slice_fast(right);
            } else {
                for m in msgs {
                    all_msgs.push_fast(Message::new(
                        m.val(),
                        m.is_update(),
                        m.view().with_shift(shift),
                    ));
                }
            }
        }

        let all_msgs = all_msgs.into_boxed_slice();
        Self { ixs, all_msgs }
    }

    #[inline(always)]
    pub fn with_write(&self, loc: Loc, t: Ts, msg: Message, kind: PromiseKind) -> (Self, Shift) {
        (self._with_write(loc, t, msg, kind), Shift { loc, t })
    }
}

impl ops::Index<Loc> for Memory {
    type Output = Slice<Ts, Message>;

    #[inline(always)]
    fn index(&self, i: Loc) -> &Self::Output {
        let i0 = self.ixs[i];
        let i = i.as_usize();
        let i1 = if i == Loc::MAX {
            self.all_msgs.len()
        } else {
            self.ixs[unsafe { Loc::from_usize_unchecked(i + 1) }]
        };
        unsafe { Slice::at_unchecked(self.all_msgs.get_unchecked(i0..i1)) }
    }
}

impl<T> ops::Index<ops::RangeFrom<Ts>> for Slice<Ts, T> {
    type Output = Slice<TsDiff, T>;

    #[inline(always)]
    fn index(&self, i: ops::RangeFrom<Ts>) -> &Self::Output {
        Slice::at(&self.as_ui()[i.start.as_usize()..])
    }
}

impl fmt::Debug for Memory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut f = f.debug_map();
        for i in Loc::iter() {
            f.entry(&i, &self[i].as_ui());
        }
        f.finish()
    }
}
