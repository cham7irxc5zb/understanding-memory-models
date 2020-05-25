use std::{mem, ptr, slice};

pub struct Raw {
    ptr: ptr::NonNull<Entry>,
    mask: usize,
}

type Entry = (u64, usize);

#[derive(Copy, Clone, Default)]
#[repr(transparent)]
pub struct Slot(usize);

impl Raw {
    fn new_size(size: usize) -> Self {
        debug_assert!(size.is_power_of_two());
        unsafe {
            let ptr = alloc(size);
            let mask = size - 1;
            ptr::write_bytes(ptr.as_ptr(), 0, size);
            Self { ptr, mask }
        }
    }

    #[inline]
    pub fn new() -> Self {
        Self::new_size(8)
    }

    #[inline(always)]
    pub fn size(&self) -> usize {
        self.mask + 1
    }

    #[inline(always)]
    fn data(&self) -> (&[Entry], usize) {
        let mask = self.mask;
        let tab = unsafe { slice::from_raw_parts(self.ptr.as_ptr(), mask + 1) };
        (tab, mask)
    }

    #[inline(always)]
    fn data_mut(&mut self) -> (&mut [Entry], usize) {
        let mask = self.mask;
        let tab = unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr(), mask + 1) };
        (tab, mask)
    }
}

impl Default for Raw {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

unsafe impl Send for Raw {}
unsafe impl Sync for Raw {}

impl Clone for Raw {
    fn clone(&self) -> Self {
        let mask = self.mask;
        let size = mask + 1;
        unsafe {
            let ptr = alloc(size);
            ptr::copy_nonoverlapping(self.ptr.as_ptr(), ptr.as_ptr(), size);
            Self { ptr, mask }
        }
    }

    fn clone_from(&mut self, other: &Self) {
        let mask = other.mask;
        let size = mask + 1;
        unsafe {
            if self.mask != mask {
                dealloc(self.ptr, self.mask + 1);
                self.mask = mask;
                self.ptr = alloc(size);
            }
            ptr::copy_nonoverlapping(other.ptr.as_ptr(), self.ptr.as_ptr(), size);
        }
    }
}

impl Drop for Raw {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            dealloc(self.ptr, self.mask + 1);
        }
    }
}

impl Raw {
    pub const SENTINEL: u64 = 0;
    pub const NOT_SENTINEL: u64 = !Self::SENTINEL;

    #[inline]
    pub fn lookup_by<T>(
        &self,
        qh: u64,
        mut chk: impl FnMut(usize) -> Option<T>,
    ) -> Result<T, Slot> {
        let (tab, mask) = self.data();
        let mut slot = qh as usize & mask;
        loop {
            let (sh, sv) = unsafe { *tab.get_unchecked(slot) };
            if sh == Self::SENTINEL || h_lt(sh, qh, slot, mask) {
                return Err(Slot(slot));
            }
            if sh == qh {
                if let Some(r) = chk(sv) {
                    return Ok(r);
                }
            }
            slot = (slot + 1) & mask;
        }
    }

    pub fn lookup_for_insert(&self, qh: u64) -> Slot {
        let (tab, mask) = self.data();
        let mut slot = qh as usize & mask;
        loop {
            let sh = unsafe { tab.get_unchecked(slot).0 };
            if sh == Self::SENTINEL || h_lt(sh, qh, slot, mask) {
                return Slot(slot);
            }
            slot = (slot + 1) & mask;
        }
    }

    pub unsafe fn insert(&mut self, slot: Slot, qh: u64, qv: usize) {
        let (tab, mask) = self.data_mut();
        let Slot(mut slot) = slot;
        let mut q = (qh, qv);

        loop {
            let s = tab.get_unchecked_mut(slot);
            mem::swap(s, &mut q);
            if q.0 == Self::SENTINEL {
                break;
            }
            slot = (slot + 1) & mask;
        }
    }

    #[inline]
    fn insert_ordered(&mut self, qh: u64, qv: usize) {
        let (tab, mask) = self.data_mut();
        let mut slot = qh as usize & mask;
        loop {
            let s = unsafe { tab.get_unchecked_mut(slot) };
            if s.0 == Self::SENTINEL {
                *s = (qh, qv);
                break;
            }
            slot = (slot + 1) & mask;
        }
    }

    pub fn grow(&mut self) {
        let (old_tab, old_mask) = self.data();
        let mut new = Self::new_size(old_mask * 2 + 2);

        let mid = old_tab
            .iter()
            .enumerate()
            .find(|(_, s)| s.0 == Self::SENTINEL)
            .map(|(i, _)| i)
            .unwrap();

        let mut old_slot = mid;
        loop {
            old_slot = (old_slot + 1) & old_mask;
            if old_slot == mid {
                break;
            }
            let (qh, qv) = unsafe { *old_tab.get_unchecked(old_slot) };
            if qh != Self::SENTINEL {
                new.insert_ordered(qh, qv);
            }
        }

        *self = new;
    }
}

impl Raw {
    #[inline]
    pub fn maybe_grow(&mut self, elements: usize, fix_vh: u64, fix_slot: &mut Slot) {
        if self.mask - (self.mask >> 2) <= elements {
            self.grow();
            *fix_slot = self.lookup_for_insert(fix_vh);
        }
    }
}

#[inline(always)]
fn h_lt(a: u64, b: u64, slot: usize, mask: usize) -> bool {
    let a = slot.wrapping_sub(a as usize) & mask;
    let b = slot.wrapping_sub(b as usize) & mask;
    a < b
}

unsafe fn alloc(size: usize) -> ptr::NonNull<Entry> {
    let mut r = Vec::<mem::MaybeUninit<Entry>>::with_capacity(size);
    let ptr = ptr::NonNull::new_unchecked(r.as_mut_ptr() as *mut Entry);
    mem::forget(r);
    ptr
}

unsafe fn dealloc(p: ptr::NonNull<Entry>, size: usize) {
    mem::drop(Vec::from_raw_parts(p.as_ptr(), 0, size))
}
