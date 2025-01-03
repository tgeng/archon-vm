use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

// TODO: I need to figure out a way to force function pointers to be 4-byte aligned. This is true
//  on 64-bit ARM as far as I know but may not be true on x86.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UniformType {
    /// 63-bit integer or embedding of primitives that are smaller than a machine word (8 bytes
    /// since we only support 64-bit machines). An example of the latter is a F32, which takes only
    /// 4 bytes.
    ///
    /// The lowest bit is always 0.
    Raw,

    /// Pointer to some struct (or heterogenous array, if one prefers) whose fields are all in
    /// uniform representation.
    ///
    /// Lowest 2 bits are 01
    ///
    /// # Note:
    /// 1. The -1 word contains the byte length of this struct. This size is needed to check effect
    ///    argument equality at runtime. It should be possible to leverage the information from the
    ///    memory allocator to avoid storing this size in future.
    /// 2. The length is not needed by any language features because all accesses are statically
    ///    verified. If an unknown length array is used, typically user program would use a sigma
    ///    type consisting of the length and the array.
    SPtr,

    /// Pointer to an array containing non-pointers. Such a pointer may point to a single raw
    /// function, an 64-bit integer or a double precision floating point number. Or it may points
    /// to an array of such values. Also, it may point to a string.
    ///
    /// Lowest 2 bits are 11.
    ///
    /// # Note:
    /// See the note for [UniformType::SPtr].
    PPtr,
}

impl UniformType {
    pub fn from_bits(input: usize) -> UniformType {
        match input & UNIFORM_TAG_MASK {
            0b01 => UniformType::SPtr,
            0b11 => UniformType::PPtr,
            _ => UniformType::Raw,
        }
    }
    pub fn to_uniform_sptr<T>(ptr: *const T) -> usize {
        ptr as usize | 0b01
    }

    pub fn to_uniform_pptr<T>(ptr: *const T) -> usize {
        ptr as usize | 0b11
    }
}

pub const UNIFORM_TAG_MASK: usize = 0b11;
pub const POINTER_MASK: usize = !0b11;

pub trait UniformPtr<T> {
    fn to_normal_ptr(self) -> T;
}

impl UniformPtr<*mut usize> for *mut usize {
    fn to_normal_ptr(self) -> *mut usize {
        ((self as usize) & POINTER_MASK) as *mut usize
    }
}

impl UniformPtr<*const usize> for *const usize {
    fn to_normal_ptr(self) -> *const usize {
        ((self as usize) & POINTER_MASK) as *mut usize
    }
}

impl UniformPtr<*const usize> for usize {
    fn to_normal_ptr(self) -> *const usize {
        (self & POINTER_MASK) as *mut usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(C, align(8))]
pub struct UPtr<T>(*const u8, PhantomData<T>);

impl<'a, T> UPtr<&'a T> {
    pub fn as_uniform(&self) -> usize {
        self.0 as usize
    }
}

impl<'a, T> Deref for UPtr<&'a T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            let ptr = self.0 as usize;
            let ptr = ptr & POINTER_MASK;
            &*(ptr as *const T)
        }
    }
}

impl<'a, T> Deref for UPtr<&'a mut T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            let ptr = self.0 as usize;
            let ptr = ptr & POINTER_MASK;
            &*(ptr as *const T)
        }
    }
}

impl<'a, T> DerefMut for UPtr<&'a mut T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            let ptr = self.0 as usize;
            let ptr = ptr & POINTER_MASK;
            &mut *(ptr as *mut T)
        }
    }
}
