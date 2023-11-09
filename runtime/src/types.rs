#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UniformType {
    /// 63-bit integer. The lowest bit is always 0.
    Int,

    /// Pointer to some struct (or heterogenous array, if one prefers) whose fields are all in
    /// uniform representation.
    ///
    /// Lowest 3 bits are 001
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
    /// Lowest 3 bits are 011.
    ///
    /// # Note:
    /// See the note for [UniformType::SPtr].
    PPtr,

    /// Embedding of a primitive taking 4 or less bytes. For example, a 32-bit integer, single
    /// precision floating point number, etc. Last 3 bits are 100.
    Embed,
}

impl UniformType {
    pub fn from_bits(input: usize) -> UniformType {
        match input & UNIFORM_TAG_MASK {
            0b000 => UniformType::Int,
            0b001 => UniformType::SPtr,
            0b011 => UniformType::PPtr,
            0b100 => UniformType::Embed,
            _ => unreachable!(),
        }
    }
}

pub const UNIFORM_TAG_MASK: usize = 0b111;
pub const POINTER_MASK: usize = !0b111;

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

