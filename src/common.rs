/// Common bit and byte manip.

pub mod u16 {
    /// Set the nth bit.
    #[inline]
    pub const fn bit(n: usize) -> u16 {
        1 << n
    }

    /// Set all bits between the top and bottom (inclusive).
    #[inline]
    pub const fn bits(mut bottom: usize, top: usize) -> u16 {
        let mut out = 0;
        while bottom <= top {
            out |= bit(bottom);
            bottom += 1;
        }
        return out;
    }

    /// Check if the nth bit is set.
    #[inline]
    pub const fn test_bit(val: u16, n: usize) -> bool {
        (val & bit(n)) != 0
    }
}

pub mod u32 {
    /// Set the nth bit.
    #[inline]
    pub const fn bit(n: usize) -> u32 {
        1 << n
    }

    /// Set all bits between the top and bottom (inclusive).
    #[inline]
    pub const fn bits(mut bottom: usize, top: usize) -> u32 {
        let mut out = 0;
        while bottom <= top {
            out |= bit(bottom);
            bottom += 1;
        }
        return out;
    }

    /// Check if the nth bit is set.
    #[inline]
    pub const fn test_bit(val: u32, n: usize) -> bool {
        (val & bit(n)) != 0
    }

    /// Get the low half of a word.
    #[inline]
    pub const fn lo(val: u32) -> u16 {
        val as u16
    }

    /// Get the high half of a word.
    #[inline]
    pub const fn hi(val: u32) -> u16 {
        (val >> 16) as u16
    }
}

pub mod u64 {
    /// Set the nth bit.
    #[inline]
    pub const fn bit(n: usize) -> u64 {
        1 << n
    }

    /// Set all bits between the top and bottom (inclusive).
    /*pub const fn bits(mut bottom: usize, top: usize) -> u64 {
        let mut out = 0;
        while bottom <= top {
            out |= bit(bottom);
            bottom += 1;
        }
        return out;
    }*/

    /// Check if the nth bit is set.
    #[inline]
    pub const fn test_bit(val: u64, n: usize) -> bool {
        (val & bit(n)) != 0
    }

    /// Make a 64-bit value from two 32-bit values (high to low).
    #[inline]
    pub const fn make(hi: u32, lo: u32) -> u64 {
        ((hi as u64) << 32) | (lo as u64)
    }

    /// Get the low word of a doubleword.
    #[inline]
    pub const fn lo(val: u64) -> u32 {
        val as u32
    }

    /// Get the high word of a doubleword.
    #[inline]
    pub const fn hi(val: u64) -> u32 {
        (val >> 32) as u32
    }
}
