/// Common bit and byte manip.

/// Set the nth bit.
pub const fn bit(n: usize) -> u32 {
    1 << n
}

/// Set the nth bit of a 64-bit value.
pub const fn bit_64(n: usize) -> u64 {
    1 << n
}

/// Set all bits between the top and bottom (inclusive).
pub const fn bits(mut bottom: usize, top: usize) -> u32 {
    let mut out = 0;
    while bottom <= top {
        out |= bit(bottom);
        bottom += 1;
    }
    return out;
}

/// Check if the nth bit is set.
pub const fn test_bit(val: u32, n: usize) -> bool {
    (val & bit(n)) != 0
}

/// Check if the nth bit is set of a 64-bit value.
pub const fn test_bit_64(val: u64, n: usize) -> bool {
    (val & bit_64(n)) != 0
}

/// Construct a halfword from bytes (high to low).
pub const fn make_16(byte_1: u8, byte_0: u8) -> u16 {
    ((byte_1 as u16) << 8) |
    (byte_0 as u16)
}

/// Deconstruct a halfword into bytes (high to low).
pub const fn from_16(val: u16) -> [u8; 2] {
    [
        (val >> 8) as u8,
        val as u8
    ]
}

/// Construct a word from bytes (high to low).
pub const fn make_32(byte_3: u8, byte_2: u8, byte_1: u8, byte_0: u8) -> u32 {
    ((byte_3 as u32) << 24) |
    ((byte_2 as u32) << 16) |
    ((byte_1 as u32) << 8) |
    (byte_0 as u32)
}

/// Deconstruct a word into bytes (high to low).
pub const fn from_32(val: u32) -> [u8; 4] {
    [
        (val >> 24) as u8,
        (val >> 16) as u8,
        (val >> 8) as u8,
        val as u8
    ]
}

/// Make a 64-bit value from two 32-bit values (high to low).
pub const fn make_64(hi: u32, lo: u32) -> u64 {
    ((hi as u64) << 32) | (lo as u64)
}

/// Get the low word of a doubleword.
pub const fn lo_64(val: u64) -> u32 {
    val as u32
}

/// Get the high word of a doubleword.
pub const fn hi_64(val: u64) -> u32 {
    (val >> 32) as u32
}