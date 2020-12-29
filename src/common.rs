/// Common bit and byte manip.

/// Set the nth bit.
pub const fn bit(n: usize) -> u32 {
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