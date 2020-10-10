/// Common bit manip.

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