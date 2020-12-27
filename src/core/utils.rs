/// Additional utils specific to ARM.

use crate::common::test_bit;

/// Make fsxc mask for manipulating PSR.
/// Creates a 32-bit mask based on the status of certain bits in the input value.
/// Bit 19: flags. Bits 24-31
/// Bit 18: status. Bits 16-23
/// Bit 17: extension. Bits 8-15
/// Bit 16: control. Bits 0-7
pub const fn fsxc_mask(from: u32) -> u32 {
    let mut mask = 0;
    if test_bit(from, 19) {
        mask |= 0xFF00_0000;
    }
    if test_bit(from, 18) {
        mask |= 0x00FF_0000;
    }
    if test_bit(from, 17) {
        mask |= 0x0000_FF00;
    }
    if test_bit(from, 16) {
        mask |= 0x0000_00FF;
    }
    mask
}