/// ARMv4 Instruction Set

mod armv4_test;
pub mod instructions;
pub mod decode;
pub mod decodethumb;
pub mod execute;

use crate::common::u32::test_bit;

/// Make fsxc mask for manipulating PSR.
/// Creates a 32-bit mask based on the status of certain bits in the input value.
/// Bit 19: flags. Bits 24-31
/// Bit 18: status. Bits 16-23
/// Bit 17: extension. Bits 8-15
/// Bit 16: control. Bits 0-7
const fn fsxc_mask(from: u32) -> u32 {
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

/// Calculate the number of cycles needed for the multiply.
/// This depends on the status of the most significant bytes of the second operand.
/// Assume a full multiply takes 4 cycles.
/// If the most significant byte is all 0 or all 1, it can be reduced by 1 cycle.
/// Continue for the 2nd and 3rd most significant bytes.
const fn mul_cycles(op2: u32) -> usize {
    let leading_zeros = op2.leading_zeros();
    if leading_zeros == 0 {
        let leading_ones = op2.leading_ones();
        if leading_ones >= 24 {
            1
        } else if leading_ones >= 16 {
            2
        } else if leading_ones >= 8 {
            3
        } else {
            4
        }
    } else if leading_zeros >= 24 {
        1
    } else if leading_zeros >= 16 {
        2
    } else if leading_zeros >= 8 {
        3
    } else {
        4
    }
}
