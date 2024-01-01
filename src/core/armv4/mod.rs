/// ARMv4 Instruction Set

#[allow(dead_code)]
mod armv4_test;
pub mod instructions;
pub mod coproc;
pub mod execute;
pub mod compile;

use crate::core::decode;

pub use self::{
    instructions::ARMv4Instruction,
    execute::ARMv4,
    compile::ARMv4Compiler,
    coproc::{CoprocV4, CoprocV4Impl}
};

pub fn decode_arm(i: u32) -> ARMv4Instruction {
    decode::decode_instruction(i).into()
}
pub fn decode_thumb(i: u16) -> ARMv4Instruction {
    decode::decode_thumb(i).into()
}

/// Calculate the number of cycles needed for the multiply.
/// This depends on the status of the most significant bytes of the second operand.
/// Assume a full multiply takes 4 cycles.
/// If the most significant byte is all 0 or all 1, it can be reduced by 1 cycle.
/// Continue for the 2nd and 3rd most significant bytes.
const fn mul_cycles(op2: u32) -> usize {
    let leading = (op2.leading_zeros() | op2.leading_ones()) as usize;
    if leading == 32 { return 1; }
    (39 - leading) / 8
}

#[test]
fn test_mul_cycles() {
    assert_eq!(mul_cycles(0), 1);
    assert_eq!(mul_cycles(0xFFFF_FFFF), 1);
    assert_eq!(mul_cycles(0xFF), 1);
    assert_eq!(mul_cycles(0xFFFF_FF13), 1);
    assert_eq!(mul_cycles(0xFFFF), 2);
    assert_eq!(mul_cycles(0x1234), 2);
    assert_eq!(mul_cycles(0xFFFF_0123), 2);
    assert_eq!(mul_cycles(0xFF_FFFF), 3);
    assert_eq!(mul_cycles(0x54_1234), 3);
    assert_eq!(mul_cycles(0xFF75_9999), 3);
    assert_eq!(mul_cycles(0xFF88_8888), 3);
    assert_eq!(mul_cycles(0x1234_5678), 4);
    assert_eq!(mul_cycles(0x0200_0000), 4);
}