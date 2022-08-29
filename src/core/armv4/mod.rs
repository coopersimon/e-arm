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
    let leading = op2.leading_zeros() | op2.leading_ones();
    if leading >= 24 {
        1
    } else if leading >= 16 {
        2
    } else if leading >= 8 {
        3
    } else {
        4
    }
}
