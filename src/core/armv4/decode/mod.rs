mod arm;
mod thumb;

use crate::{
    common::u32::test_bit,
    core::ARMCondition
};

pub use self::arm::decode_instruction as decode_arm_v4;
pub use thumb::decode_thumb as decode_thumb_v4;

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

/// Decode the condition from the instruction.
fn decode_cond(cond_bits: u32) -> ARMCondition {
    use ARMCondition::*;
    match cond_bits {
        0x0 => EQ,
        0x1 => NE,
        0x2 => CS,
        0x3 => CC,
        0x4 => MI,
        0x5 => PL,
        0x6 => VS,
        0x7 => VC,
        0x8 => HI,
        0x9 => LS,
        0xA => GE,
        0xB => LT,
        0xC => GT,
        0xD => LE,
        0xE => AL,
        0xF => AL,  // reserved.
        _ => unreachable!()
    }
}
