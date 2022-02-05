/// Decoding ARM instructions.
/// 
/// Decoding produces an ARM instruction that may not be
/// supported by a certain version - in that case, the
/// instruction is undefined.

mod arm;
mod thumb;

use crate::{
    core::ARMCondition,
};

pub use self::arm::decode_instruction;
pub use thumb::decode_thumb;

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
        0xF => NV,  // reserved
        _ => unreachable!()
    }
}
