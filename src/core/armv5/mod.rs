/// ARMv5 Instruction Set

#[allow(dead_code)]
mod armv5_test;
pub mod instructions;
pub mod coproc;
pub mod execute;

use crate::core::decode::*;

pub use self::{
    instructions::ARMv5Instruction,
    execute::ARMv5,
    coproc::{CoprocV5, CoprocV5Impl}
};

pub fn decode_arm(i: u32) -> ARMv5Instruction {
    decode_instruction(i)
}

pub fn decode_thumb(i: u16) -> ARMv5Instruction {
    decode_thumb(i)
}

pub trait ARMCoreV5 {
    /// Reference a coprocessor mutably.
    fn mut_coproc_v5<'a>(&'a mut self, coproc: usize) -> Option<&'a mut CoprocV5Impl>;
}