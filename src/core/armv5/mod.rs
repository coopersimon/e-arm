/// ARMv5 Instruction Set

#[allow(dead_code)]
mod armv5_test;
pub mod instructions;
pub mod coproc;
pub mod execute;

use coproc::CoprocV5Impl;

pub trait ARMCoreV5 {
    /// Reference a coprocessor mutably.
    fn mut_coproc_v5<'a>(&'a mut self, coproc: usize) -> Option<&'a mut CoprocV5Impl>;
}