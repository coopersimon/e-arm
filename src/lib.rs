mod core;
mod common;
mod arm7;
mod memory;
mod debugger;

pub use crate::core::{
    ARMCore, ARMCoreJIT, CPSR, SwiHook, CoprocV4Impl,
    ARMv4, ARMv4Instruction, CoprocV4,
    ARMv5, ARMv5Instruction, CoprocV5,
    decode_arm_v4, decode_thumb_v4, decode_arm_v5, decode_thumb_v5
};

pub use crate::memory::{
    Mem32, MemCycleType
};

pub use crate::arm7::{
    ARM7TDMI
};

pub use crate::debugger::*;

/// Exceptions that come from external lines.
pub enum ExternalException {
    /// Reset
    RST,
    /// Fast Interrupt
    FIQ,
    /// Interrupt
    IRQ
}