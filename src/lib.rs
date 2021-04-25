mod core;
mod common;
mod arm7;
mod memory;
mod coproc;
mod debugger;
mod jit;

pub use crate::core::{
    ARMCore, ARMv4, ARMv4Instruction, CPSR, SwiHook, decode_arm_v4, decode_thumb_v4
};

pub use crate::memory::{
    Mem32, MemCycleType
};

pub use crate::coproc::{
    Coprocessor, CoprocImpl
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