mod core;
mod memory;

mod arm7;
mod arm9;
mod debugger;

mod common;

pub use crate::core::{
    ARMCore, ARMCoreJIT, CPSR, SwiHook
};
pub use crate::core::armv4 as armv4;
pub use crate::core::armv5 as armv5;

pub use crate::memory::{
    Mem32, MemCycleType
};

pub use crate::arm7::{
    ARM7TDMI
};
pub use crate::arm9::{
    ARM9ES, ARM9Mem
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