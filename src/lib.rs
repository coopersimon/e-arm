mod core;
mod common;
mod arm7;
mod memory;
mod coproc;
mod debugger;

pub use crate::core::{
    ARMCore, ARMv4, ARMv4Decode, Thumbv4Decode, ARMv4Instruction
};

pub use crate::memory::{
    Mem32, MemCycleType
};

pub use crate::coproc::{
    Coprocessor
};

pub use crate::arm7::{
    ARM7TDMI
};

pub use crate::debugger::*;

/// Exceptions that can be triggered in the processor.
/// Listed in priority order.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Exception {
    Reset,
    DataAbort,
    FastInterrupt,
    Interrupt,
    PrefetchAbort,
    SoftwareInterrupt,
    UndefinedInstruction,
}
