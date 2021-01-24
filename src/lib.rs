mod core;
mod common;
mod arm7;
mod memory;
mod coproc;

pub use crate::core::{
    ARMCore, ARMv4, Thumbv4
};

pub use crate::memory::{
    Mem32
};

pub use crate::coproc::{
    Coprocessor
};

pub use crate::arm7::{
    ARM7TDMI
};

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
