
use crate::{
    common::{
        u32::*,
        u64, lo_64, hi_64, make_64
    },
    core::{ARMCore, CPSR, ARMv4},
    memory::{Mem32, MemCycleType}
};

/// Execution of ARMv5 instructions.
pub trait ARMv5<M: Mem32<Addr = u32>>: ARMv4<M> {

}
