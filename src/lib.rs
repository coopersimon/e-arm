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

pub trait Clockable {
    fn clock(&mut self, cycles: usize);
}