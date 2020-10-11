/// Core traits and types for ARM processors (data access).

mod armv4;
mod armv4_test;

use bitflags::bitflags;
use crate::common::{bit, bits};

pub use armv4::ARMv4;

bitflags! {
    #[derive(Default)]
    pub struct CPSR: u32 {
        const N = bit(31);
        const Z = bit(30);
        const C = bit(29);
        const V = bit(28);
        const I = bit(7);
        const F = bit(6);
        const T = bit(5);
        const MODE = bits(0, 4);
    }
}

impl CPSR {
    fn carry(self) -> u32 {
        if self.contains(CPSR::C) {
            1
        } else {
            0
        }
    }
}

pub type SPSR = CPSR;

pub trait ARMCore {
    fn read_reg(&self, n: usize) -> u32;
    fn write_reg(&mut self, n: usize, data: u32);

    fn read_cpsr(&self) -> CPSR;
    fn write_cpsr(&mut self, data: CPSR);

    fn set_mode(&mut self, mode: Mode);

    fn add_cycles(&mut self, cycles: usize);
}

#[derive(PartialEq, Eq)]
pub enum Mode {
    USR,    // User
    FIQ,    // Fast Interrupt
    IRQ,    // Interrupt
    UND,    // Undefined
    SVC     // Supervisor
    // Abort?
    // System?
}