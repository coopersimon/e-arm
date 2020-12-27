/// Core traits and types for ARM processors (data access).

mod armv4;
mod armv4_test;
mod utils;

use bitflags::bitflags;
use crate::common::{bit, bits};

pub use armv4::ARMv4;

pub mod constants {
    pub const LINK_REG: usize = 14;
    pub const PC_REG: usize = 15;
}

bitflags! {
    #[derive(Default)]
    pub struct CPSR: u32 {
        const N = bit(31);      // Negative
        const Z = bit(30);      // Zero
        const C = bit(29);      // Carry
        const V = bit(28);      // Overflow
        const I = bit(7);       // IRQs disabled
        const F = bit(6);       // FIQs disabled
        const T = bit(5);       // THUMB instruction set enabled

        const MODE = bits(0, 4);    // Interrupt mode mask
        const USR = 0x10;   // User
        const FIQ = 0x11;   // Fast interrupt
        const IRQ = 0x12;   // Interrupt
        const SVC = 0x13;   // Supervisor
        const ABT = 0x17;   // Abort
        const UND = 0x1B;   // Undefined
        const SYS = 0x1F;   // System
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

    pub fn set_mode(&mut self, mode: Mode) {
        self.remove(CPSR::MODE);
        self.insert(mode.into());
    }
}

pub type SPSR = CPSR;

pub trait ARMCore {
    fn read_reg(&self, n: usize) -> u32;
    fn write_reg(&mut self, n: usize, data: u32);

    fn read_cpsr(&self) -> CPSR;
    fn write_cpsr(&mut self, data: CPSR);

    fn read_spsr(&self) -> CPSR;
    fn write_spsr(&mut self, data: CPSR);

    fn trigger_exception(&mut self, exception: Exception);
    fn return_from_exception(&mut self);

    fn add_cycles(&mut self, cycles: usize);
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    USR,    // User
    FIQ,    // Fast Interrupt
    IRQ,    // Interrupt
    UND,    // Undefined
    SVC,    // Supervisor
    ABT,    // Abort
    // System?
}

impl From<Mode> for CPSR {
    fn from(mode: Mode) -> Self {
        use Mode::*;
        match mode {
            USR => CPSR::USR,
            FIQ => CPSR::FIQ,
            IRQ => CPSR::IRQ,
            UND => CPSR::UND,
            SVC => CPSR::SVC,
            ABT => CPSR::ABT,
        }
    }
}

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