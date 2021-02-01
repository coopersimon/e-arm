/// Core traits and types for ARM processors (data access).

mod armv4;
mod armv4_test;
mod armv4thumb;
mod utils;

use bitflags::bitflags;
use crate::common::u32::{bit, bits};
use crate::coproc::Coprocessor;
use crate::memory::Mem32;

pub use armv4::ARMv4;
pub use armv4thumb::Thumbv4;

pub mod constants {
    pub const SP_REG: usize = 13;
    pub const LINK_REG: usize = 14;
    pub const PC_REG: usize = 15;

    /// 32-bit ARM instruction size in bytes.
    pub const I_SIZE: u32 = 4;
    /// 16-bit THUMB instruction size in bytes.
    pub const T_SIZE: u32 = 2;
}

bitflags! {
    #[derive(Default)]
    pub struct CPSR: u32 {
        const FLAGS = bits(24, 31);
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

    pub fn mode(self) -> Mode {
        use Mode::*;
        match self & CPSR::MODE {
            CPSR::FIQ => FIQ,
            CPSR::IRQ => IRQ,
            CPSR::UND => UND,
            CPSR::SVC => SVC,
            CPSR::ABT => ABT,
            _ => USR,
        }
    }

    pub fn instr_size(&self) -> u32 {
        use constants::*;
        if self.contains(CPSR::T) {T_SIZE} else {I_SIZE}
    }
}

pub type SPSR = CPSR;

pub trait ARMCore<M: Mem32> {
    fn read_reg(&self, n: usize) -> u32;
    fn write_reg(&mut self, n: usize, data: u32);
    /// Directly modify the PC.
    /// 
    /// Keep in mind the PC will be incremented after the execution completes,
    /// so the destination must be offset by the branch instruction size.
    fn do_branch(&mut self, dest: u32);

    /// For STM when force usr-reg access.
    fn read_usr_reg(&self, n: usize) -> u32;
    /// For LDM when force usr-reg access.
    /// Never use for writing PC.
    fn write_usr_reg(&mut self, n: usize, data: u32);

    /// Read from CPSR.
    fn read_cpsr(&self) -> CPSR;
    /// Write to CPSR.
    /// This can change the mode.
    fn write_cpsr(&mut self, data: CPSR);
    /// Write NZCV flags. This will only change the top 8 bits.
    fn write_flags(&mut self, flags: CPSR);

    fn read_spsr(&self) -> CPSR;
    fn write_spsr(&mut self, data: CPSR);

    fn trigger_exception(&mut self, exception: crate::Exception);
    fn return_from_exception(&mut self);

    /// Called when the next fetch is from non-sequential memory.
    /// Usually called from store instructions.
    fn next_fetch_non_seq(&mut self);

    fn ref_mem<'a>(&'a mut self) -> &'a mut M;
    fn ref_coproc<'a>(&'a mut self, coproc: usize) -> Option<&'a mut Box<dyn Coprocessor>>;
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
