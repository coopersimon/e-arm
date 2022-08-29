/// Core traits and types for ARM processors (data access).

pub mod armv4;
pub mod armv5;
pub mod decode;
mod jit;

#[allow(dead_code)]
mod test_utils;

use std::fmt;
use bitflags::bitflags;
use crate::common::u32::{bit, bits};
use crate::memory::{Mem32, MemCycleType};
use crate::armv4::CoprocV4;

pub use jit::*;

pub use decode::*;

pub use armv5::{
    instructions::ARMv5Instruction,
    execute::ARMv5,
    coproc::*
};

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
        const Q = bit(27);      // Sticky overflow / Saturate (ARMv5)

        const BLI = bit(8);     // Temporary I stack for Thumb BLs
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

/// Top-level ARM processor public use.
/// 
/// Call `step` in a loop to drive the ARM processor.
pub trait ARMDriver {
    /// Advance a single instruction through the pipeline.
    /// Will always fetch a new instruction,
    /// however it may not always execute one.
    /// 
    /// Returns how many cycles passed.
    /// 
    /// Internally clocks the memory bus.
    fn step(&mut self) -> usize;
}

/// Internal methods to access and modify internal ARM core.
pub trait ARMCore<M: Mem32<Addr = u32>> {
    /// Read a single general purpose register.
    /// 
    /// Can only access registers available in the current mode.
    fn read_reg(&self, n: usize) -> u32;
    /// Write a single general purpose register.
    /// 
    /// Can only access registers available in the current mode.
    fn write_reg(&mut self, n: usize, data: u32);
    /// Write back a single general purpose register
    /// after loading from memory.
    /// 
    /// Can only access registers available in the current mode.
    /// 
    /// Involves the writeback pipeline stage, and therefore may
    /// take an extra cycle. Returns the number of immediate cycles
    /// used.
    fn writeback_reg(&mut self, n: usize, data: u32) -> usize;

    /// Directly modify the PC.
    /// 
    /// Keep in mind the PC will be incremented after the execution completes,
    /// so the destination must be offset by the branch instruction size.
    fn do_branch(&mut self, dest: u32);
    /// Call a subroutine.
    /// 
    /// This is the entry point for JIT compiled code,
    /// on compatible processors.
    /// 
    /// The `i_size_offset` should be set to the callee's i_size.
    /// This way the addrs can be adjusted for the auto PC increment.
    fn call_subroutine(&mut self, dest: u32, i_size_offset: u32);

    /// Clock and handle interrupts.
    fn clock(&mut self, cycles: usize);

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

    // Exceptions
    fn reset(&mut self);
    fn interrupt(&mut self);
    fn fast_interrupt(&mut self);
    fn software_exception(&mut self);
    fn undefined_exception(&mut self);
    fn return_from_exception(&mut self);

    /// If system calls are implemented in rust, this should be called.
    /// 
    /// Returns true if hook is present.
    fn try_swi_hook(&mut self, comment: u32) -> bool;

    /// Called when the next fetch is from non-sequential memory.
    /// Usually called from store instructions.
    fn next_fetch_non_seq(&mut self);

    /// Reference the memory bus immutably.
    fn ref_mem<'a>(&'a self) -> &'a M;
    /// Reference the memory bus mutably.
    fn mut_mem<'a>(&'a mut self) -> &'a mut M;
    /// Reference a coprocessor mutably.
    fn mut_coproc<'a>(&'a mut self, coproc: usize) -> Option<&'a mut dyn CoprocV4>;

    // Memory
    fn load_byte(&mut self, cycle: MemCycleType, addr: u32) -> (u8, usize) {
        self.mut_mem().load_byte(cycle, addr)
    }
    fn store_byte(&mut self, cycle: MemCycleType, addr: u32, data: u8) -> usize {
        self.mut_mem().store_byte(cycle, addr, data)
    }
    /// Load a halfword, force aligning the address and rotating the result.
    fn load_halfword(&mut self, cycle: MemCycleType, addr: u32) -> (u16, usize) {
        let (data, cycles) = self.mut_mem().load_halfword(cycle, addr & 0xFFFF_FFFE);
        (data.rotate_right((addr & 1) * 8), cycles)
    }
    /// Store a halfword, force aligning the address.
    fn store_halfword(&mut self, cycle: MemCycleType, addr: u32, data: u16) -> usize {
        self.mut_mem().store_halfword(cycle, addr & 0xFFFF_FFFE, data)
    }
    /// Load a halfword, force aligning the address and rotating the result.
    fn load_word(&mut self, cycle: MemCycleType, addr: u32) -> (u32, usize) {
        let (data, cycles) = self.mut_mem().load_word(cycle, addr & 0xFFFF_FFFC);
        (data.rotate_right((addr & 3) * 8), cycles)
    }
    /// Load a word, force aligning the address.
    /// Used for load multiple.
    fn load_word_force_align(&mut self, cycle: MemCycleType, addr: u32) -> (u32, usize) {
        let (data, cycles) = self.mut_mem().load_word(cycle, addr & 0xFFFF_FFFC);
        (data, cycles)
    }
    /// Store a word, force aligning the address.
    fn store_word(&mut self, cycle: MemCycleType, addr: u32, data: u32) -> usize {
        self.mut_mem().store_word(cycle, addr & 0xFFFF_FFFC, data)
    }
}

/// When SWI is called, a function of this type can be called to handle it outside the interpreter.
/// 
/// The first argument is the SWI comment. The second arg is the memory interface. The remaining args are r0-r3.
/// 
/// It will clock the memory internally.
/// 
/// It returns new values for r0, r1, and r3.
pub type SwiHook<M> = fn(u32, &mut M, &[u32; 4]) -> [u32; 3];

/// ARM condition codes.
#[derive(Clone, Copy, PartialEq)]
pub enum ARMCondition {
    EQ, // Z set
    NE, // Z clear
    CS, // C set
    CC, // C clear
    MI, // N set
    PL, // N clear
    VS, // V set
    VC, // V clear
    HI, // C set and Z clear
    LS, // C clear or Z set
    GE, // N xnor V
    LT, // N xor V
    GT, // Z clear and (N xnor V)
    LE, // Z set or (N xor V)
    AL, // Always
    NV, // Never (reserved)
}

impl fmt::Display for ARMCondition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ARMCondition::*;
        match self {
            EQ => write!(f, "EQ"),
            NE => write!(f, "NE"),
            CS => write!(f, "CS"),
            CC => write!(f, "CC"),
            MI => write!(f, "MI"),
            PL => write!(f, "PL"),
            VS => write!(f, "VS"),
            VC => write!(f, "VC"),
            HI => write!(f, "HI"),
            LS => write!(f, "LS"),
            GE => write!(f, "GE"),
            LT => write!(f, "LT"),
            GT => write!(f, "GT"),
            LE => write!(f, "LE"),
            AL | NV => write!(f, ""),
        }
    }
}

impl ARMCondition {
    fn eval<M: Mem32<Addr = u32>, A: ARMCore<M>>(self, core: &mut A) -> bool {
        use ARMCondition::*;
        match self {
            EQ => core.read_cpsr().contains(CPSR::Z),
            NE => !core.read_cpsr().contains(CPSR::Z),
            CS => core.read_cpsr().contains(CPSR::C),
            CC => !core.read_cpsr().contains(CPSR::C),
            MI => core.read_cpsr().contains(CPSR::N),
            PL => !core.read_cpsr().contains(CPSR::N),
            VS => core.read_cpsr().contains(CPSR::V),
            VC => !core.read_cpsr().contains(CPSR::V),
            HI => {
                let cpsr = core.read_cpsr();
                cpsr.contains(CPSR::C) && !cpsr.contains(CPSR::Z)
            },
            LS => {
                let cpsr = core.read_cpsr();
                !cpsr.contains(CPSR::C) || cpsr.contains(CPSR::Z)
            },
            GE => {
                let cpsr = core.read_cpsr();
                cpsr.contains(CPSR::N) == cpsr.contains(CPSR::V)
            },
            LT => {
                let cpsr = core.read_cpsr();
                cpsr.contains(CPSR::N) != cpsr.contains(CPSR::V)
            },
            GT => {
                let cpsr = core.read_cpsr();
                !cpsr.contains(CPSR::Z) && (cpsr.contains(CPSR::N) == cpsr.contains(CPSR::V))
            },
            LE => {
                let cpsr = core.read_cpsr();
                cpsr.contains(CPSR::Z) || (cpsr.contains(CPSR::N) != cpsr.contains(CPSR::V))
            },
            AL | NV => true,
        }
    }
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
