/// ARM9E-S processor.

use crate::core::{
    constants::*, Mode, CPSR, SPSR, SwiHook,
    ARMDriver, ARMCore,
    armv4::{
        ARMv4, CoprocV4
    },
    armv5::{
        ARMCoreV5, ARMv5, CoprocV5, CoprocV5Impl, decode_arm, decode_thumb
    }
};
use crate::common::u32;
use crate::memory::{
    Mem32, MemCycleType
};
use crate::{
    ExternalException,
    Debugger, CPUState
};

pub trait ARM9Mem: Mem32<Addr = u32> {
    fn mut_cp15<'a>(&'a mut self) -> &'a mut dyn CoprocV5;
}
const NUM_COPROCS: usize = 15;

/// Builder class for the ARM9.
/// 
/// Call `build` to finish building.
pub struct ARM9ESBuilder<M: ARM9Mem> {
    mem:        Box<M>,
    coproc:     [Option<CoprocV5Impl>; NUM_COPROCS],
    swi_hook:   Option<SwiHook<M>>,
}

impl<M: ARM9Mem> ARM9ESBuilder<M> {
    pub fn build(self) -> ARM9ES<M> {
        ARM9ES {
            regs: [0; 16],
            fiq_regs: [0; 7],
            irq_regs: [0; 2],
            und_regs: [0; 2],
            abt_regs: [0; 2],
            svc_regs: [0; 2],

            cpsr: CPSR::I | CPSR::F | CPSR::SVC,
            fiq_spsr: Default::default(),
            irq_spsr: Default::default(),
            und_spsr: Default::default(),
            abt_spsr: Default::default(),
            svc_spsr: Default::default(),

            mem:        self.mem,
            coproc:     self.coproc,
            swi_hook:   self.swi_hook,

            fetched_instr: None,
            decoded_instr: None,
            fetch_type:    MemCycleType::N,
        }
    }

    pub fn add_coproc(mut self, coproc: CoprocV5Impl, at: usize) -> Self {
        self.coproc[at] = Some(coproc);
        self
    }

    pub fn set_swi_hook(mut self, swi_hook: SwiHook<M>) -> Self {
        self.swi_hook = Some(swi_hook);
        self
    }
}

/// ARM9xxE-S processor.
/// 
/// Implements ARMv5 instruction set, Thumb compatible.
pub struct ARM9ES<M: ARM9Mem> {
    regs: [u32; 16],
    fiq_regs: [u32; 7],
    irq_regs: [u32; 2],
    und_regs: [u32; 2],
    abt_regs: [u32; 2],
    svc_regs: [u32; 2],

    cpsr: CPSR,
    fiq_spsr: SPSR,
    irq_spsr: SPSR,
    und_spsr: SPSR,
    abt_spsr: SPSR,
    svc_spsr: SPSR,

    mem:        Box<M>,
    coproc:     [Option<CoprocV5Impl>; NUM_COPROCS],
    swi_hook:   Option<SwiHook<M>>,
    
    fetched_instr: Option<u32>,
    decoded_instr: Option<u32>,
    fetch_type:    MemCycleType,
}

impl<M: ARM9Mem> ARM9ES<M> {
    pub fn new(mem: Box<M>) -> ARM9ESBuilder<M> {
        ARM9ESBuilder {
            mem:        mem,
            coproc:     [None, None, None, None, None, None, None, None, None, None, None, None, None, None, None],
            swi_hook:   None,
        }
    }

    /// Shadow the registers of the processor.
    ///
    /// Call this both before and after setting cpsr.
    fn shadow_registers(&mut self) {
        use Mode::*;
        match self.cpsr.mode() {
            USR => {},
            FIQ => {
                std::mem::swap(&mut self.regs[8], &mut self.fiq_regs[0]);
                std::mem::swap(&mut self.regs[9], &mut self.fiq_regs[1]);
                std::mem::swap(&mut self.regs[10], &mut self.fiq_regs[2]);
                std::mem::swap(&mut self.regs[11], &mut self.fiq_regs[3]);
                std::mem::swap(&mut self.regs[12], &mut self.fiq_regs[4]);
                std::mem::swap(&mut self.regs[13], &mut self.fiq_regs[5]);
                std::mem::swap(&mut self.regs[14], &mut self.fiq_regs[6]);
            },
            IRQ => {
                std::mem::swap(&mut self.regs[13], &mut self.irq_regs[0]);
                std::mem::swap(&mut self.regs[14], &mut self.irq_regs[1]);
            },
            UND => {
                std::mem::swap(&mut self.regs[13], &mut self.und_regs[0]);
                std::mem::swap(&mut self.regs[14], &mut self.und_regs[1]);
            },
            SVC => {
                std::mem::swap(&mut self.regs[13], &mut self.svc_regs[0]);
                std::mem::swap(&mut self.regs[14], &mut self.svc_regs[1]);
            },
            ABT => {
                std::mem::swap(&mut self.regs[13], &mut self.abt_regs[0]);
                std::mem::swap(&mut self.regs[14], &mut self.abt_regs[1]);
            },
        }
    }

    /// Flush the pipeline when the program counter changes.
    ///
    /// This also disables interrupts until the pipeline fills up.
    fn flush_pipeline(&mut self) {
        self.fetched_instr = None;
        self.decoded_instr = None;
        self.next_fetch_non_seq();
    }
}

impl<M: ARM9Mem> ARMDriver for ARM9ES<M> {
    /// Advance a single instruction through the pipeline.
    /// Will always fetch a new instruction,
    /// however it may not always execute one.
    /// 
    /// Returns how many cycles passed.
    fn step(&mut self) -> usize {
        let pc_next = self.regs[PC_REG];
        let executing_instr = self.decoded_instr;
        let cycles = if self.cpsr.contains(CPSR::T) {
            // Fetch the next instr.
            let (new_fetched_instr, fetch_cycles) = self.mem.fetch_instr_halfword(self.fetch_type, pc_next);
            // Shift the pipeline
            self.decoded_instr = self.fetched_instr;
            self.fetched_instr = Some(new_fetched_instr as u32);
            // Execute the decoded instr.
            let execute_cycles = if let Some(instr) = executing_instr {
                let i = decode_thumb(instr as u16);
                i.execute(self)
            } else {
                0
            };
            self.regs[PC_REG] = self.regs[PC_REG].wrapping_add(T_SIZE);
            // Calc cycles
            fetch_cycles + execute_cycles
        } else {
            // Fetch the next instr.
            let (new_fetched_instr, fetch_cycles) = self.mem.fetch_instr_word(self.fetch_type, pc_next);
            // Shift the pipeline
            self.decoded_instr = self.fetched_instr;
            self.fetched_instr = Some(new_fetched_instr);
            // Execute the decoded instr.
            let execute_cycles = if let Some(instr) = executing_instr {
                let i = decode_arm(instr);
                i.execute(self)
            } else {
                0
            };
            self.regs[PC_REG] = self.regs[PC_REG].wrapping_add(I_SIZE);
            // Calc cycles
            fetch_cycles + execute_cycles
        };
        self.fetch_type = MemCycleType::S;
        self.clock(cycles);
        cycles
    }
}

impl<M: ARM9Mem> ARMCore<M> for ARM9ES<M> {
    fn read_reg(&self, n: usize) -> u32 {
        self.regs[n]
    }
    fn write_reg(&mut self, n: usize, data: u32) {
        if n == PC_REG {
            let pc = data & 0xFFFF_FFFE;
            self.do_branch(pc.wrapping_sub(self.cpsr.instr_size()));
        } else {
            self.regs[n] = data;
        }
    }

    fn do_branch(&mut self, dest: u32) {
        self.regs[PC_REG] = dest;
        self.flush_pipeline();
    }
    fn call_subroutine(&mut self, dest: u32) {
        self.regs[PC_REG] = dest.wrapping_sub(self.cpsr.instr_size());

        self.flush_pipeline();
    }

    fn clock(&mut self, cycles: usize) {
        match self.mem.clock(cycles) {
            Some(ExternalException::RST) => self.reset(),
            Some(ExternalException::FIQ) => if !self.cpsr.contains(CPSR::F) && self.decoded_instr.is_some() {
                self.fast_interrupt();
            },
            Some(ExternalException::IRQ) => if !self.cpsr.contains(CPSR::I) && self.decoded_instr.is_some() {
                self.interrupt();
            },
            None => {}
        }
    }

    fn read_usr_reg(&self, n: usize) -> u32 {
        let mode = self.cpsr.mode();
        match n {
            13..=14 if mode == Mode::IRQ => self.irq_regs[n - 13],
            13..=14 if mode == Mode::UND => self.und_regs[n - 13],
            13..=14 if mode == Mode::SVC => self.svc_regs[n - 13],
            13..=14 if mode == Mode::ABT => self.abt_regs[n - 13],
            8..=14 if mode == Mode::FIQ => self.fiq_regs[n - 8],
            _ => self.regs[n],
        }
    }
    fn write_usr_reg(&mut self, n: usize, data: u32) {
        let mode = self.cpsr.mode();
        match n {
            13..=14 if mode == Mode::IRQ => self.irq_regs[n - 13] = data,
            13..=14 if mode == Mode::UND => self.und_regs[n - 13] = data,
            13..=14 if mode == Mode::SVC => self.svc_regs[n - 13] = data,
            13..=14 if mode == Mode::ABT => self.abt_regs[n - 13] = data,
            8..=14 if mode == Mode::FIQ => self.fiq_regs[n - 8] = data,
            _ => self.regs[n] = data,
        }
    }

    fn read_cpsr(&self) -> CPSR {
        self.cpsr
    }
    fn write_cpsr(&mut self, data: CPSR) {
        self.shadow_registers();
        self.cpsr = data;
        self.shadow_registers();
    }
    fn write_flags(&mut self, flags: CPSR) {
        self.cpsr = flags;
    }

    fn read_spsr(&self) -> CPSR {
        use Mode::*;
        match self.cpsr.mode() {
            USR => panic!("Can't read SPSR in USR mode"),
            FIQ => self.fiq_spsr,
            IRQ => self.irq_spsr,
            UND => self.und_spsr,
            SVC => self.svc_spsr,
            ABT => self.abt_spsr,
        }
    }
    fn write_spsr(&mut self, data: CPSR) {
        use Mode::*;
        match self.cpsr.mode() {
            USR => panic!("Can't write SPSR in USR mode"),
            FIQ => self.fiq_spsr = data,
            IRQ => self.irq_spsr = data,
            UND => self.und_spsr = data,
            SVC => self.svc_spsr = data,
            ABT => self.abt_spsr = data,
        }
    }

    fn reset(&mut self) {
        self.shadow_registers();
        self.svc_regs[1] = self.regs[PC_REG] - self.cpsr.instr_size();
        self.regs[PC_REG] = 0xFFFF_0000;
        self.svc_spsr = self.cpsr;

        self.cpsr.set_mode(Mode::SVC);
        self.cpsr.remove(CPSR::T);
        self.cpsr.insert(CPSR::I | CPSR::F);

        self.shadow_registers();
        self.flush_pipeline();
    }
    fn interrupt(&mut self) {
        self.shadow_registers();
        self.irq_regs[1] = self.regs[PC_REG] - if self.cpsr.contains(CPSR::T) {0} else {I_SIZE};
        self.regs[PC_REG] = 0xFFFF_0018;
        self.irq_spsr = self.cpsr;

        self.cpsr.set_mode(Mode::IRQ);
        self.cpsr.remove(CPSR::T);
        self.cpsr.insert(CPSR::I);

        self.shadow_registers();
        self.flush_pipeline();
    }
    fn fast_interrupt(&mut self) {
        self.shadow_registers();
        self.fiq_regs[6] = self.regs[PC_REG] - if self.cpsr.contains(CPSR::T) {0} else {I_SIZE};
        self.regs[PC_REG] = 0xFFFF_001C;
        self.fiq_spsr = self.cpsr;

        self.cpsr.set_mode(Mode::FIQ);
        self.cpsr.remove(CPSR::T);
        self.cpsr.insert(CPSR::I | CPSR::F);

        self.shadow_registers();
        self.flush_pipeline();
    }
    fn software_exception(&mut self) {
        self.shadow_registers();
        self.svc_regs[1] = self.regs[PC_REG] - self.cpsr.instr_size();
        self.regs[PC_REG] = 0xFFFF_0008 - self.cpsr.instr_size();
        self.svc_spsr = self.cpsr;

        self.cpsr.set_mode(Mode::SVC);
        self.cpsr.remove(CPSR::T);
        self.cpsr.insert(CPSR::I);

        self.shadow_registers();
        self.flush_pipeline();
    }
    fn undefined_exception(&mut self) {
        self.shadow_registers();
        self.und_regs[1] = self.regs[PC_REG] - self.cpsr.instr_size();
        self.regs[PC_REG] = 0xFFFF_0004 - self.cpsr.instr_size();
        self.und_spsr = self.cpsr;

        self.cpsr.set_mode(Mode::UND);
        self.cpsr.remove(CPSR::T);
        self.cpsr.insert(CPSR::I);

        self.shadow_registers();
        self.flush_pipeline();
    }

    fn return_from_exception(&mut self) {
        self.shadow_registers();

        use Mode::*;
        match self.cpsr.mode() {
            USR => {},
            FIQ => {
                self.cpsr = self.fiq_spsr;
            },
            IRQ => {
                self.cpsr = self.irq_spsr;
            },
            UND => {
                self.cpsr = self.und_spsr;
            },
            SVC => {
                self.cpsr = self.svc_spsr;
            },
            ABT => {
                self.cpsr = self.abt_spsr;
            }
        }

        self.shadow_registers();
    }

    fn try_swi_hook(&mut self, comment: u32) -> bool {
        if let Some(hook) = self.swi_hook {
            use std::convert::TryInto;
            let regs = hook(comment, &mut self.mem, self.regs[0..4].try_into().unwrap());
            self.regs[0] = regs[0];
            self.regs[1] = regs[1];
            self.regs[3] = regs[2];
            true
        } else {
            false
        }
    }

    fn next_fetch_non_seq(&mut self) {
        self.fetch_type = MemCycleType::N;
    }

    fn ref_mem<'a>(&'a self) -> &'a M {
        &self.mem
    }

    fn mut_mem<'a>(&'a mut self) -> &'a mut M {
        &mut self.mem
    }

    fn mut_coproc<'a>(&'a mut self, coproc: usize) -> Option<&'a mut dyn CoprocV4> {
        if coproc == 15 {
            return Some(self.mem.mut_cp15().as_v4());
        }
        match &mut self.coproc[coproc] {
            Some(c) => Some(c.as_v4()),
            None => None
        }
    }
}

impl<M: ARM9Mem> ARMCoreV5 for ARM9ES<M> {
    /// Reference a coprocessor mutably.
    fn mut_coproc_v5<'a>(&'a mut self, coproc: usize) -> Option<&'a mut dyn CoprocV5> {
        if coproc == 15 {
            return Some(self.mem.mut_cp15());
        }
        match &mut self.coproc[coproc] {
            Some(c) => Some(c.as_mut()),
            None => None
        }
    }
}

impl<M: ARM9Mem> ARMv4<M> for ARM9ES<M> {}
impl<M: ARM9Mem> ARMv5<M> for ARM9ES<M> {}

impl<M: ARM9Mem> Debugger for ARM9ES<M> {
    fn inspect_state(&mut self) -> CPUState {
        let next_instr = if self.cpsr.contains(CPSR::T) {
            self.mem.fetch_instr_halfword(MemCycleType::N, self.regs[PC_REG]).0 as u32
        } else {
            self.mem.fetch_instr_word(MemCycleType::N, self.regs[PC_REG]).0
        };
        let thumb_mode = self.cpsr.contains(CPSR::T);
        let pipeline = if thumb_mode {[
            Some(decode_thumb(next_instr as u16)),
            self.fetched_instr.map(|i| decode_thumb(i as u16)),
            self.decoded_instr.map(|i| decode_thumb(i as u16))
        ]} else {[
            Some(decode_arm(next_instr)),
            self.fetched_instr.map(|i| decode_arm(i)),
            self.decoded_instr.map(|i| decode_arm(i))
        ]};
        CPUState {
            regs:   self.regs,
            flags:  self.cpsr.bits(),
            thumb_mode: thumb_mode,

            pipeline: pipeline,
        }
    }
}
