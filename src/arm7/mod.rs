/// ARM7TDMI processor.

mod utils;

use crate::core::{
    constants::*,
    Mode,
    CPSR,
    SPSR,
    ARMCore,
    ARMv4,
    Thumbv4
};
use crate::memory::{
    Mem32, MemCycleType
};
use crate::coproc::Coprocessor;
use crate::Exception;
use crate::{Debugger, CPUState};

pub struct ARM7TDMI<M: Mem32> {
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

    mem:    M,
    coproc: Box<[Option<Box<dyn Coprocessor>>]>,
    
    fetched_instr: Option<u32>,
    decoded_instr: Option<u32>,
    fetch_type:    MemCycleType,
}

impl<M: Mem32<Addr = u32>> ARM7TDMI<M> {
    pub fn new(mem: M, coproc: std::collections::HashMap<usize, Box<dyn Coprocessor>>) -> Self {
        Self {
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

            mem:    mem,
            coproc: utils::to_slice(coproc),

            fetched_instr: None,
            decoded_instr: None,
            fetch_type:    MemCycleType::N,
        }
    }

    /// Advance a single instruction through the pipeline.
    /// Will always fetch a new instruction,
    /// however it may not always execute one.
    /// 
    /// Returns how many cycles passed.
    /// 
    /// Note that after each step exceptions should be passed to the CPU
    /// via `trigger_exception`.
    pub fn step(&mut self) -> usize {
        let pc_next = self.regs[PC_REG];
        let executing_instr = self.decoded_instr;
        let cycles = if self.cpsr.contains(CPSR::T) {
            // Fetch the next instr.
            let (new_fetched_instr, fetch_cycles) = self.mem.load_halfword(self.fetch_type, pc_next);
            // Shift the pipeline
            self.decoded_instr = self.fetched_instr;
            self.fetched_instr = Some(new_fetched_instr as u32);
            // Execute the decoded instr.
            let execute_cycles = if let Some(instr) = executing_instr {
                self.execute_thumb(instr as u16)
            } else {
                0
            };
            self.regs[PC_REG] = self.regs[PC_REG].wrapping_add(T_SIZE);
            // Calc cycles
            fetch_cycles + execute_cycles
        } else {
            // Fetch the next instr.
            let (new_fetched_instr, fetch_cycles) = self.mem.load_word(self.fetch_type, pc_next);
            // Shift the pipeline
            self.decoded_instr = self.fetched_instr;
            self.fetched_instr = Some(new_fetched_instr);
            // Execute the decoded instr.
            let execute_cycles = if let Some(instr) = executing_instr {
                self.execute_instruction(instr)
            } else {
                0
            };
            self.regs[PC_REG] = self.regs[PC_REG].wrapping_add(I_SIZE);
            // Calc cycles
            fetch_cycles + execute_cycles
        };
        self.fetch_type = MemCycleType::S;
        cycles
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
}

impl<M: Mem32<Addr = u32>> ARMCore<M> for ARM7TDMI<M> {
    fn read_reg(&self, n: usize) -> u32 {
        self.regs[n]
    }
    fn write_reg(&mut self, n: usize, data: u32) {
        if n == PC_REG {
            self.do_branch(data.wrapping_sub(self.cpsr.instr_size()));
        } else {
            self.regs[n] = data;
        }
    }
    fn do_branch(&mut self, dest: u32) {
        self.regs[PC_REG] = dest;

        // Flush pipeline
        self.fetched_instr = None;
        self.decoded_instr = None;
        self.next_fetch_non_seq();
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
            USR => CPSR::default(),
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
            USR => return,
            FIQ => self.fiq_spsr = data,
            IRQ => self.irq_spsr = data,
            UND => self.und_spsr = data,
            SVC => self.svc_spsr = data,
            ABT => self.abt_spsr = data,
        }
    }

    fn trigger_exception(&mut self, exception: Exception) {
        self.shadow_registers();
        
        use Exception::*;
        match exception {
            Reset => {
                self.svc_regs[1] = self.regs[PC_REG] - self.cpsr.instr_size();
                self.regs[PC_REG] = 0x0000_0000;
                self.svc_spsr = self.cpsr;

                self.cpsr.set_mode(Mode::SVC);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I | CPSR::F);
            },
            DataAbort => {
                self.abt_regs[1] = self.regs[PC_REG] - self.cpsr.instr_size();
                self.regs[PC_REG] = 0x0000_0010;
                self.abt_spsr = self.cpsr;

                self.cpsr.set_mode(Mode::ABT);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I);
            },
            FastInterrupt if !self.cpsr.contains(CPSR::F) => {
                self.fiq_regs[6] = self.regs[PC_REG] - self.cpsr.instr_size();
                self.regs[PC_REG] = 0x0000_001C;
                self.fiq_spsr = self.cpsr;

                self.cpsr.set_mode(Mode::FIQ);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I | CPSR::F);
            },
            Interrupt if !self.cpsr.contains(CPSR::I) => {
                self.irq_regs[1] = self.regs[PC_REG] - self.cpsr.instr_size();
                self.regs[PC_REG] = 0x0000_0018;
                self.irq_spsr = self.cpsr;

                self.cpsr.set_mode(Mode::IRQ);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I);
            },
            PrefetchAbort => {
                self.abt_regs[1] = self.regs[PC_REG] - self.cpsr.instr_size();
                self.regs[PC_REG] = 0x0000_000C;
                self.abt_spsr = self.cpsr;

                self.cpsr.set_mode(Mode::ABT);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I);
            },
            SoftwareInterrupt => {
                self.svc_regs[1] = self.regs[PC_REG] - self.cpsr.instr_size();
                self.regs[PC_REG] = 0x0000_0008 - self.cpsr.instr_size();
                self.svc_spsr = self.cpsr;

                self.cpsr.set_mode(Mode::SVC);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I);
            },
            UndefinedInstruction => {
                self.und_regs[1] = self.regs[PC_REG] - self.cpsr.instr_size();
                self.regs[PC_REG] = 0x0000_0004;
                self.und_spsr = self.cpsr;

                self.cpsr.set_mode(Mode::UND);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I);
            },
            _ => {},
        }

        self.shadow_registers();
        // Flush pipeline
        self.fetched_instr = None;
        self.decoded_instr = None;
        self.next_fetch_non_seq();
    }

    fn return_from_exception(&mut self) {
        self.shadow_registers();

        use Mode::*;
        match self.cpsr.mode() {
            USR => panic!("Attempting to transition from USR to USR"),
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

    fn next_fetch_non_seq(&mut self) {
        self.fetch_type = MemCycleType::N;
    }

    fn ref_mem<'a>(&'a mut self) -> &'a mut M {
        &mut self.mem
    }

    fn ref_coproc<'a>(&'a mut self, coproc: usize) -> Option<&'a mut Box<dyn Coprocessor>> {
        self.coproc[coproc].as_mut()
    }
}

impl<M: Mem32<Addr = u32>> ARMv4<M> for ARM7TDMI<M> {}
impl<M: Mem32<Addr = u32>> Thumbv4<M> for ARM7TDMI<M> {}

impl<M: Mem32<Addr = u32>> Debugger for ARM7TDMI<M> {
    fn inspect_state(&mut self) -> CPUState {
        let next_instr = if self.cpsr.contains(CPSR::T) {
            self.mem.load_halfword(MemCycleType::N, self.regs[PC_REG]).0 as u32
        } else {
            self.mem.load_word(MemCycleType::N, self.regs[PC_REG]).0
        };
        CPUState {
            regs:   self.regs,
            flags:  self.cpsr.bits(),
            thumb_mode: self.cpsr.contains(CPSR::T),

            pipeline:   [
                Some(next_instr),
                self.fetched_instr,
                self.decoded_instr,
            ]
        }
    }
}
