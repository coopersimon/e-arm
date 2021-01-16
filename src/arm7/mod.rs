/// ARM7TDMI processor.

use crate::core::{
    constants::*,
    Exception,
    Mode,
    CPSR,
    SPSR,
    ARMCore,
    ARMv4,
    Thumbv4
};
use crate::memory::Mem32;
use crate::coproc::Coprocessor;

pub struct ARM7TDMI<M: Mem32> {
    mode: Mode,

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
    coproc: [Option<Box<dyn Coprocessor>>; 16],
    
    fetched_instr: Option<u32>,
    decoded_instr: Option<u32>,
}

impl<M: Mem32<Addr = u32>> ARM7TDMI<M> {
    pub fn new(mem: M, coproc: [Option<Box<dyn Coprocessor>>; 16]) -> Self {
        Self {
            mode: Mode::USR,
            regs: [0; 16],
            fiq_regs: [0; 7],
            irq_regs: [0; 2],
            und_regs: [0; 2],
            abt_regs: [0; 2],
            svc_regs: [0; 2],

            cpsr: Default::default(),
            fiq_spsr: Default::default(),
            irq_spsr: Default::default(),
            und_spsr: Default::default(),
            abt_spsr: Default::default(),
            svc_spsr: Default::default(),

            mem:    mem,
            coproc: coproc,

            fetched_instr: None,
            decoded_instr: None,
        }
    }

    /// Step a single instruction.
    /// Returns how many cycles passed.
    pub fn step(&mut self) -> usize {
        if self.cpsr.contains(CPSR::T) {
            // Execute the decoded instr.
            let execute_cycles = if let Some(executing_instr) = self.decoded_instr {
                self.execute_thumb(executing_instr as u16)
            } else {
                0
            };
            // Fetch the next instr.
            let (new_fetched_instr, fetch_cycles) = self.load_halfword(self.regs[PC_REG]);
            self.regs[PC_REG] += 2;
            // Shift the pipeline
            self.decoded_instr = self.fetched_instr;
            self.fetched_instr = Some(new_fetched_instr as u32);
            // Calc cycles
            fetch_cycles + execute_cycles
        } else {
            // Execute the decoded instr.
            let execute_cycles = if let Some(executing_instr) = self.decoded_instr {
                self.execute_instruction(executing_instr)
            } else {
                0
            };
            // Fetch the next instr.
            let (new_fetched_instr, fetch_cycles) = self.load_word(self.regs[PC_REG]);
            self.regs[PC_REG] += 4;
            // Shift the pipeline
            self.decoded_instr = self.fetched_instr;
            self.fetched_instr = Some(new_fetched_instr);
            // Calc cycles
            fetch_cycles + execute_cycles
        }
    }
}

impl<M: Mem32<Addr = u32>> Mem32 for ARM7TDMI<M> {
    type Addr = u32;

    fn load_byte(&mut self, addr: Self::Addr) -> (u8, usize) {
        self.mem.load_byte(addr)
    }
    fn store_byte(&mut self, addr: Self::Addr, data: u8) -> usize {
        self.mem.store_byte(addr, data)
    }

    fn load_halfword(&mut self, addr: Self::Addr) -> (u16, usize) {
        self.mem.load_halfword(addr)
    }
    fn store_halfword(&mut self, addr: Self::Addr, data: u16) -> usize {
        self.mem.store_halfword(addr, data)
    }

    fn load_word(&mut self, addr: Self::Addr) -> (u32, usize) {
        self.mem.load_word(addr)
    }
    fn store_word(&mut self, addr: Self::Addr, data: u32) -> usize {
        self.mem.store_word(addr, data)
    }
}

impl<M: Mem32> ARMCore for ARM7TDMI<M> {
    fn read_reg(&self, n: usize) -> u32 {
        self.regs[n]
    }
    fn write_reg(&mut self, n: usize, data: u32) {
        self.regs[n] = data;
        if n == PC_REG {
            // Flush pipeline
            self.fetched_instr = None;
            self.decoded_instr = None;
        }
    }

    fn read_usr_reg(&self, n: usize) -> u32 {
        match n {
            13..=14 if self.mode == Mode::IRQ => self.irq_regs[n - 13],
            13..=14 if self.mode == Mode::UND => self.und_regs[n - 13],
            13..=14 if self.mode == Mode::SVC => self.svc_regs[n - 13],
            13..=14 if self.mode == Mode::ABT => self.abt_regs[n - 13],
            8..=14 if self.mode == Mode::FIQ => self.fiq_regs[n - 8],
            _ => self.regs[n],
        }
    }
    fn write_usr_reg(&mut self, n: usize, data: u32) {
        match n {
            13..=14 if self.mode == Mode::IRQ => self.irq_regs[n - 13] = data,
            13..=14 if self.mode == Mode::UND => self.und_regs[n - 13] = data,
            13..=14 if self.mode == Mode::SVC => self.svc_regs[n - 13] = data,
            13..=14 if self.mode == Mode::ABT => self.abt_regs[n - 13] = data,
            8..=14 if self.mode == Mode::FIQ => self.fiq_regs[n - 8] = data,
            _ => self.regs[n] = data,
        }
    }

    fn read_cpsr(&self) -> CPSR {
        self.cpsr
    }
    fn write_cpsr(&mut self, data: CPSR) {
        self.cpsr = data;
    }

    fn read_spsr(&self) -> CPSR {
        use Mode::*;
        match self.mode {
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
        match self.mode {
            USR => return,
            FIQ => self.fiq_spsr = data,
            IRQ => self.irq_spsr = data,
            UND => self.und_spsr = data,
            SVC => self.svc_spsr = data,
            ABT => self.abt_spsr = data,
        }
    }

    fn trigger_exception(&mut self, exception: Exception) {
        use Exception::*;
        match exception {
            Reset => {
                self.regs[LINK_REG] = self.regs[PC_REG];
                self.regs[PC_REG] = 0x0000_0000;
                self.svc_spsr = self.cpsr;

                self.mode = Mode::SVC;
                self.cpsr.set_mode(Mode::SVC);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I | CPSR::F);
            },
            DataAbort => {
                self.abt_regs[0] = self.regs[13];
                self.abt_regs[1] = self.regs[14];
                self.regs[LINK_REG] = self.regs[PC_REG];
                self.regs[PC_REG] = 0x0000_0010;
                self.abt_spsr = self.cpsr;

                self.mode = Mode::ABT;
                self.cpsr.set_mode(Mode::ABT);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I);
            },
            FastInterrupt => {
                self.fiq_regs[0] = self.regs[8];
                self.fiq_regs[1] = self.regs[9];
                self.fiq_regs[2] = self.regs[10];
                self.fiq_regs[3] = self.regs[11];
                self.fiq_regs[4] = self.regs[12];
                self.fiq_regs[5] = self.regs[13];
                self.fiq_regs[6] = self.regs[14];
                self.regs[LINK_REG] = self.regs[PC_REG];
                self.regs[PC_REG] = 0x0000_001C;
                self.fiq_spsr = self.cpsr;

                self.mode = Mode::FIQ;
                self.cpsr.set_mode(Mode::FIQ);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I | CPSR::F);
            },
            Interrupt => {
                self.irq_regs[0] = self.regs[13];
                self.irq_regs[1] = self.regs[14];
                self.regs[LINK_REG] = self.regs[PC_REG];
                self.regs[PC_REG] = 0x0000_0018;
                self.irq_spsr = self.cpsr;

                self.mode = Mode::IRQ;
                self.cpsr.set_mode(Mode::IRQ);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I);
            },
            PrefetchAbort => {
                self.abt_regs[0] = self.regs[13];
                self.abt_regs[1] = self.regs[14];
                self.regs[LINK_REG] = self.regs[PC_REG];
                self.regs[PC_REG] = 0x0000_000C;
                self.abt_spsr = self.cpsr;

                self.mode = Mode::ABT;
                self.cpsr.set_mode(Mode::ABT);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I);
            },
            SoftwareInterrupt => {
                self.svc_regs[0] = self.regs[13];
                self.svc_regs[1] = self.regs[14];
                self.regs[LINK_REG] = self.regs[PC_REG];
                self.regs[PC_REG] = 0x0000_0008;
                self.svc_spsr = self.cpsr;

                self.mode = Mode::SVC;
                self.cpsr.set_mode(Mode::SVC);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I);
            },
            UndefinedInstruction => {
                self.und_regs[0] = self.regs[13];
                self.und_regs[1] = self.regs[14];
                self.regs[LINK_REG] = self.regs[PC_REG];
                self.regs[PC_REG] = 0x0000_0004;
                self.und_spsr = self.cpsr;

                self.mode = Mode::UND;
                self.cpsr.set_mode(Mode::UND);
                self.cpsr.remove(CPSR::T);
                self.cpsr.insert(CPSR::I);
            },
        }
    }

    fn return_from_exception(&mut self) {
        use Mode::*;
        match self.mode {
            USR => panic!("Attempting to transition from USR to USR"),
            FIQ => {
                self.cpsr = self.fiq_spsr;
                self.regs[8] = self.fiq_regs[0];
                self.regs[9] = self.fiq_regs[1];
                self.regs[10] = self.fiq_regs[2];
                self.regs[11] = self.fiq_regs[3];
                self.regs[12] = self.fiq_regs[4];
                self.regs[13] = self.fiq_regs[5];
                self.regs[14] = self.fiq_regs[6];
            },
            IRQ => {
                self.cpsr = self.irq_spsr;
                self.regs[13] = self.irq_regs[0];
                self.regs[14] = self.irq_regs[1];
            },
            UND => {
                self.cpsr = self.und_spsr;
                self.regs[13] = self.und_regs[0];
                self.regs[14] = self.und_regs[1];
            },
            SVC => {
                self.cpsr = self.svc_spsr;
                self.regs[13] = self.svc_regs[0];
                self.regs[14] = self.svc_regs[1];
            },
            ABT => {
                self.cpsr = self.abt_spsr;
                self.regs[13] = self.abt_regs[0];
                self.regs[14] = self.abt_regs[1];
            }
        }
        self.mode = USR;
    }

    fn ref_coproc<'a>(&'a mut self, coproc: usize) -> Option<&'a mut Box<dyn Coprocessor>> {
        self.coproc[coproc].as_mut()
    }
}

impl<M: Mem32<Addr = u32>> ARMv4 for ARM7TDMI<M> {}
impl<M: Mem32<Addr = u32>> Thumbv4 for ARM7TDMI<M> {}