/// ARM7TDMI processor.

use crate::core::{
    Mode,
    CPSR,
    SPSR,
    ARMCore
};

pub struct ARM7TDMI {
    mode: Mode,

    regs: [u32; 16],
    fiq_regs: [u32; 7],
    irq_regs: [u32; 2],
    und_regs: [u32; 2],
    svc_regs: [u32; 2],

    cpsr: CPSR,
    fiq_spsr: SPSR,
    irq_spsr: SPSR,
    und_spsr: SPSR,
    svc_spsr: SPSR,

    // memory...
}

impl ARM7TDMI {
    pub fn new() -> Self {
        Self {
            mode: Mode::USR,
            regs: [0; 16],
            fiq_regs: [0; 7],
            irq_regs: [0; 2],
            und_regs: [0; 2],
            svc_regs: [0; 2],

            cpsr: Default::default(),
            fiq_spsr: Default::default(),
            irq_spsr: Default::default(),
            und_spsr: Default::default(),
            svc_spsr: Default::default(),
        }
    }
}

impl ARMCore for ARM7TDMI {
    fn read_reg(&self, n: usize) -> u32 {
        self.regs[n]
    }
    fn write_reg(&mut self, n: usize, data: u32) {
        self.regs[n] = data;
    }

    fn read_cpsr(&self) -> CPSR {
        self.cpsr
    }
    fn write_cpsr(&mut self, data: CPSR) {
        self.cpsr = data;
    }

    fn set_mode(&mut self, mode: Mode) {
        // TODO: trigger interrupt?
        self.mode = mode;
    }
}