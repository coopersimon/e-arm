/// Debugger interface

pub struct CPUState {
    pub regs: [u32; 16],
    pub flags: u32,
    pub thumb_mode: bool,

    pub pipeline: [Option<crate::ARMv4Instruction>; 3],
}

pub trait Debugger {
    fn inspect_state(&mut self) -> CPUState;
}
