/// Debugger interface

pub struct CPUState {
    pub regs: [u32; 16],
    pub flags: u32,
}

pub trait Debugger {
    fn inspect_state(&self) -> CPUState;
}
