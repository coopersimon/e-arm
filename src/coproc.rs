/// Coprocessor interface for ARM processor.

/// Coprocessor interface.
/// The main processor will call the coprocessor via these methods.
pub trait Coprocessor {
    /// Transfer from ARM register to Coproc register.
    fn mcr(&mut self, dest_reg: usize, op_reg: usize, data: u32, op: u32, info: u32);

    /// Transfer from Coproc register to ARM register.
    fn mrc(&mut self, src_reg: usize, op_reg: usize, op: u32, info: u32) -> u32;

    /// Transfer from memory to Coproc register.
    fn ldc(&mut self, transfer_len: bool, dest_reg: usize, data: u32);

    /// Transfer from Coproc register to memory.
    fn stc(&mut self, transfer_len: bool, src_reg: usize) -> u32;

    /// Coprocessor data operation.
    fn cdp(&mut self, op: u32, reg_cn: usize, reg_cd: usize, info: u32, reg_cm: usize);
}