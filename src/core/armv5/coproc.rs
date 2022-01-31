use crate::armv4::CoprocV4;

pub type CoprocV5Impl = Box<dyn CoprocV5 + Send>;

/// ARMv5 Coprocessor interface.
/// The main processor will call the coprocessor via these methods.
pub trait CoprocV5: CoprocV4 {
    /// Transfer from ARM register to Coproc register.
    fn mcr2(&mut self, dest_reg: usize, op_reg: usize, data: u32, op: u32, info: u32) -> usize;

    /// Transfer from Coproc register to ARM register.
    fn mrc2(&mut self, src_reg: usize, op_reg: usize, op: u32, info: u32) -> (u32, usize);

    /// Multi-transfer from ARM register to Coproc register.
    fn mcrr(&mut self, op_reg: usize, data_lo: u32, data_hi: u32, op: u32) -> usize;

    /// Multi-transfer from Coproc register to ARM register.
    fn mrrc(&mut self, op_reg: usize, op: u32) -> (u32, u32, usize);
    
    /// Transfer from memory to Coproc register.
    fn ldc2(&mut self, transfer_len: bool, dest_reg: usize, data: u32) -> usize;

    /// Transfer from Coproc register to memory.
    fn stc2(&mut self, transfer_len: bool, src_reg: usize) -> (u32, usize);

    /// Coprocessor data operation.
    fn cdp2(&mut self, op: u32, reg_cn: usize, reg_cd: usize, info: u32, reg_cm: usize) -> usize;
}