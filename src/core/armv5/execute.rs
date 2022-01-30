
use crate::{
    common::{
        u32, u64
    },
    core::{ARMCore, CPSR, ARMv4, constants::*},
    memory::{Mem32, MemCycleType}
};

const Q_MAX: u32 = i32::MAX as u32;
const Q_MIN: u32 = i32::MIN as u32;

/// Execution of ARMv5 instructions.
pub trait ARMv5<M: Mem32<Addr = u32>>: ARMv4<M> {

    /// BKPT
    /// Breakpoint
    fn bkpt(&mut self, _comment: u32) -> usize {
        // TODO
        0
    }

    // Saturating arithmetic

    /// QADD
    /// Saturating signed add
    fn qadd(&mut self, rd: usize, rm: usize, rn: usize) -> usize {
        let op1 = self.read_reg(rm) as i32;
        let op2 = self.read_reg(rn) as i32;
        match op1.checked_add(op2) {
            Some(result) => self.write_reg(rd, result as u32),
            None => {
                self.write_reg(rd, Q_MAX);
                let mut cpsr = self.read_cpsr();
                cpsr.insert(CPSR::Q);
                self.write_flags(cpsr);
            }
        }
        0
    }

    /// QSUB
    /// Saturating signed sub
    fn qsub(&mut self, rd: usize, rm: usize, rn: usize) -> usize {
        let op1 = self.read_reg(rm) as i32;
        let op2 = self.read_reg(rn) as i32;
        match op1.checked_sub(op2) {
            Some(result) => self.write_reg(rd, result as u32),
            None => {
                self.write_reg(rd, Q_MIN);
                let mut cpsr = self.read_cpsr();
                cpsr.insert(CPSR::Q);
                self.write_flags(cpsr);
            }
        }
        0
    }

    /// Find op2 for the QDxxx ops: Rn * 2
    /// Will saturate and set Q flag if overflow occurs.
    fn qdop2(&mut self, rn: usize) -> i32 {
        let reg = self.read_reg(rn) as i32;
        match reg.checked_shl(1) {
            Some(result) => result,
            None => {
                let mut cpsr = self.read_cpsr();
                cpsr.insert(CPSR::Q);
                self.write_flags(cpsr);
                if reg < 0 {
                    i32::MIN
                } else {
                    i32::MAX
                }
            }
        }
    }

    /// QDADD
    /// Saturating signed double + add
    fn qdadd(&mut self, rd: usize, rm: usize, rn: usize) -> usize {
        let op1 = self.read_reg(rm) as i32;
        let op2 = self.qdop2(rn);
        match op1.checked_add(op2) {
            Some(result) => self.write_reg(rd, result as u32),
            None => {
                let mut cpsr = self.read_cpsr();
                cpsr.insert(CPSR::Q);
                self.write_flags(cpsr);
                self.write_reg(rd, if op1 < 0 {
                    Q_MIN
                } else {
                    Q_MAX
                });
            }
        }
        0
    }

    /// QDSUB
    /// Saturating signed double + sub
    fn qdsub(&mut self, rd: usize, rm: usize, rn: usize) -> usize {
        let op1 = self.read_reg(rm) as i32;
        let op2 = self.qdop2(rn);
        match op1.checked_sub(op2) {
            Some(result) => self.write_reg(rd, result as u32),
            None => {
                let mut cpsr = self.read_cpsr();
                cpsr.insert(CPSR::Q);
                self.write_flags(cpsr);
                self.write_reg(rd, if op1 < 0 {
                    Q_MAX
                } else {
                    Q_MIN
                });
            }
        }
        0
    }

    // Halfword multiplication

    /// SMULxy
    /// Signed multiply halfword
    fn smulxy(&mut self, rd: usize, rm: usize, rs: usize, y: bool, x: bool) -> usize {
        let op1 = if x {u32::hi(self.read_reg(rm))} else {u32::lo(self.read_reg(rm))} as i16;
        let op2 = if y {u32::hi(self.read_reg(rs))} else {u32::lo(self.read_reg(rs))} as i16;
        let result = (op1 as i32) * (op2 as i32);
        self.write_reg(rd, result as u32);
        0
    }

    /// SMULWy
    /// Signed multiply halfword wide
    fn smulwy(&mut self, rd: usize, rm: usize, rs: usize, y: bool) -> usize {
        let op1 = self.read_reg(rm) as i32;
        let op2 = if y {u32::hi(self.read_reg(rs))} else {u32::lo(self.read_reg(rs))} as i16;
        let result = (op1 as i64) * (op2 as i64);
        let result_32 = (result >> 16) as i32;
        self.write_reg(rd, result_32 as u32);
        0
    }

    /// SMLAxy
    /// Signed multiply halfword and accumulate
    fn smlaxy(&mut self, rd: usize, rm: usize, rs: usize, rn: usize, y: bool, x: bool) -> usize {
        let op1 = if x {u32::hi(self.read_reg(rm))} else {u32::lo(self.read_reg(rm))} as i16;
        let op2 = if y {u32::hi(self.read_reg(rs))} else {u32::lo(self.read_reg(rs))} as i16;
        let mul_result = (op1 as i32) * (op2 as i32);
        let (result, overflow) = mul_result.overflowing_add(self.read_reg(rn) as i32);
        if overflow {
            let mut cpsr = self.read_cpsr();
            cpsr.insert(CPSR::Q);
            self.write_flags(cpsr);
        }
        self.write_reg(rd, result as u32);
        0
    }

    /// SMLAWy
    /// Signed multiply halfword wide and accumulate
    fn smlawy(&mut self, rd: usize, rm: usize, rs: usize, rn: usize, y: bool) -> usize {
        let op1 = self.read_reg(rm) as i32;
        let op2 = if y {u32::hi(self.read_reg(rs))} else {u32::lo(self.read_reg(rs))} as i16;
        let mul_result = (op1 as i64) * (op2 as i64);
        let mul_result_32 = (mul_result >> 16) as i32;
        let (result, overflow) = mul_result_32.overflowing_add(self.read_reg(rn) as i32);
        if overflow {
            let mut cpsr = self.read_cpsr();
            cpsr.insert(CPSR::Q);
            self.write_flags(cpsr);
        }
        self.write_reg(rd, result as u32);
        0
    }

    /// SMLALxy
    /// Signed multiply halfword and long accumulate
    fn smlalxy(&mut self, rd_hi: usize, rd_lo: usize, rm: usize, rs: usize, y: bool, x: bool) -> usize {
        let op1 = if x {u32::hi(self.read_reg(rm))} else {u32::lo(self.read_reg(rm))} as i16;
        let op2 = if y {u32::hi(self.read_reg(rs))} else {u32::lo(self.read_reg(rs))} as i16;
        let mul_result = (op1 as i32) * (op2 as i32);
        let acc_op = u64::make(self.read_reg(rd_hi), self.read_reg(rd_lo));
        // TODO: should we sign-extend here?
        let result = acc_op.wrapping_add(mul_result as u64);
        self.write_reg(rd_lo, u64::lo(result));
        self.write_reg(rd_hi, u64::hi(result));
        1
    }

    // Branch

    /// BLX
    /// Branch, link, and exchange - using immediate value
    fn blxi(&mut self, offset: u32) -> usize {
        let current_pc = self.read_reg(PC_REG).wrapping_sub(I_SIZE);
        self.write_reg(LINK_REG, current_pc);

        let mut cpsr = self.read_cpsr();
        cpsr.insert(CPSR::T);
        self.write_flags(cpsr);

        // Call into JIT cache.
        self.call_subroutine(self.read_reg(PC_REG).wrapping_add(offset));
        0
    }

    /// BLX
    /// Branch, link, and exchange - using register value
    fn blxr(&mut self, reg: usize) -> usize {
        let reg_val = self.read_reg(reg);
        let mut cpsr = self.read_cpsr();
        let src_i_size = cpsr.instr_size();
        cpsr.set(CPSR::T, u32::test_bit(reg_val, 0));
        self.write_flags(cpsr);

        let current_pc = self.read_reg(PC_REG).wrapping_sub(src_i_size);
        self.write_reg(LINK_REG, current_pc);

        let dest = reg_val & 0xFFFFFFFE;
        // Call into JIT cache.
        self.call_subroutine(dest.wrapping_sub(src_i_size));
        0
    }
}
