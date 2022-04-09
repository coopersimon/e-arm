
use crate::{
    common::{
        u32, u64
    },
    core::{
        CPSR,
        constants::*,
    },
    armv4::{
        ARMv4,
        instructions::{TransferParams, ShiftOperand, OpData}
    },
    memory::{Mem32, MemCycleType}
};
use super::ARMCoreV5;

const Q_MAX: u32 = i32::MAX as u32;
const Q_MIN: u32 = i32::MIN as u32;

/// Execution of ARMv5 instructions.
pub trait ARMv5<M: Mem32<Addr = u32>>: ARMv4<M> + ARMCoreV5 {

    /// BKPT
    /// Breakpoint
    fn bkpt(&mut self, _comment: u32) -> usize {
        // TODO
        0
    }

    /// CLZ
    /// Count leading zeroes
    fn clz(&mut self, rd: usize, rm: usize) -> usize {
        let op = self.read_reg(rm);
        let result = op.leading_zeros();
        self.write_reg(rd, result);
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

    /// QSUB
    /// Saturating signed sub
    fn qsub(&mut self, rd: usize, rm: usize, rn: usize) -> usize {
        let op1 = self.read_reg(rm) as i32;
        let op2 = self.read_reg(rn) as i32;
        match op1.checked_sub(op2) {
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

    /// Find op2 for the QDxxx ops: Rn * 2
    /// Will saturate and set Q flag if overflow occurs.
    fn qdop2(&mut self, rn: usize) -> i32 {
        let reg = self.read_reg(rn) as i32;
        match reg.checked_mul(2) {
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
                    Q_MIN
                } else {
                    Q_MAX
                });
            }
        }
        0
    }

    // Halfword multiplication

    /// SMULxy
    /// Signed multiply halfword
    fn smulxy(&mut self, rd: usize, rm: usize, rs: usize, y: bool, x: bool) -> usize {
        let op1 = if x {u32::hi(self.read_reg(rm))} else {u32::lo(self.read_reg(rm))} as u32;
        let op2 = if y {u32::hi(self.read_reg(rs))} else {u32::lo(self.read_reg(rs))} as u32;
        let result = op1 * op2;
        self.write_reg(rd, result);
        0
    }

    /// SMULWy
    /// Signed multiply halfword wide
    fn smulwy(&mut self, rd: usize, rm: usize, rs: usize, y: bool) -> usize {
        let op1 = self.read_reg(rm) as u64;
        let op2 = if y {u32::hi(self.read_reg(rs))} else {u32::lo(self.read_reg(rs))} as u64;
        let result = op1 * op2;
        let result_32 = (result >> 16) as u32;
        self.write_reg(rd, result_32);
        0
    }

    /// SMLAxy
    /// Signed multiply halfword and accumulate
    fn smlaxy(&mut self, rd: usize, rm: usize, rs: usize, rn: usize, y: bool, x: bool) -> usize {
        let op1 = if x {u32::hi(self.read_reg(rm))} else {u32::lo(self.read_reg(rm))} as u32;
        let op2 = if y {u32::hi(self.read_reg(rs))} else {u32::lo(self.read_reg(rs))} as u32;
        let mul_result = (op1 * op2) as i32;
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
        let op1 = self.read_reg(rm) as u64;
        let op2 = if y {u32::hi(self.read_reg(rs))} else {u32::lo(self.read_reg(rs))} as u64;
        let mul_result = op1 * op2;
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
        let op1 = if x {u32::hi(self.read_reg(rm))} else {u32::lo(self.read_reg(rm))} as u32;
        let op2 = if y {u32::hi(self.read_reg(rs))} else {u32::lo(self.read_reg(rs))} as u32;
        let mul_result = op1 * op2;
        let acc_op = u64::make(self.read_reg(rd_hi), self.read_reg(rd_lo));
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
        cpsr.set(CPSR::T, u32::test_bit(reg_val, 0));
        self.write_flags(cpsr);

        let current_pc = self.read_reg(PC_REG).wrapping_sub(I_SIZE);
        self.write_reg(LINK_REG, current_pc);

        let dest = reg_val & 0xFFFFFFFE;
        // Call into JIT cache.
        self.call_subroutine(dest);
        0
    }

    /// Thumb BLX
    /// Branch, link, and exchange - using register value
    fn tblxr(&mut self, reg: usize) -> usize {
        let reg_val = self.read_reg(reg);
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::T, u32::test_bit(reg_val, 0));
        self.write_flags(cpsr);

        let current_pc = self.read_reg(PC_REG).wrapping_sub(T_SIZE);
        self.write_reg(LINK_REG, current_pc | 1);

        let dest = reg_val & 0xFFFFFFFE;
        // Call into JIT cache.
        self.call_subroutine(dest);
        0
    }

    // Data transfer

    /// PLD
    /// Pre-load data
    fn pld(&mut self, _transfer_params: TransferParams, _offset: ShiftOperand) -> usize {
        // TODO
        0
    }

    /// LDRD
    /// Load 8 bytes from memory.
    fn ldrd(&mut self, transfer_params: TransferParams, dest_reg: usize, offset: OpData) -> usize {
        let offset = self.eval_op_data(offset);
        let base_addr = self.read_reg(transfer_params.base_reg);
        let offset_addr = if transfer_params.inc {
            base_addr.wrapping_add(offset)  // Inc
        } else {
            base_addr.wrapping_sub(offset)  // Dec
        };
        let transfer_addr = if transfer_params.pre_index {
            offset_addr // Pre
        } else {
            base_addr   // Post
        };

        let (data, cycles_lo) = self.load_word(MemCycleType::N, transfer_addr);
        self.write_reg(dest_reg, data as u32);
        let (data, cycles_hi) = self.load_word(MemCycleType::S, transfer_addr.wrapping_add(4));
        self.write_reg(dest_reg + 1, data as u32);

        if !transfer_params.pre_index || transfer_params.writeback {
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        cycles_lo + cycles_hi
    }

    /// STRD
    /// Store 8 bytes to memory.
    fn strd(&mut self, transfer_params: TransferParams, src_reg: usize, offset: OpData) -> usize {
        let data_lo = self.read_reg(src_reg);
        let data_hi = self.read_reg(src_reg + 1);
        self.next_fetch_non_seq();

        let offset = self.eval_op_data(offset);
        let base_addr = self.read_reg(transfer_params.base_reg);
        let offset_addr = if transfer_params.inc {
            base_addr.wrapping_add(offset)  // Inc
        } else {
            base_addr.wrapping_sub(offset)  // Dec
        };
        let transfer_addr = if transfer_params.pre_index {
            offset_addr // Pre
        } else {
            base_addr   // Post
        };

        let cycles_lo = self.store_word(MemCycleType::N, transfer_addr, data_lo);
        let cycles_hi = self.store_word(MemCycleType::S, transfer_addr.wrapping_add(4), data_hi);

        if !transfer_params.pre_index || transfer_params.writeback {
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        cycles_lo + cycles_hi
    }

    // Coprocessor

    /// MRC2
    /// Move from coprocessor to ARM
    fn mrc2(&mut self, coproc: usize, coproc_reg: usize, arm_reg: usize, op_reg: usize, op: u32, info: u32) -> usize {
        if let Some(c) = self.mut_coproc_v5(coproc) {
            let (data, cycles) = c.mrc2(coproc_reg, op_reg, op, info);
            self.write_reg(arm_reg, data);
            cycles
        } else {
            self.undefined()
        }
    }

    /// MCR2
    /// Move from ARM to coprocessor
    fn mcr2(&mut self, coproc: usize, coproc_reg: usize, arm_reg: usize, op_reg: usize, op: u32, info: u32) -> usize {
        let data = self.read_reg(arm_reg);
        if let Some(c) = self.mut_coproc_v5(coproc) {
            c.mcr2(coproc_reg, op_reg, data, op, info)
        } else {
            self.undefined()
        }
    }

    /// STC2
    /// Store into memory from coprocessor
    /// TODO: transfer len of more than 1?
    fn stc2(&mut self, coproc: usize, transfer_len: bool, transfer_params: TransferParams, offset: u32, coproc_reg: usize) -> usize {
        let base_addr = self.read_reg(transfer_params.base_reg);
        let offset_addr = if transfer_params.inc {
            base_addr.wrapping_add(offset)  // Inc
        } else {
            base_addr.wrapping_sub(offset)  // Dec
        };
        let transfer_addr = if transfer_params.pre_index {
            offset_addr // Pre
        } else {
            base_addr   // Post
        };

        let cycles = if let Some(c) = self.mut_coproc_v5(coproc) {
            let (data, cop_cycles) = c.stc2(transfer_len, coproc_reg);
            let mem_cycles = self.store_word(MemCycleType::N, transfer_addr, data);
            self.next_fetch_non_seq();
            cop_cycles + mem_cycles
        } else {
            self.undefined()
        };

        if !transfer_params.pre_index || transfer_params.writeback {
            // Write offset address back into base register.
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        cycles
    }

    /// LDC2
    /// Load from memory to coprocessor
    /// TODO: transfer len of more than 1?
    fn ldc2(&mut self, coproc: usize, transfer_len: bool, transfer_params: TransferParams, offset: u32, coproc_reg: usize) -> usize {
        let base_addr = self.read_reg(transfer_params.base_reg);
        let offset_addr = if transfer_params.inc {
            base_addr.wrapping_add(offset)  // Inc
        } else {
            base_addr.wrapping_sub(offset)  // Dec
        };
        let transfer_addr = if transfer_params.pre_index {
            offset_addr // Pre
        } else {
            base_addr   // Post
        };

        let (data, mem_cycles) = self.load_word(MemCycleType::N, transfer_addr);
        let cycles = if let Some(c) = self.mut_coproc_v5(coproc) {
            c.ldc2(transfer_len, coproc_reg, data) + mem_cycles
        } else {
            self.undefined()
        };

        if !transfer_params.pre_index || transfer_params.writeback {
            // Write offset address back into base register.
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        cycles
    }

    /// CDP2
    /// Coprocessor data operation
    fn cdp2(&mut self, op: u32, reg_n: usize, reg_d: usize, info: u32, reg_m: usize, coproc: usize) -> usize {
        if let Some(c) = self.mut_coproc_v5(coproc) {
            c.cdp2(op, reg_n, reg_d, info, reg_m)
        } else {
            self.undefined()
        }
    }

    /// MRRC
    /// Move double from coprocessor to ARM
    fn mrrc(&mut self, coproc: usize, arm_reg_n: usize, arm_reg_d: usize, op_reg: usize, op: u32) -> usize {
        if let Some(c) = self.mut_coproc_v5(coproc) {
            let (data_lo, data_hi, cycles) = c.mrrc(op_reg, op);
            self.write_reg(arm_reg_d, data_lo);
            self.write_reg(arm_reg_n, data_hi);
            cycles
        } else {
            self.undefined()
        }
    }

    /// MCRR
    /// Move double from ARM to coprocessor
    fn mcrr(&mut self, coproc: usize, arm_reg_n: usize, arm_reg_d: usize, op_reg: usize, op: u32) -> usize {
        let data_lo = self.read_reg(arm_reg_d);
        let data_hi = self.read_reg(arm_reg_n);
        if let Some(c) = self.mut_coproc_v5(coproc) {
            c.mcrr(op_reg, data_lo, data_hi, op)
        } else {
            self.undefined()
        }
    }
}
