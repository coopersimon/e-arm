
use super::{
    instructions::*,
    mul_cycles
};
use crate::{
    common::{
        u32::*,
        u64
    },
    core::{ARMCore, CPSR, constants::*},
    memory::{Mem32, MemCycleType}
};

/// Execution of ARMv4 instructions.
pub trait ARMv4<M: Mem32<Addr = u32>>: ARMCore<M> {

    /// SWI
    /// Software interrupt
    fn swi(&mut self, comment: u32) -> usize {
        if !self.try_swi_hook(comment) {
            self.software_exception();
        }
        0
    }

    /// Called when an undefined instruction is encountered.
    /// Returns the amount of additional cycles taken.
    fn undefined(&mut self) -> usize {
        let pc = self.read_reg(PC_REG) - 8;
        let (val, _) = self.load_word(MemCycleType::S, pc);
        panic!("undefined: {:X} @ {:X}", val, pc);
        //self.undefined_exception();
        //0
    }

    // Shifts

    /// LSL
    /// Logical shift left (fill with zeroes)
    fn lsl(&mut self, set_carry: bool, val: u32, shift_amount: u32) -> u32 {
        if set_carry && shift_amount != 0 {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::C, test_bit(val, (32 - shift_amount) as usize));
            self.write_flags(cpsr);
        }
        val.wrapping_shl(shift_amount)
    }

    /// LSR
    /// Logical shift right (fill with zeroes)
    fn lsr(&mut self, set_carry: bool, val: u32, shift_amount: u32) -> u32 {
        if set_carry {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::C, test_bit(val, (shift_amount - 1) as usize));
            self.write_flags(cpsr);
        }
        val.wrapping_shr(shift_amount)
    }

    /// LSR #32
    /// Logical shift right by 32 bits (fill with zeroes)
    fn lsr_32(&mut self, set_carry: bool, val: u32) -> u32 {
        if set_carry {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::C, test_bit(val, 31));
            self.write_flags(cpsr);
        }
        0
    }

    /// ASR
    /// Arithmetic shift right (sign-extend)
    fn asr(&mut self, set_carry: bool, val: u32, shift_amount: u32) -> u32 {
        if set_carry {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::C, test_bit(val, (shift_amount - 1) as usize));
            self.write_flags(cpsr);
        }
        (val as i32).wrapping_shr(shift_amount) as u32
    }

    /// ASR # 32
    /// Arithmetic shift right by 32 bits (sign-extend)
    fn asr_32(&mut self, set_carry: bool, val: u32) -> u32 {
        if set_carry {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::C, test_bit(val, 31));
            self.write_flags(cpsr);
        }
        (val as i32).wrapping_shr(31) as u32
    }

    /// ROR
    /// Rotate right
    fn ror(&mut self, set_carry: bool, val: u32, shift_amount: u32) -> u32 {
        let result = val.rotate_right(shift_amount);
        if set_carry {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::C, test_bit(result, 31));
            self.write_flags(cpsr);
        }
        result
    }

    /// RRX
    /// Rotate right with carry.
    fn rrx(&mut self, set_carry: bool, val: u32) -> u32 {
        let mut cpsr = self.read_cpsr();
        let carry = if cpsr.contains(CPSR::C) {bit(31)} else {0};
        if set_carry {
            cpsr.set(CPSR::C, test_bit(val, 0));
            self.write_flags(cpsr);
        }
        (val >> 1) | carry
    }

    // Logic

    /// Writeback a data processing instruction.
    /// Also sets the n and z flags optionally.
    fn writeback(&mut self, s: bool, rd: usize, result: u32) {
        self.write_reg(rd, result);
        if s {
            if rd == PC_REG {
                self.return_from_exception();
            } else {
                let mut cpsr = self.read_cpsr();
                cpsr.set(CPSR::N, test_bit(result, 31));
                cpsr.set(CPSR::Z, result == 0);
                self.write_flags(cpsr);
            }
        }
    }

    /// AND
    /// Bitwise AND
    fn and(&mut self, s: bool, rd: usize, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, s);
        let op1 = self.read_reg(rn);
        let result = op1 & op2;
        self.writeback(s, rd, result);
        cycles
    }

    /// EOR
    /// Bitwise exclusive OR (xor)
    fn eor(&mut self, s: bool, rd: usize, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, s);
        let op1 = self.read_reg(rn);
        let result = op1 ^ op2;
        self.writeback(s, rd, result);
        cycles
    }

    /// ORR
    /// Bitwise inclusive OR
    fn orr(&mut self, s: bool, rd: usize, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, s);
        let op1 = self.read_reg(rn);
        let result = op1 | op2;
        self.writeback(s, rd, result);
        cycles
    }

    /// BIC
    /// NOT op2 with AND
    fn bic(&mut self, s: bool, rd: usize, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, s);
        let op1 = self.read_reg(rn);
        let result = op1 & !op2;
        self.writeback(s, rd, result);
        cycles
    }

    /// MOV
    /// Move op2 into rd.
    fn mov(&mut self, s: bool, rd: usize, op2: ALUOperand) -> usize {
        let (data, cycles) = self.eval_alu_op(op2, s);
        self.writeback(s, rd, data);
        cycles
    }

    /// MVN
    /// Move NOT op2 into rd.
    fn mvn(&mut self, s: bool, rd: usize, op2: ALUOperand) -> usize {
        let (data, cycles) = self.eval_alu_op(op2, s);
        self.writeback(s, rd, !data);
        cycles
    }

    // Comparisons

    /// TST
    /// Bitwise AND and set flags.
    fn tst(&mut self, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, true);
        let op1 = self.read_reg(rn);
        let result = op1 & op2;
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::N, test_bit(result, 31));
        cpsr.set(CPSR::Z, result == 0);
        self.write_flags(cpsr);
        cycles
    }

    /// TEQ
    /// Bitwise XOR and set flags.
    fn teq(&mut self, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, true);
        let op1 = self.read_reg(rn);
        let result = op1 ^ op2;
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::N, test_bit(result, 31));
        cpsr.set(CPSR::Z, result == 0);
        self.write_flags(cpsr);
        cycles
    }

    /// CMP
    /// Arithmetic sub and set flags.
    fn cmp(&mut self, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, false);
        let op1 = self.read_reg(rn);
        let (result, overflow) = op1.overflowing_sub(op2);
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::N, test_bit(result, 31));
        cpsr.set(CPSR::Z, result == 0);
        cpsr.set(CPSR::C, !overflow);
        cpsr.set(CPSR::V, test_bit((op1 ^ op2) & (op1 ^ result), 31));
        self.write_flags(cpsr);
        cycles
    }

    /// CMP
    /// Arithmetic add and set flags.
    fn cmn(&mut self, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, false);
        let op1 = self.read_reg(rn);
        let (result, overflow) = op1.overflowing_add(op2);
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::N, test_bit(result, 31));
        cpsr.set(CPSR::Z, result == 0);
        cpsr.set(CPSR::C, overflow);
        cpsr.set(CPSR::V, test_bit(!(op1 ^ op2) & (op1 ^ result), 31));
        self.write_flags(cpsr);
        cycles
    }

    // Arithmetic

    /// ADD
    /// Arithmetic add without carry.
    fn add(&mut self, s: bool, rd: usize, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, false);
        let op1 = self.read_reg(rn);
        let (result, overflow) = op1.overflowing_add(op2);
        self.write_reg(rd, result);
        if s {
            if rd == PC_REG {
                self.return_from_exception();
            } else {
                let mut cpsr = self.read_cpsr();
                cpsr.set(CPSR::N, test_bit(result, 31));
                cpsr.set(CPSR::Z, result == 0);
                cpsr.set(CPSR::C, overflow);
                cpsr.set(CPSR::V, test_bit(!(op1 ^ op2) & (op1 ^ result), 31));
                self.write_flags(cpsr);
            }
        }
        cycles
    }

    /// ADD Rd,PC,#
    /// Arithmetic add constant to PC. (thumb)
    fn taddpc(&mut self, rd: usize, op2: u32) -> usize {
        let op1 = self.read_reg(PC_REG) & 0xFFFF_FFFC;
        let result = op1.wrapping_add(op2);
        self.write_reg(rd, result);
        0
    }

    /// SUB
    /// Arithmetic subtract without carry.
    fn sub(&mut self, s: bool, rd: usize, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, false);
        let op1 = self.read_reg(rn);
        let (result, overflow) = op1.overflowing_sub(op2);
        self.write_reg(rd, result);
        if s {
            if rd == PC_REG {
                self.return_from_exception();
            } else {
                let mut cpsr = self.read_cpsr();
                cpsr.set(CPSR::N, test_bit(result, 31));
                cpsr.set(CPSR::Z, result == 0);
                cpsr.set(CPSR::C, !overflow);
                cpsr.set(CPSR::V, test_bit((op1 ^ op2) & (op1 ^ result), 31));
                self.write_flags(cpsr);
            }
        }
        cycles
    }

    /// RSB
    /// Arithmetic subtract without carry.
    fn rsb(&mut self, s: bool, rd: usize, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, false);
        let op1 = self.read_reg(rn);
        let (result, overflow) = op2.overflowing_sub(op1);
        self.write_reg(rd, result);
        if s {
            if rd == PC_REG {
                self.return_from_exception();
            } else {
                let mut cpsr = self.read_cpsr();
                cpsr.set(CPSR::N, test_bit(result, 31));
                cpsr.set(CPSR::Z, result == 0);
                cpsr.set(CPSR::C, !overflow);
                cpsr.set(CPSR::V, test_bit((op1 ^ op2) & (op1 ^ result), 31));
                self.write_flags(cpsr);
            }
        }
        cycles
    }

    /// ADC
    /// Arithmetic add with carry.
    fn adc(&mut self, s: bool, rd: usize, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, false);
        let op1 = self.read_reg(rn);
        self.arithmetic_carry(s, rd, op1, op2);
        cycles
    }

    /// SBC
    /// Arithmetic subtract with borrow.
    fn sbc(&mut self, s: bool, rd: usize, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, false);
        let op1 = self.read_reg(rn);
        self.arithmetic_carry(s, rd, op1, !op2);
        cycles
    }

    /// RSC
    /// Reverse subtract with borrow.
    fn rsc(&mut self, s: bool, rd: usize, rn: usize, op2: ALUOperand) -> usize {
        let (op2, cycles) = self.eval_alu_op(op2, false);
        let op1 = self.read_reg(rn);
        self.arithmetic_carry(s, rd, op2, !op1);
        cycles
    }

    /// Do an arithmetic add / subtract with carry / borrow.
    /// Bitwise invert op2 to do subtract.
    fn arithmetic_carry(&mut self, s: bool, rd: usize, op1: u32, op2: u32) {
        let mut cpsr = self.read_cpsr();
        let (r1, o1) = op1.overflowing_add(op2);
        let (result, o2) = r1.overflowing_add(cpsr.carry());
        self.write_reg(rd, result);
        if s {
            if rd == PC_REG {
                self.return_from_exception();
            } else {
                cpsr.set(CPSR::N, test_bit(result, 31));
                cpsr.set(CPSR::Z, result == 0);
                cpsr.set(CPSR::C, o1 || o2);
                cpsr.set(CPSR::V, test_bit(!(op1 ^ op2) & (op1 ^ result), 31));
                self.write_flags(cpsr);
            }
        }
    }

    // Multiplication

    /// MUL
    /// Multiply
    fn mul(&mut self, s: bool, rd: usize, rs: usize, rm: usize) -> usize {
        let op1 = self.read_reg(rm);
        let op2 = self.read_reg(rs);
        let result = op1.wrapping_mul(op2);
        self.write_reg(rd, result);
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.remove(CPSR::C);
            self.write_flags(cpsr);
        }
        mul_cycles(op2)
    }

    /// MLA
    /// Multiply and accumulate
    fn mla(&mut self, s: bool, rd: usize, rn: usize, rs: usize, rm: usize) -> usize {
        let op1 = self.read_reg(rm);
        let op2 = self.read_reg(rs);
        let mul_result = op1.wrapping_mul(op2);
        let result = mul_result.wrapping_add(self.read_reg(rn));
        self.write_reg(rd, result);
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.remove(CPSR::C);
            self.write_flags(cpsr);
        }
        mul_cycles(op2) + 1
    }

    /// UMULL
    /// Unsigned long multiply
    fn umull(&mut self, s: bool, rd_hi: usize, rd_lo: usize, rn: usize, rm: usize) -> usize {
        let op1 = self.read_reg(rm) as u64;
        let op2 = self.read_reg(rn);
        let op2_64 = op2 as u64;
        let result = op1.wrapping_mul(op2_64);
        self.write_reg(rd_lo, u64::lo(result));
        self.write_reg(rd_hi, u64::hi(result));
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, u64::test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.remove(CPSR::C);
            self.write_flags(cpsr);
        }
        mul_cycles(op2) + 1
    }

    /// UMLAL
    /// Unsigned long multiply and accumulate
    fn umlal(&mut self, s: bool, rd_hi: usize, rd_lo: usize, rn: usize, rm: usize) -> usize {
        let op1 = self.read_reg(rm) as u64;
        let op2 = self.read_reg(rn);
        let op2_64 = op2 as u64;
        let mul_result = op1.wrapping_mul(op2_64);
        let acc_op = u64::make(self.read_reg(rd_hi), self.read_reg(rd_lo));
        let result = mul_result.wrapping_add(acc_op);
        self.write_reg(rd_lo, u64::lo(result));
        self.write_reg(rd_hi, u64::hi(result));
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, u64::test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.remove(CPSR::C);
            self.write_flags(cpsr);
        }
        mul_cycles(op2) + 2
    }

    /// SMULL
    /// Signed long multiply
    fn smull(&mut self, s: bool, rd_hi: usize, rd_lo: usize, rn: usize, rm: usize) -> usize {
        let op1 = (self.read_reg(rm) as i32) as i64;
        let op2 = self.read_reg(rn);
        let op2_64 = (op2 as i32) as i64;
        let result = op1.wrapping_mul(op2_64) as u64;
        self.write_reg(rd_lo, u64::lo(result));
        self.write_reg(rd_hi, u64::hi(result));
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, u64::test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.remove(CPSR::C);
            self.write_flags(cpsr);
        }
        mul_cycles(op2) + 1
    }

    /// SMLAL
    /// Signed long multiply and accumulate
    fn smlal(&mut self, s: bool, rd_hi: usize, rd_lo: usize, rn: usize, rm: usize) -> usize {
        let op1 = (self.read_reg(rm) as i32) as i64;
        let op2 = self.read_reg(rn);
        let op2_64 = (op2 as i32) as i64;
        let mul_result = op1.wrapping_mul(op2_64) as u64;
        let acc_op = u64::make(self.read_reg(rd_hi), self.read_reg(rd_lo));
        let result = mul_result.wrapping_add(acc_op);
        self.write_reg(rd_lo, u64::lo(result));
        self.write_reg(rd_hi, u64::hi(result));
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, u64::test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.remove(CPSR::C);
            self.write_flags(cpsr);
        }
        mul_cycles(op2) + 2
    }

    // Branch

    /// B
    /// Branch
    fn b(&mut self, offset: u32) -> usize {
        let current_pc = self.read_reg(PC_REG).wrapping_sub(I_SIZE);
        self.do_branch(current_pc.wrapping_add(offset));
        // Branches themselves take 0 additional cycles.
        // The pipeline however is flushed which will lead to extra cycles.
        // Therefore it will take a few additional steps to start executing again.
        0
    }

    /// Thumb B
    /// Branch
    fn tb(&mut self, offset: u32) -> usize {
        let current_pc = self.read_reg(PC_REG).wrapping_sub(T_SIZE);
        self.do_branch(current_pc.wrapping_add(offset));
        0
    }

    /// BL
    /// Branch and link (using r14)
    fn bl(&mut self, offset: u32) -> usize {
        let current_pc = self.read_reg(PC_REG).wrapping_sub(I_SIZE);
        self.write_reg(LINK_REG, current_pc);
        // Call into JIT cache.
        self.call_subroutine(self.read_reg(PC_REG).wrapping_add(offset), I_SIZE);
        0
    }

    /// BX
    /// Branch and exchange - switch to Thumb ISA
    fn bx(&mut self, reg: usize) -> usize {
        let reg_val = self.read_reg(reg);
        let mut cpsr = self.read_cpsr();
        let src_i_size = cpsr.instr_size();
        cpsr.set(CPSR::T, test_bit(reg_val, 0));
        self.write_flags(cpsr);

        let dest = reg_val & 0xFFFFFFFE;
        self.do_branch(dest.wrapping_sub(src_i_size));
        0
    }

    /// Thumb BL (low halfword)
    /// Branch and link. First part of two instructions.
    fn tbl_lo(&mut self, offset: u32) -> usize {
        let target_addr = self.read_reg(PC_REG).wrapping_add(offset);
        self.write_reg(LINK_REG, target_addr);
        // To ensure an interrupt doesn't happen between here and the next instruction,
        // we must briefly disable interrupts.
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::BLI, cpsr.contains(CPSR::I));
        cpsr.insert(CPSR::I);
        self.write_cpsr(cpsr);
        0
    }

    /// Thumb BL (high halfword)
    /// Branch and link. Second part of two instructions.
    fn tbl_hi(&mut self, offset: u32) -> usize {
        let return_addr = self.read_reg(PC_REG).wrapping_sub(T_SIZE);
        let dest = self.read_reg(LINK_REG).wrapping_add(offset);
        self.write_reg(LINK_REG, return_addr | 1);
        // We must re-enable interrupts if they were enabled before.
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::I, cpsr.contains(CPSR::BLI));
        cpsr.remove(CPSR::BLI);
        self.write_cpsr(cpsr);
        // Call into JIT cache.
        self.call_subroutine(dest, T_SIZE);
        0
    }

    // Data transfer

    /// MRS
    /// Move program status register into general purpose register
    fn mrs(&mut self, spsr: bool, rd: usize) -> usize {
        if spsr {
            self.write_reg(rd, self.read_spsr().bits());
        } else {
            self.write_reg(rd, self.read_cpsr().bits());
        }
        0
    }

    /// MSR
    /// Move general purpose register into program status register
    fn msr(&mut self, spsr: bool, mask: u32, op: OpData) -> usize {
        let data = self.eval_op_data(op);
        if spsr {
            let old_spsr = self.read_spsr().bits() & !mask;
            let new_spsr = CPSR::from_bits_truncate((data & mask) | old_spsr);
            self.write_spsr(new_spsr);
        } else {
            let old_cpsr = self.read_cpsr().bits() & !mask;
            let new_cpsr = CPSR::from_bits_truncate((data & mask) | old_cpsr);
            self.write_cpsr(new_cpsr);
        }
        0
    }

    /// SWP
    /// Single data swap (word).
    fn swp(&mut self, rn: usize, rd: usize, rm: usize) -> usize {
        let addr = self.read_reg(rn);
        let reg_data = self.read_reg(rm);
        let (mem_data, load_cycles) = self.load_word(MemCycleType::N, addr);
        let store_cycles = self.store_word(MemCycleType::N, addr, reg_data);
        self.write_reg(rd, mem_data);
        load_cycles + store_cycles + 1
    }

    /// SWPB
    /// Single data swap (byte).
    fn swpb(&mut self, rn: usize, rd: usize, rm: usize) -> usize {
        let addr = self.read_reg(rn);
        let reg_data = self.read_reg(rm);
        let (mem_data, load_cycles) = self.load_byte(MemCycleType::N, addr);
        let store_cycles = self.store_byte(MemCycleType::N, addr, reg_data as u8);
        self.write_reg(rd, mem_data as u32);
        load_cycles + store_cycles + 1
    }

    /// LDR
    /// Load a single word from memory and store it in a register.
    fn ldr(&mut self, transfer_params: TransferParams, dest_reg: usize, offset: ShiftOperand) -> usize {
        let offset = self.eval_shift_op(offset, false);
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

        let (data, cycles) = self.load_word(MemCycleType::N, transfer_addr);
        self.write_reg(dest_reg, data);

        if !transfer_params.pre_index || transfer_params.writeback {
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        // Loads take one extra internal cycle.
        cycles + 1
    }

    /// Thumb LDR PC-relative
    fn tldrpc(&mut self, dest_reg: usize, offset: u32) -> usize {
        let base_addr = self.read_reg(PC_REG) & 0xFFFF_FFFC;
        let transfer_addr = base_addr.wrapping_add(offset);
        let (data, cycles) = self.load_word(MemCycleType::N, transfer_addr);
        self.write_reg(dest_reg, data);
        cycles + 1
    }

    /// LDRB
    /// Load a single byte from memory and store it in a register.
    fn ldrb(&mut self, transfer_params: TransferParams, dest_reg: usize, offset: ShiftOperand) -> usize {
        let offset = self.eval_shift_op(offset, false);
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

        let (data, cycles) = self.load_byte(MemCycleType::N, transfer_addr);
        self.write_reg(dest_reg, data as u32);

        if !transfer_params.pre_index || transfer_params.writeback {
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        // Loads take one extra internal cycle.
        cycles + 1
    }

    /// STR
    /// Store a single word into memory.
    fn str(&mut self, transfer_params: TransferParams, src_reg: usize, offset: ShiftOperand) -> usize {
        let data = if src_reg == PC_REG {
            self.read_reg(src_reg).wrapping_add(4)  // TODO: +2 for thumb...
        } else {
            self.read_reg(src_reg)
        };
        self.next_fetch_non_seq();

        let offset = self.eval_shift_op(offset, false);
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

        let cycles = self.store_word(MemCycleType::N, transfer_addr, data);

        if !transfer_params.pre_index || transfer_params.writeback {
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        cycles
    }

    /// STRB
    /// Store a single byte into memory.
    fn strb(&mut self, transfer_params: TransferParams, src_reg: usize, offset: ShiftOperand) -> usize {
        let data = if src_reg == PC_REG {
            self.read_reg(src_reg).wrapping_add(4)  // TODO: +2 for thumb...
        } else {
            self.read_reg(src_reg)
        };
        self.next_fetch_non_seq();

        let offset = self.eval_shift_op(offset, false);
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

        let cycles = self.store_byte(MemCycleType::N, transfer_addr, data as u8);

        if !transfer_params.pre_index || transfer_params.writeback {
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        cycles
    }

    /// LDRH
    /// Load 2 bytes from memory.
    fn ldrh(&mut self, transfer_params: TransferParams, dest_reg: usize, offset: OpData) -> usize {
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

        let (data, cycles) = self.load_halfword(MemCycleType::N, transfer_addr);
        self.write_reg(dest_reg, data as u32);

        if !transfer_params.pre_index || transfer_params.writeback {
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        // Loads take one extra internal cycle.
        cycles + 1
    }

    /// LDRSB
    /// Load byte from memory, and sign-extend.
    fn ldrsb(&mut self, transfer_params: TransferParams, dest_reg: usize, offset: OpData) -> usize {
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

        let (data, cycles) = self.load_byte(MemCycleType::N, transfer_addr);
        let signed_data = (data as i8) as i32;
        self.write_reg(dest_reg, signed_data as u32);

        if !transfer_params.pre_index || transfer_params.writeback {
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        // Loads take one extra internal cycle.
        cycles + 1
    }

    /// LDRSH
    /// Load halfword from memory, and sign-extend.
    fn ldrsh(&mut self, transfer_params: TransferParams, dest_reg: usize, offset: OpData) -> usize {
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

        let (data, cycles) = self.load_halfword(MemCycleType::N, transfer_addr);
        let signed_data = (data as i16) as i32;
        self.write_reg(dest_reg, signed_data as u32);

        if !transfer_params.pre_index || transfer_params.writeback {
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        // Loads take one extra internal cycle.
        cycles + 1
    }

    /// STRH
    /// Store 2 bytes into memory.
    fn strh(&mut self, transfer_params: TransferParams, src_reg: usize, offset: OpData) -> usize {
        let data = if src_reg == PC_REG {
            self.read_reg(src_reg).wrapping_add(4)  // TODO: +2 for thumb...
        } else {
            self.read_reg(src_reg)
        };
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

        let cycles = self.store_halfword(MemCycleType::N, transfer_addr, data as u16);

        if !transfer_params.pre_index || transfer_params.writeback {
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        cycles
    }

    /// LDM
    /// Block load from memory into registers.
    /// Start from the address in the base reg. Load each register in low-high.
    /// Adds 4 to the address before loading if pre == true.
    /// 
    /// If s == true, then the processor will return from exception if PC is loaded,
    /// or it will transfer into user registers.
    fn ldm(&mut self, transfer_params: TransferParams, reg_list: u32, s: bool) -> usize {
        let load_from_user = s && !test_bit(reg_list, PC_REG);

        let base_addr = self.read_reg(transfer_params.base_reg);
        let offset = reg_list.count_ones() * 4;

        // Address to start transferring, and address to write back into register.
        // The lowest address is always the start address.
        let (mut transfer_addr, writeback_addr) = if transfer_params.inc {
            (base_addr, base_addr.wrapping_add(offset))
        } else {
            let low_addr = base_addr.wrapping_sub(offset);
            (low_addr, low_addr)
        };

        let mut access_type = MemCycleType::N;
        let cycles = if transfer_params.pre_index == transfer_params.inc {
            (0..16).filter(|reg| test_bit(reg_list, *reg)).fold(0, |acc, reg| {
                transfer_addr += 4;
                let (data, cycles) = self.load_word_force_align(access_type, transfer_addr);
                access_type = MemCycleType::S;
                if load_from_user {
                    self.write_usr_reg(reg, data);
                } else {
                    self.write_reg(reg, data);
                }
                acc + cycles
            }) + 1
        } else {
            (0..16).filter(|reg| test_bit(reg_list, *reg)).fold(0, |acc, reg| {
                let (data, cycles) = self.load_word_force_align(access_type, transfer_addr);
                access_type = MemCycleType::S;
                if load_from_user {
                    self.write_usr_reg(reg, data);
                } else {
                    self.write_reg(reg, data);
                }
                transfer_addr += 4;
                acc + cycles
            }) + 1
        };

        if transfer_params.writeback {
            self.write_reg(transfer_params.base_reg, writeback_addr);
        }

        if s && test_bit(reg_list, PC_REG) {
            self.return_from_exception();
        }
        cycles
    }

    /// STM
    /// Block store from registers into memory.
    /// Start from the base address (addr). Store each register in low-high.
    /// Adds 4 to the address before storing if pre == true.
    /// 
    /// If s == true, then the processor will transfer from user registers.
    fn stm(&mut self, transfer_params: TransferParams, reg_list: u32, s: bool) -> usize {
        self.next_fetch_non_seq();
        let load_from_user = s && !test_bit(reg_list, PC_REG);

        let base_addr = self.read_reg(transfer_params.base_reg);
        let offset = reg_list.count_ones() * 4;
        
        // Address to start transferring, and address to write back into register.
        // The lowest address is always the start address.
        let (mut transfer_addr, writeback_addr) = if transfer_params.inc {
            (base_addr, base_addr.wrapping_add(offset))
        } else {
            let low_addr = base_addr.wrapping_sub(offset);
            (low_addr, low_addr)
        };

        let mut access_type = MemCycleType::N;
        let cycles = if transfer_params.pre_index == transfer_params.inc {
            (0..16).filter(|reg| test_bit(reg_list, *reg)).fold(0, |acc, reg| {
                transfer_addr += 4;
                let data = if load_from_user {
                    self.read_usr_reg(reg)
                } else {
                    self.read_reg(reg)
                };
                let cycles = self.store_word(access_type, transfer_addr, data);
                access_type = MemCycleType::S;
                acc + cycles
            }) + 1
        } else {
            (0..16).filter(|reg| test_bit(reg_list, *reg)).fold(0, |acc, reg| {
                let data = if load_from_user {
                    self.read_usr_reg(reg)
                } else {
                    self.read_reg(reg)
                };
                let cycles = self.store_word(access_type, transfer_addr, data);
                access_type = MemCycleType::S;
                transfer_addr += 4;
                acc + cycles
            }) + 1
        };

        if transfer_params.writeback {
            self.write_reg(transfer_params.base_reg, writeback_addr);
        }

        cycles
    }

    // Coprocessor

    /// MRC
    /// Move from coprocessor to ARM
    fn mrc(&mut self, coproc: usize, coproc_reg: usize, arm_reg: usize, op_reg: usize, op: u32, info: u32) -> usize {
        if let Some(c) = self.mut_coproc(coproc) {
            let (data, cycles) = c.mrc(coproc_reg, op_reg, op, info);
            self.write_reg(arm_reg, data);
            cycles
        } else {
            self.undefined()
        }
    }

    /// MCR
    /// Move from ARM to coprocessor
    fn mcr(&mut self, coproc: usize, coproc_reg: usize, arm_reg: usize, op_reg: usize, op: u32, info: u32) -> usize {
        let data = self.read_reg(arm_reg);
        if let Some(c) = self.mut_coproc(coproc) {
            c.mcr(coproc_reg, op_reg, data, op, info)
        } else {
            self.undefined()
        }
    }

    /// STC
    /// Store into memory from coprocessor
    /// TODO: transfer len of more than 1?
    fn stc(&mut self, coproc: usize, transfer_len: bool, transfer_params: TransferParams, offset: u32, coproc_reg: usize) -> usize {
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

        let cycles = if let Some(c) = self.mut_coproc(coproc) {
            let (data, cop_cycles) = c.stc(transfer_len, coproc_reg);
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

    /// LDC
    /// Load from memory to coprocessor
    /// TODO: transfer len of more than 1?
    fn ldc(&mut self, coproc: usize, transfer_len: bool, transfer_params: TransferParams, offset: u32, coproc_reg: usize) -> usize {
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
        let cycles = if let Some(c) = self.mut_coproc(coproc) {
            c.ldc(transfer_len, coproc_reg, data) + mem_cycles
        } else {
            self.undefined()
        };

        if !transfer_params.pre_index || transfer_params.writeback {
            // Write offset address back into base register.
            self.write_reg(transfer_params.base_reg, offset_addr);
        }

        cycles
    }

    /// CDP
    /// Coprocessor data operation
    fn cdp(&mut self, op: u32, reg_n: usize, reg_d: usize, info: u32, reg_m: usize, coproc: usize) -> usize {
        if let Some(c) = self.mut_coproc(coproc) {
            c.cdp(op, reg_n, reg_d, info, reg_m)
        } else {
            self.undefined()
        }
    }

    // Helpers

    /// Evaluate the second operand of the ALU.
    /// 
    /// Returns the value and how many cycles it took to eval.
    /// The carry flag might be set. If it should be, `set_carry` should be `true`.
    fn eval_alu_op(&mut self, op: ALUOperand, set_carry: bool) -> (u32, usize) {
        use ALUOperand::*;
        use RegShiftOperand::*;
        match op {
            Normal(n) => (self.eval_shift_op(n, set_carry), 0),
            RegShift{op, shift_reg, reg} => {
                let shift = self.read_reg(reg);
                let shift_amount = self.read_reg(shift_reg) & 0xFF;
                let set_carry = set_carry && (shift_amount != 0);
                (match op {
                    LSL => if shift_amount <= 32 {
                        self.lsl(set_carry, shift, shift_amount)
                    } else {
                        0
                    },
                    LSR => if shift_amount < 32 {
                        self.lsr(set_carry, shift, shift_amount)
                    } else {
                        self.lsr_32(set_carry, shift)
                    },
                    ASR => if shift_amount < 32 {
                        self.asr(set_carry, shift, shift_amount)
                    } else {
                        self.asr_32(set_carry, shift)
                    },
                    ROR => self.ror(set_carry, shift, shift_amount),
                }, 1)
            }
        }
    }

    /// Evaluate the operand.
    /// 
    /// The carry flag might be set. If it should be, `set_carry` should be `true`.
    /// This should be the case for instructions that set flags but don't set the carry.
    fn eval_shift_op(&mut self, op: ShiftOperand, set_carry: bool) -> u32 {
        use ShiftOperand::*;
        match op {
            Immediate(i) => i,
            Register(reg) => self.read_reg(reg),
            LSL{shift_amount, reg} => self.lsl(set_carry, self.read_reg(reg), shift_amount),
            LSR{shift_amount, reg} => self.lsr(set_carry, self.read_reg(reg), shift_amount),
            ASR{shift_amount, reg} => self.asr(set_carry, self.read_reg(reg), shift_amount),
            ROR{shift_amount, reg} => self.ror(set_carry, self.read_reg(reg), shift_amount),
            LSR32{reg} => self.lsr_32(set_carry, self.read_reg(reg)),
            ASR32{reg} => self.asr_32(set_carry, self.read_reg(reg)),
            RRX{reg} => self.rrx(set_carry, self.read_reg(reg)),
        }
    }

    /// Evaluate the operand.
    fn eval_op_data(&mut self, op: OpData) -> u32 {
        use OpData::*;
        match op {
            Immediate(i) => i,
            Register(reg) => self.read_reg(reg),
        }
    }
}