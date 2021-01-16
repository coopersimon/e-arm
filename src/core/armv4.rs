/// ARMv4 Instruction Set

use super::{
    constants::*,
    utils::mul_cycles,
    *
};
use crate::common::{
    u32::*,
    u64, lo_64, hi_64, make_64
};
use crate::memory::Mem32;

/*mod Cond {
    const EQ: u32 = 0;     // Z set
    const NE: u32 = 1;     // Z clear
    const CS: u32 = 2;     // C set
    const CC: u32 = 3;     // C clear
    const MI: u32 = 4;     // N set
    const PL: u32 = 5;     // N clear
    const VS: u32 = 6;     // V set
    const VC: u32 = 7;     // V clear
    const HI: u32 = 8;     // C set and Z clear
    const LS: u32 = 9;     // C clear or Z set
    const GE: u32 = 10;    // N xnor V
    const LT: u32 = 11;    // N xor V
    const GT: u32 = 12;    // Z clear and (N xnor V)
    const LE: u32 = 13;    // Z set or (N xor V)
    const AL: u32 = 14;    // Always
}*/

pub trait ARMv4: ARMCore + Mem32<Addr = u32> {
    /// Decode and execute the instruction.
    /// 
    /// Returns the number of cycles needed to execute it.
    /// This includes any internal (I) cycles and non-seq loads and stores (N).
    /// It does _not_ include the initial fetch cycles (S) or any pipeline flush stall cycles.
    fn execute_instruction(&mut self, i: u32) -> usize {
        const COPROC: u32 = 0b11 << 26;
        const BRANCH: u32 = 0b10 << 26;
        const TRANSFER: u32 = 0b01 << 26;
        const ALU: u32 = 0b00 << 26;
        if self.check_cond(i >> 28) {
            match i & bits(26, 27) {
                COPROC      => self.decode_coproc(i),
                BRANCH      => self.decode_branch(i),
                TRANSFER    => self.decode_transfer(i),
                ALU         => self.decode_alu(i),
                _ => unreachable!(),
            }
        } else {
            0
        }
    }

    /// Check if the instruction should run based on the condition.
    fn check_cond(&self, i: u32) -> bool {
        match i {
            0x0 => self.read_cpsr().contains(CPSR::Z),  // EQ
            0x1 => !self.read_cpsr().contains(CPSR::Z), // NE
            0x2 => self.read_cpsr().contains(CPSR::C),  // CS
            0x3 => !self.read_cpsr().contains(CPSR::C), // CC
            0x4 => self.read_cpsr().contains(CPSR::N),  // MI
            0x5 => !self.read_cpsr().contains(CPSR::N), // PL
            0x6 => self.read_cpsr().contains(CPSR::V),  // VS
            0x7 => !self.read_cpsr().contains(CPSR::V), // VC
            0x8 => {    // HI
                let cpsr = self.read_cpsr();
                cpsr.contains(CPSR::C) && !cpsr.contains(CPSR::Z)
            },
            0x9 => {    // LS
                let cpsr = self.read_cpsr();
                !cpsr.contains(CPSR::C) || cpsr.contains(CPSR::Z)
            },
            0xA => {    // GE
                let cpsr = self.read_cpsr();
                cpsr.contains(CPSR::N) == cpsr.contains(CPSR::V)
            },
            0xB => {    // LT
                let cpsr = self.read_cpsr();
                cpsr.contains(CPSR::N) != cpsr.contains(CPSR::V)
            },
            0xC => {    // GT
                let cpsr = self.read_cpsr();
                !cpsr.contains(CPSR::Z) && (cpsr.contains(CPSR::N) == cpsr.contains(CPSR::V))
            },
            0xD => {    // LE
                let cpsr = self.read_cpsr();
                cpsr.contains(CPSR::Z) || (cpsr.contains(CPSR::N) != cpsr.contains(CPSR::V))
            },
            0xE => true,    // AL
            0xF => false,   // NE
            _ => unreachable!()
        }
    }

    // Decoding functions

    /// Decode a coprocessor or SWI instruction.
    /// i has the value cccc11...
    fn decode_coproc(&mut self, i: u32) -> usize {
        if test_bit(i, 25) {
            if test_bit(i, 24) {
                self.swi();
                0
            } else {
                self.decode_coproc_op(i)
            }
        } else {
            self.decode_coproc_transfer(i)
        }
    }

    /// Decode a coprocessor register transfer or data op.
    fn decode_coproc_op(&mut self, i: u32) -> usize {
        let reg_n = ((i >> 16) & 0xF) as usize;
        let reg_d = ((i >> 12) & 0xF) as usize;
        let coproc = ((i >> 8) & 0xF) as usize;
        let info = (i >> 5) & 0x7;
        let reg_m = (i & 0xF) as usize;
        if test_bit(i, 4) {
            let op = (i >> 21) & 0x7;
            if test_bit(i, 20) {
                self.mrc(coproc, reg_n, reg_d, reg_m, op, info)
            } else {
                self.mcr(coproc, reg_n, reg_d, reg_m, op, info)
            }
        } else {
            let op = (i >> 20) & 0xF;
            self.cdp(op, reg_n, reg_d, info, reg_m, coproc)
        }
    }

    /// Decode a coprocessor data transfer with memory.
    fn decode_coproc_transfer(&mut self, i: u32) -> usize {
        let base_reg = ((i >> 16) & 0xF) as usize;
        let data_reg = ((i >> 12) & 0xF) as usize;
        let coproc = ((i >> 8) & 0xF) as usize;
        let offset = (i & 0xFF) << 2;

        let base_addr = self.read_reg(base_reg);
        let offset_addr = if test_bit(i, 23) {
            base_addr.wrapping_add(offset)  // Inc
        } else {
            base_addr.wrapping_sub(offset)  // Dec
        };
        let pre_index = test_bit(i, 24);
        let transfer_addr = if pre_index {
            offset_addr // Pre
        } else {
            base_addr   // Post
        };

        let cycles = if test_bit(i, 20) {
            self.ldc(test_bit(i, 22), transfer_addr, coproc, data_reg)
        } else {
            self.stc(test_bit(i, 22), transfer_addr, coproc, data_reg)
        };

        if !pre_index || test_bit(i, 21) {
            // Write offset address back into base register.
            self.write_reg(base_reg, offset_addr);
        }

        cycles
    }

    /// Decode a branch or block transfer instruction.
    /// i has the value cccc10...
    fn decode_branch(&mut self, i: u32) -> usize {
        if test_bit(i, 25) {
            if test_bit(i, 24) {
                self.b(i);
            } else {
                self.bl(i);
            }
            // Branches themselves take 0 additional cycles.
            // The pipeline however is flushed which will lead to extra cycles.
            // Therefore it will take a few additional steps to start executing again.
            0
        } else {
            self.transfer_multiple(i)
        }
    }

    /// Decode a single transfer instruction (load or store).
    /// i has the value cccc01...
    fn decode_transfer(&mut self, i: u32) -> usize {
        let base_reg = ((i >> 16) & 0xF) as usize;
        let data_reg = ((i >> 12) & 0xF) as usize;
        let offset = self.offset(i);

        let base_addr = self.read_reg(base_reg);
        let offset_addr = if test_bit(i, 23) {
            base_addr.wrapping_add(offset)  // Inc
        } else {
            base_addr.wrapping_sub(offset)  // Dec
        };
        let pre_index = test_bit(i, 24);
        let transfer_addr = if pre_index {
            offset_addr // Pre
        } else {
            base_addr   // Post
        };

        let cycles = if test_bit(i, 20) {
            self.ldr(test_bit(i, 22), transfer_addr, data_reg)
        } else {
            self.str(test_bit(i, 22), transfer_addr, data_reg)
        };

        if !pre_index || test_bit(i, 21) {
            // Write offset address back into base register.
            self.write_reg(base_reg, offset_addr);
        }

        cycles
    }

    /// Decode an ALU instruction.
    /// i has the value cccc00...
    fn decode_alu(&mut self, i: u32) -> usize {
        // If bit 25 is 0, then bits 7 and 4 can determine if it's an extension instruction.
        if !test_bit(i, 25) && test_bit(i, 7) && test_bit(i, 4) {   // TODO: optimise this check
            self.decode_other(i)
        } else {
            self.decode_data_proc(i)
        }
    }

    /// Decode extended transfers, SWP and multiplication.
    /// All of these instructions have bit25 = 0 and bits7 and 4 = 1.
    /// The exact instruction depends on the value of bits 6 and 5.
    fn decode_other(&mut self, i: u32) -> usize {
        const HALFWORD: u32 = 0b01 << 5;
        const OTHER: u32 = 0b00 << 5;
        match i & bits(5, 6) {
            HALFWORD => self.decode_transfer_halfword(i),
            OTHER if test_bit(i, 24) => if !test_bit(i, 20) {
                self.swp(i)
            } else {
                self.undefined()
            },
            OTHER => self.decode_multiply(i),
            _ => self.undefined(),
        }
    }

    /// Decode a multiply instruction.
    fn decode_multiply(&mut self, i: u32) -> usize {
        let set_flags = test_bit(i, 20);
        let rd = ((i >> 16) & 0xF) as usize;
        let rn = ((i >> 12) & 0xF) as usize;
        let rs = ((i >> 8) & 0xF) as usize;
        let rm = (i & 0xF) as usize;
        match (i >> 21) & 0xF {
            0x0 => self.mul(set_flags, rd, rs, rm),
            0x1 => self.mla(set_flags, rd, rn, rs, rm),
            0x4 => self.umull(set_flags, rn, rd, rs, rm),
            0x5 => self.umlal(set_flags, rn, rd, rs, rm),
            0x6 => self.smull(set_flags, rn, rd, rs, rm),
            0x7 => self.smlal(set_flags, rn, rd, rs, rm),
            _ => unreachable!("unknown MUL instruction"),
        }
    }

    /// Decode a data processing instruction.
    fn decode_data_proc(&mut self, i: u32) -> usize {
        let set_flags = test_bit(i, 20);
        let rn = ((i >> 16) & 0xF) as usize;
        let rd = ((i >> 12) & 0xF) as usize;

        let compare = || {
            set_flags && (rd == 0)
        };

        match (i >> 21) & 0xF {
            0x0 => {
                let op2 = self.op2(i, true);
                self.and(set_flags, rd, self.read_reg(rn), op2);    // AND
            },
            0x1 => {
                let op2 = self.op2(i, true);
                self.eor(set_flags, rd, self.read_reg(rn), op2);    // EOR
            },
            0x2 => {
                let op2 = self.op2(i, false);
                self.sub(set_flags, rd, self.read_reg(rn), op2);    // SUB
            },
            0x3 => {
                let op2 = self.op2(i, false);
                self.sub(set_flags, rd, op2, self.read_reg(rn));    // RSB
            },
            0x4 => {
                let op2 = self.op2(i, false);
                self.add(set_flags, rd, self.read_reg(rn), op2);    // ADD
            },
            0x5 => {
                let op2 = self.op2(i, false);
                self.adc(set_flags, rd, self.read_reg(rn), op2);    // ADC
            },
            0x6 => {
                let op2 = self.op2(i, false);
                self.adc(set_flags, rd, self.read_reg(rn), !op2);   // SBC
            },
            0x7 => {
                let op2 = self.op2(i, false);
                self.adc(set_flags, rd, op2, !self.read_reg(rn));   // RSC
            },
            0xC => {
                let op2 = self.op2(i, true);
                self.orr(set_flags, rd, self.read_reg(rn), op2);    // ORR
            },
            0xE => {
                let op2 = self.op2(i, true);
                self.bic(set_flags, rd, self.read_reg(rn), op2);    // BIC
            },
            0x8 => if compare() {
                let op2 = self.op2(i, true);
                self.tst(self.read_reg(rn), op2)                    // TST
            } else {
                return self.decode_ext_mode(i, rn, rd, false, false);
            },
            0x9 => if compare() {
                let op2 = self.op2(i, true);
                self.teq(self.read_reg(rn), op2)                    // TEQ
            } else {
                return self.decode_ext_mode(i, rn, rd, true, false);
            },
            0xA => if compare() {
                let op2 = self.op2(i, false);
                self.cmp(self.read_reg(rn), op2)                    // CMP
            } else {
                return self.decode_ext_mode(i, rn, rd, false, true);
            },
            0xB => if compare() {
                let op2 = self.op2(i, false);
                self.cmn(self.read_reg(rn), op2)                    // CMN
            } else {
                return self.decode_ext_mode(i, rn, rd, true, true);
            },
            0xD => {
                let op2 = self.op2(i, true);
                self.mov(set_flags, rd, op2);                       // MOV
            },
            0xF => {
                let op2 = self.op2(i, true);
                self.mov(set_flags, rd, !op2);                      // MVN
            },
            _ => return self.undefined(),
        }

        // TODO: optimise this check
        if !test_bit(i, 25) && test_bit(i, 4) {
            self.write_reg(PC_REG, self.read_reg(PC_REG).wrapping_sub(4));
            1   // Register shift takes an extra internal cycle
        } else {
            0
        }
    }

    /// Decodes extension mode instructions.
    /// This includes MRS, MSR and BX
    /// This has multiple entry points so some data has already been decoded.
    fn decode_ext_mode(&mut self, i: u32, rn: usize, rd: usize, msr: bool, spsr: bool) -> usize {
        if msr && (rd == 0xF) {
            if !test_bit(i, 25) {
                match i & 0xFF0 {
                    0xF10 if rn == 0xF => self.bx(self.read_reg((i & 0xF) as usize)),
                    0x000 => self.msr(spsr, i),
                    _ => {self.undefined();},
                }
            } else {
                self.msr(spsr, i)
            }
        } else if !msr && (rn == 0xF) && (i & 0xFFF) == 0 {
            self.mrs(spsr, rd);
        } else {
            self.undefined();
        }
        0
    }

    /// Calculate the second operand of an arithmetic / logic instruction.
    /// Can involve a shift.
    /// This instruction _may_ set the carry flag for certain instructions.
    /// It _will_ increment the program counter if a shift-by-register is involved.
    /// 
    /// Extracts a value from the lower 12 bits based on the 25th bit.
    fn op2(&mut self, i: u32, set_carry: bool) -> u32 {
        // Immediate value with rotate:
        if test_bit(i, 25) {
            let shift = (i >> 7) & 0x1E;
            let n = i & 0xFF;
            return n.rotate_right(shift);
        }
        // Register value with optional shift:
        let r_val = self.read_reg((i & 0xF) as usize);
        let shift_type = (i >> 5) & 3;
            // Shift by register:
        let shift = if test_bit(i, 4) {
            self.write_reg(PC_REG, self.read_reg(PC_REG).wrapping_add(4));
            let shift_reg = (i >> 8) & 0xF;
            self.read_reg(shift_reg as usize) & 0x1F

            // Shift by immediate:
        } else {
            let shift_amount = (i >> 7) & 0x1F;
            // Special shift cases when the immediate is 0:
            if shift_amount == 0 {
                let mut cpsr = self.read_cpsr();
                let ret = match shift_type {
                    0 => r_val,
                    1 => {
                        cpsr.set(CPSR::C, test_bit(r_val, 31));
                        0
                    },
                    2 => {
                        cpsr.set(CPSR::C, test_bit(r_val, 31));
                        (r_val as i32).wrapping_shr(31) as u32
                    },
                    3 => {  // RRX #1
                        let carry = if cpsr.contains(CPSR::C) {bit(31)} else {0};
                        cpsr.set(CPSR::C, test_bit(r_val, 0));
                        (r_val >> 1) | carry
                    },
                    _ => unreachable!()
                };
                if set_carry {
                    self.write_cpsr(cpsr);
                }
                return ret;
            }
            shift_amount
        };

        match shift_type {
            0 => self.lsl(set_carry, r_val, shift),
            1 => self.lsr(set_carry, r_val, shift),
            2 => self.asr(set_carry, r_val, shift),
            3 => self.ror(r_val, shift),
            _ => unreachable!()
        }
    }

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
                self.write_cpsr(cpsr);
            }
        }
    }

    /// Calculate the offset of a load/store instruction.
    fn offset(&self, i: u32) -> u32 {
        // Immediate offset:
        if !test_bit(i, 25) {
            return i & 0xFFF;
        }
        // Register shifted by immediate:
        let r_val = self.read_reg((i & 0xF) as usize);
        let shift_amount = (i >> 7) & 0x1F;
        let shift_type = (i >> 5) & 3;
        if shift_amount == 0 {
            return match shift_type {
                0 => r_val,
                1 => 0,
                2 => (r_val as i32).wrapping_shr(31) as u32,
                3 => {  // RRX #1
                    let carry = if self.read_cpsr().contains(CPSR::C) {bit(31)} else {0};
                    (r_val >> 1) | carry
                },
                _ => unreachable!()
            };
        }
        match shift_type {
            0 => r_val.wrapping_shl(shift_amount),
            1 => r_val.wrapping_shr(shift_amount),
            2 => (r_val as i32).wrapping_shr(shift_amount) as u32,
            3 => r_val.rotate_right(shift_amount),
            _ => unreachable!()
        }
    }

    /// Decode a load or store halfword instruction.
    fn decode_transfer_halfword(&mut self, i: u32) -> usize {
        let base_reg = ((i >> 16) & 0xF) as usize;
        let data_reg = ((i >> 12) & 0xF) as usize;
        let offset = self.halfword_offset(i);

        let base_addr = self.read_reg(base_reg);
        let offset_addr = if test_bit(i, 23) {
            base_addr.wrapping_add(offset)  // Inc
        } else {
            base_addr.wrapping_sub(offset)  // Dec
        };
        let pre_index = test_bit(i, 24);
        let transfer_addr = if pre_index {
            offset_addr // Pre
        } else {
            base_addr   // Post
        };

        let cycles = if test_bit(i, 20) {
            self.ldrh(transfer_addr, data_reg)
        } else {
            self.strh(transfer_addr, data_reg)
        };

        if !pre_index || test_bit(i, 21) {
            // Write offset address back into base register.
            self.write_reg(base_reg, offset_addr);
        }

        cycles
    }

    /// Calculate the offset of a halfword load/store instruction.
    fn halfword_offset(&self, i: u32) -> u32 {
        if test_bit(i, 22) {
            // Immediate offset:
            ((i >> 4) & 0xF0) | (i & 0xF)
        } else {
            // Register offset:
            self.read_reg((i & 0xF) as usize)
        }
    }

    /// Decode a transfer multiple instruction.
    /// Writes registers low-high into low-high addresses.
    fn transfer_multiple(&mut self, i: u32) -> usize {
        let base_reg = ((i >> 16) & 0xF) as usize;
        if base_reg == PC_REG {
            self.undefined();
            return 0;
        }
        let base_addr = self.read_reg(base_reg);

        let data_regs = i & 0xFFFF;
        let offset = data_regs.count_ones() * 4;
        let increment = test_bit(i, 23);
        // Address to start transferring, and address to write back into register.
        // The lowest address is always the start address.
        let (transfer_addr, writeback_addr) = if !increment {
            let low_addr = base_addr.wrapping_sub(offset);
            (low_addr, low_addr)
        } else {
            (base_addr, base_addr.wrapping_add(offset))
        };

        // Add before load/store.
        let pre_index = test_bit(i, 24) == increment;

        let cycles = if test_bit(i, 20) {
            self.ldm(transfer_addr, data_regs, pre_index, test_bit(i, 22))
        } else {
            self.stm(transfer_addr, data_regs, pre_index, test_bit(i, 22))
        };

        if test_bit(i, 21) {
            // Write offset address back into base register.
            self.write_reg(base_reg, writeback_addr);
        }

        cycles
    }

    /// Called when an undefined instruction is encountered.
    /// Returns the amount of additional cycles taken.
    fn undefined(&mut self) -> usize {
        self.trigger_exception(crate::Exception::UndefinedInstruction);
        0
    }

    // Shifts

    /// LSL
    /// Logical shift left (fill with zeroes)
    fn lsl(&mut self, set_carry: bool, val: u32, shift_amount: u32) -> u32 {
        if set_carry {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::C, test_bit(val, (32 - shift_amount) as usize));
            self.write_cpsr(cpsr);
        }
        val.wrapping_shl(shift_amount)
    }

    /// LSR
    /// Logical shift right (fill with zeroes)
    fn lsr(&mut self, set_carry: bool, val: u32, shift_amount: u32) -> u32 {
        if set_carry {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::C, test_bit(val, (shift_amount - 1) as usize));
            self.write_cpsr(cpsr);
        }
        val.wrapping_shr(shift_amount)
    }

    /// ASR
    /// Arithmetic shift right (sign-extend)
    fn asr(&mut self, set_carry: bool, val: u32, shift_amount: u32) -> u32 {
        if set_carry {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::C, test_bit(val, (shift_amount - 1) as usize));
            self.write_cpsr(cpsr);
        }
        (val as i32).wrapping_shr(shift_amount) as u32
    }

    /// ROR
    /// Rotate right
    fn ror(&mut self, val: u32, shift_amount: u32) -> u32 {
        val.rotate_right(shift_amount)
    }

    // Logic

    /// AND
    /// Bitwise AND
    fn and(&mut self, s: bool, rd: usize, op1: u32, op2: u32) {
        let result = op1 & op2;
        self.writeback(s, rd, result);
    }

    /// EOR
    /// Bitwise exclusive OR (xor)
    fn eor(&mut self, s: bool, rd: usize, op1: u32, op2: u32) {
        let result = op1 ^ op2;
        self.writeback(s, rd, result);
    }

    /// ORR
    /// Bitwise inclusive OR
    fn orr(&mut self, s: bool, rd: usize, op1: u32, op2: u32) {
        let result = op1 | op2;
        self.writeback(s, rd, result);
    }

    /// BIC
    /// NOT op2 with AND
    fn bic(&mut self, s: bool, rd: usize, op1: u32, op2: u32) {
        let result = op1 & !op2;
        self.writeback(s, rd, result);
    }

    /// MOV
    /// Perform bitwise NOT on op2 to get MVN.
    fn mov(&mut self, s: bool, rd: usize, op2: u32) {
        self.writeback(s, rd, op2);
    }

    // Comparisons

    /// TST
    /// Bitwise AND and set flags.
    fn tst(&mut self, op1: u32, op2: u32) {
        let result = op1 & op2;
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::N, test_bit(result, 31));
        cpsr.set(CPSR::Z, result == 0);
        self.write_cpsr(cpsr);
    }

    /// TEQ
    /// Bitwise OR and set flags.
    fn teq(&mut self, op1: u32, op2: u32) {
        let result = op1 | op2;
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::N, test_bit(result, 31));
        cpsr.set(CPSR::Z, result == 0);
        self.write_cpsr(cpsr);
    }

    /// CMP
    /// Arithmetic sub and set flags.
    fn cmp(&mut self, op1: u32, op2: u32) {
        let (result, overflow) = op1.overflowing_sub(op2);
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::N, test_bit(result, 31));
        cpsr.set(CPSR::Z, result == 0);
        cpsr.set(CPSR::C, !overflow);
        cpsr.set(CPSR::V, test_bit((op1 ^ op2) & (op1 ^ result), 31));
        self.write_cpsr(cpsr);
    }

    /// CMP
    /// Arithmetic add and set flags.
    fn cmn(&mut self, op1: u32, op2: u32) {
        let (result, overflow) = op1.overflowing_add(op2);
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::N, test_bit(result, 31));
        cpsr.set(CPSR::Z, result == 0);
        cpsr.set(CPSR::C, overflow);
        cpsr.set(CPSR::V, test_bit(!(op1 ^ op2) & (op1 ^ result), 31));
        self.write_cpsr(cpsr);
    }

    // Arithmetic

    /// ADD
    /// Arithmetic add without carry.
    fn add(&mut self, s: bool, rd: usize, op1: u32, op2: u32) {
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
                self.write_cpsr(cpsr);
            }
        }
    }

    /// SUB / RSB
    /// Arithmetic subtract without carry.
    /// Swap op1 and op2 to get RSB.
    /// TODO: Check if we can use add with negated op2.
    fn sub(&mut self, s: bool, rd: usize, op1: u32, op2: u32) {
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
                self.write_cpsr(cpsr);
            }
        }
    }

    /// ADC / SBC / RSC
    /// Arithmetic add or subtract with carry / borrow.
    /// Perform bitwise NOT on op2 to get SBC/RSC.
    /// Swap op1 and op2 to get RSC.
    fn adc(&mut self, s: bool, rd: usize, op1: u32, op2: u32) {
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
                self.write_cpsr(cpsr);
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
            self.write_cpsr(cpsr);
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
            self.write_cpsr(cpsr);
        }
        mul_cycles(op2) + 1
    }

    /// UMULL
    /// Unsigned long multiply
    fn umull(&mut self, s: bool, rd_lo: usize, rd_hi: usize, rn: usize, rm: usize) -> usize {
        let op1 = self.read_reg(rm) as u64;
        let op2 = self.read_reg(rn);
        let op2_64 = op2 as u64;
        let result = op1.wrapping_mul(op2_64);
        self.write_reg(rd_lo, lo_64(result));
        self.write_reg(rd_hi, hi_64(result));
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, u64::test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.remove(CPSR::C);
            self.write_cpsr(cpsr);
        }
        mul_cycles(op2) + 1
    }

    /// UMLAL
    /// Unsigned long multiply and accumulate
    fn umlal(&mut self, s: bool, rd_lo: usize, rd_hi: usize, rn: usize, rm: usize) -> usize {
        let op1 = self.read_reg(rm) as u64;
        let op2 = self.read_reg(rn);
        let op2_64 = op2 as u64;
        let mul_result = op1.wrapping_mul(op2_64);
        let acc_op = make_64(self.read_reg(rd_hi), self.read_reg(rd_lo));
        let result = mul_result.wrapping_add(acc_op);
        self.write_reg(rd_lo, lo_64(result));
        self.write_reg(rd_hi, hi_64(result));
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, u64::test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.remove(CPSR::C);
            self.write_cpsr(cpsr);
        }
        mul_cycles(op2) + 2
    }

    /// SMULL
    /// Signed long multiply
    fn smull(&mut self, s: bool, rd_lo: usize, rd_hi: usize, rn: usize, rm: usize) -> usize {
        let op1 = (self.read_reg(rm) as i32) as i64;
        let op2 = self.read_reg(rn);
        let op2_64 = (op2 as i32) as i64;
        let result = op1.wrapping_mul(op2_64) as u64;
        self.write_reg(rd_lo, lo_64(result));
        self.write_reg(rd_hi, hi_64(result));
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, u64::test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.remove(CPSR::C);
            self.write_cpsr(cpsr);
        }
        mul_cycles(op2) + 1
    }

    /// SMLAL
    /// Signed long multiply and accumulate
    fn smlal(&mut self, s: bool, rd_lo: usize, rd_hi: usize, rn: usize, rm: usize) -> usize {
        let op1 = (self.read_reg(rm) as i32) as i64;
        let op2 = self.read_reg(rn);
        let op2_64 = (op2 as i32) as i64;
        let mul_result = op1.wrapping_mul(op2_64) as u64;
        let acc_op = make_64(self.read_reg(rd_hi), self.read_reg(rd_lo));
        let result = mul_result.wrapping_add(acc_op);
        self.write_reg(rd_lo, lo_64(result));
        self.write_reg(rd_hi, hi_64(result));
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, u64::test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.remove(CPSR::C);
            self.write_cpsr(cpsr);
        }
        mul_cycles(op2) + 2
    }

    // Branch

    /// B
    /// Branch
    fn b(&mut self, i: u32) {
        let raw_offset = i & 0xFFFFFF;
        let offset = if test_bit(raw_offset, 23) {
            (raw_offset | 0xFF000000) << 2
        } else {
            raw_offset << 2
        };

        let current_pc = self.read_reg(PC_REG);
        self.write_reg(PC_REG, current_pc.wrapping_add(offset));
    }

    /// BL
    /// Branch and link (using r14)
    fn bl(&mut self, i: u32) {
        let raw_offset = i & 0xFFFFFF;
        let offset = if test_bit(raw_offset, 23) {
            (raw_offset | 0xFF000000) << 2
        } else {
            raw_offset << 2
        };

        let current_pc = self.read_reg(PC_REG);
        self.write_reg(LINK_REG, current_pc.wrapping_sub(4));
        self.write_reg(PC_REG, current_pc.wrapping_add(offset));
    }

    /// BX
    /// Branch and exchange - switch to Thumb ISA
    fn bx(&mut self, dest: u32) {
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::T, (dest & 1) != 0);
        self.write_cpsr(cpsr);
        self.write_reg(PC_REG, dest & 0xFFFFFFFE);
    }

    /// SWI
    /// Software interrupt
    fn swi(&mut self) {
        self.trigger_exception(crate::Exception::SoftwareInterrupt);
    }

    // Data transfer

    /// MRS
    /// Move program status register into general purpose register
    fn mrs(&mut self, spsr: bool, rd: usize) {
        if spsr {
            self.write_reg(rd, self.read_spsr().bits());
        } else {
            self.write_reg(rd, self.read_cpsr().bits());
        }
    }

    /// MSR
    /// Move general purpose register into program status register
    fn msr(&mut self, spsr: bool, i: u32) {
        let mask = utils::fsxc_mask(i);
        let data = if test_bit(i, 25) { // Immediate
            let shift = (i >> 7) & 0x1E;
            let imm = i & 0xFF;
            imm.rotate_right(shift)
        } else {
            self.read_reg((i & 0xF) as usize)
        };
        if spsr {
            let old_spsr = self.read_spsr().bits() & !mask;
            let new_spsr = CPSR::from_bits_truncate((data & mask) | old_spsr);
            self.write_spsr(new_spsr);
        } else {
            let old_cpsr = self.read_cpsr().bits() & !mask;
            let new_cpsr = CPSR::from_bits_truncate((data & mask) | old_cpsr);
            self.write_cpsr(new_cpsr);
        }
    }

    /// SWP
    /// Single data swap
    fn swp(&mut self, i: u32) -> usize {
        let rn = ((i >> 16) & 0xF) as usize;
        let rd = ((i >> 12) & 0xF) as usize;
        let rm = (i & 0xF) as usize;

        let addr = self.read_reg(rn);
        if test_bit(i, 22) {
            let (data, load_cycles) = self.load_byte(addr);
            self.write_reg(rd, data as u32);
            let store_cycles = self.store_byte(addr, self.read_reg(rm) as u8);
            load_cycles + store_cycles + 1
        } else {
            let (data, load_cycles) = self.load_word(addr);
            self.write_reg(rd, data);
            let store_cycles = self.store_word(addr, self.read_reg(rm));
            load_cycles + store_cycles + 1
        }
    }

    /// LDR
    /// Load a single word or byte from memory and store it in a register.
    fn ldr(&mut self, byte: bool, addr: u32, dest_reg: usize) -> usize {
        let (data, cycles) = if byte {
            let (data_byte, cycles) = self.load_byte(addr);
            (data_byte as u32, cycles)
        } else {
            self.load_word(addr)
        };
        self.write_reg(dest_reg, data);
        // Loads take one extra internal cycle.
        cycles + 1
    }

    /// STR
    /// Store a single word or byte into memory.
    fn str(&mut self, byte: bool, addr: u32, src_reg: usize) -> usize {
        let data = if src_reg == PC_REG {
            self.read_reg(src_reg).wrapping_add(4)
        } else {
            self.read_reg(src_reg)
        };
        if byte {
            self.store_byte(addr, data as u8)
        } else {
            self.store_word(addr, data)
        }
    }

    /// LDRH
    /// Load 2 bytes from memory.
    fn ldrh(&mut self, addr: u32, dest_reg: usize) -> usize {
        let (data, cycles) = self.load_halfword(addr);
        self.write_reg(dest_reg, data as u32);
        cycles + 1
    }

    /// STRH
    /// Store 2 bytes into memory.
    fn strh(&mut self, addr: u32, src_reg: usize) -> usize {
        let data = if src_reg == PC_REG {
            self.read_reg(src_reg).wrapping_add(4)
        } else {
            self.read_reg(src_reg)
        };
        self.store_halfword(addr, data as u16)
    }

    /// LDM
    /// Block load from memory into registers.
    /// Start from the base address (addr). Load each register in low-high.
    /// Adds 4 to the address before loading if pre == true.
    /// 
    /// If s == true, then the processor will return from exception if PC is loaded,
    /// or it will transfer into user registers.
    fn ldm(&mut self, mut addr: u32, regs: u32, pre: bool, s: bool) -> usize {
        let load_from_user = s && !test_bit(regs, PC_REG);
        let cycles = if pre {
            (0..16).filter(|reg| test_bit(regs, *reg)).fold(0, |acc, reg| {
                addr += 4;
                let (data, cycles) = self.load_word(addr);
                if load_from_user {
                    self.write_usr_reg(reg, data);
                } else {
                    self.write_reg(reg, data);
                }
                acc + cycles
            }) + 1
        } else {
            (0..16).filter(|reg| test_bit(regs, *reg)).fold(0, |acc, reg| {
                let (data, cycles) = self.load_word(addr);
                if load_from_user {
                    self.write_usr_reg(reg, data);
                } else {
                    self.write_reg(reg, data);
                }
                addr += 4;
                acc + cycles
            }) + 1
        };
        if s && test_bit(regs, PC_REG) {
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
    fn stm(&mut self, mut addr: u32, regs: u32, pre: bool, s: bool) -> usize {
        if pre {
            (0..16).filter(|reg| test_bit(regs, *reg)).fold(0, |acc, reg| {
                addr += 4;
                let data = if s {
                    self.read_usr_reg(reg)
                } else {
                    self.read_reg(reg)
                };
                let cycles = self.store_word(addr, data);
                acc + cycles
            }) + 1
        } else {
            (0..16).filter(|reg| test_bit(regs, *reg)).fold(0, |acc, reg| {
                let data = if s {
                    self.read_usr_reg(reg)
                } else {
                    self.read_reg(reg)
                };
                let cycles = self.store_word(addr, data);
                addr += 4;
                acc + cycles
            }) + 1
        }
    }

    // Coprocessor

    /// MRC
    /// Move from coprocessor to ARM
    fn mrc(&mut self, coproc: usize, coproc_reg: usize, arm_reg: usize, op_reg: usize, op: u32, info: u32) -> usize {
        if let Some(c) = self.ref_coproc(coproc) {
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
        if let Some(c) = self.ref_coproc(coproc) {
            c.mcr(coproc_reg, op_reg, data, op, info)
        } else {
            self.undefined()
        }
    }

    /// STC
    /// Store into memory from coprocessor
    /// TODO: transfer len of more than 1?
    fn stc(&mut self, transfer_len: bool, addr: u32, coproc: usize, coproc_reg: usize) -> usize {
        if let Some(c) = self.ref_coproc(coproc) {
            let (data, cop_cycles) = c.stc(transfer_len, coproc_reg);
            let mem_cycles = self.store_word(addr, data);
            cop_cycles + mem_cycles
        } else {
            self.undefined()
        }
    }

    /// LDC
    /// Load from memory to coprocessor
    /// TODO: transfer len of more than 1?
    fn ldc(&mut self, transfer_len: bool, addr: u32, coproc: usize, coproc_reg: usize) -> usize {
        let (data, mem_cycles) = self.load_word(addr);
        if let Some(c) = self.ref_coproc(coproc) {
            c.ldc(transfer_len, coproc_reg, data) + mem_cycles
        } else {
            self.undefined()
        }
    }

    /// CDP
    /// Coprocessor data operation
    fn cdp(&mut self, op: u32, reg_n: usize, reg_d: usize, info: u32, reg_m: usize, coproc: usize) -> usize {
        if let Some(c) = self.ref_coproc(coproc) {
            c.cdp(op, reg_n, reg_d, info, reg_m)
        } else {
            self.undefined()
        }
    }

}