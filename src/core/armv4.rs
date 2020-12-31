/// ARMv4 Instruction Set

use super::{
    constants::*,
    *
};
use crate::common::*;
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
    fn execute_instruction(&mut self, i: u32) {
        const COPROC: u32 = 0b11 << 26;
        const BRANCH: u32 = 0b10 << 26;
        const TRANSFER: u32 = 0b01 << 26;
        const ALU: u32 = 0b00 << 26;
        if self.check_cond(i) {
            match i & bits(26, 27) {
                COPROC      => self.decode_coproc(i),
                BRANCH      => self.decode_branch(i),
                TRANSFER    => self.decode_transfer(i),
                ALU         => self.decode_alu(i),
                _ => unreachable!(),
            }
        }
    }

    /// Check if the instruction should run based on the condition.
    fn check_cond(&self, i: u32) -> bool {
        const SHIFT: usize = 28;
        match i >> SHIFT {
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
    fn decode_coproc(&mut self, i: u32) {
        if ((i >> 24) & 0xF) == 0xF {
            self.swi();
        }
    }

    /// Decode a branch or block transfer instruction.
    /// i has the value cccc10...
    fn decode_branch(&mut self, i: u32) {
        if test_bit(i, 25) {
            if test_bit(i, 24) {
                self.b(i);
            } else {
                self.bl(i);
            }
        } else {
            self.transfer_multiple(i);
        }
    }

    /// Decode a single transfer instruction (load or store).
    /// i has the value cccc01...
    fn decode_transfer(&mut self, i: u32) {
        let base_reg = {
            const SHIFT: usize = 16;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        let data_reg = {
            const SHIFT: usize = 12;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        let offset = self.offset(i);

        let base_addr = if base_reg == PC_REG {
            self.read_reg(base_reg).wrapping_add(8)
        } else {
            self.read_reg(base_reg)
        };
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

        if test_bit(i, 20) {
            self.ldr(test_bit(i, 22), transfer_addr, data_reg);
        } else {
            self.str(test_bit(i, 22), transfer_addr, data_reg);
        }

        if !pre_index || test_bit(i, 21) {
            // Write offset address back into base register.
            self.write_reg(base_reg, offset_addr);
        }
    }

    /// Decode an ALU instruction.
    /// i has the value cccc00...
    fn decode_alu(&mut self, i: u32) {
        // If bit 25 is 0, then bits 7 and 4 can determine if it's an extension instruction.
        if !test_bit(i, 25) && test_bit(i, 7) && test_bit(i, 4) {   // TODO: optimise this check
            self.decode_other(i);
        } else {
            self.decode_data_proc(i);
        }
    }

    /// Decode extended transfers, SWP and multiplication.
    /// All of these instructions have bit25 = 0 and bits7 and 4 = 1.
    /// The exact instruction depends on the value of bits 6 and 5.
    fn decode_other(&mut self, i: u32) {
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
    fn decode_multiply(&mut self, i: u32) {
        let set_flags = test_bit(i, 20);
        let rd = {
            const SHIFT: usize = 16;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        let rn = {
            const SHIFT: usize = 12;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        let rs = {
            const SHIFT: usize = 8;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        let rm = {
            const MASK: u32 = 0xF;
            (i & MASK) as usize
        };
        const SHIFT: usize = 21;
        const MASK: u32 = 0xF;
        match (i >> SHIFT) & MASK {
            0x0 => self.mul(set_flags, rd, rs, rm),
            0x1 => self.mla(set_flags, rd, rn, rs, rm),
            _ => unreachable!("unknown MUL instruction"),
        }
    }

    /// Decode a data processing instruction.
    fn decode_data_proc(&mut self, i: u32) {
        let set_flags = test_bit(i, 20);
        let rn = {
            const SHIFT: usize = 16;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        let rd = {
            const SHIFT: usize = 12;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        let (op2, shift_carry) = self.op2(i);

        let compare = || {
            set_flags && (rd == 0)
        };

        const SHIFT: usize = 21;
        const MASK: u32 = 0xF;
        match (i >> SHIFT) & MASK {
            0x0 => self.and(set_flags, rd, self.read_reg(rn), op2, shift_carry),  // AND
            0x1 => self.eor(set_flags, rd, self.read_reg(rn), op2, shift_carry),  // EOR
            0x2 => self.sub(set_flags, rd, self.read_reg(rn), op2),               // SUB
            0x3 => self.sub(set_flags, rd, op2, self.read_reg(rn)),               // RSB
            0x4 => self.add(set_flags, rd, self.read_reg(rn), op2),               // ADD
            0x5 => self.adc(set_flags, rd, self.read_reg(rn), op2),               // ADC
            0x6 => self.adc(set_flags, rd, self.read_reg(rn), !op2),              // SBC
            0x7 => self.adc(set_flags, rd, op2, !self.read_reg(rn)),              // RSC
            0xC => self.orr(set_flags, rd, self.read_reg(rn), op2, shift_carry),  // ORR
            0xE => self.bic(set_flags, rd, self.read_reg(rn), op2, shift_carry),  // BIC
            0x8 => if compare() {
                self.tst(self.read_reg(rn), op2, shift_carry)                     // TST
            } else {
                self.decode_ext_mode(i, rn, rd, false, false)
            },
            0x9 => if compare() {
                self.teq(self.read_reg(rn), op2, shift_carry)                     // TEQ
            } else {
                self.decode_ext_mode(i, rn, rd, true, false)
            },
            0xA => if compare() {
                self.cmp(self.read_reg(rn), op2)                                  // CMP
            } else {
                self.decode_ext_mode(i, rn, rd, false, true)
            },
            0xB => if compare() {
                self.cmn(self.read_reg(rn), op2)                                  // CMN
            } else {
                self.decode_ext_mode(i, rn, rd, true, true)
            },
            0xD => self.mov(set_flags, rd, op2, shift_carry),                     // MOV
            0xF => self.mov(set_flags, rd, !op2, shift_carry),                    // MVN
            _ => self.undefined(),
        }
    }

    /// Decodes extension mode instructions.
    /// This includes MRS, MSR and BX
    /// This has multiple entry points so some data has already been decoded.
    fn decode_ext_mode(&mut self, i: u32, rn: usize, rd: usize, msr: bool, spsr: bool) {
        if msr && (rd == 0xF) {
            if !test_bit(i, 25) {
                match i & 0xFF0 {
                    0xF10 if rn == 0xF => self.bx(i),
                    0x000 => self.msr(spsr, i),
                    _ => self.undefined(),
                }
            } else {
                self.msr(spsr, i);
            }
        } else if !msr && (rn == 0xF) && (i & 0xFFF) == 0 {
            self.mrs(spsr, rd);
        } else {
            self.undefined();
        }
    }

    /// Calculate the second operand of an arithmetic / logic instruction.
    /// Can involve a shift.
    /// If the carry flag should be modified for logic ops, it is returned via the second return value.
    /// Extracts a value from the lower 12 bits based on the 25th bit.
    fn op2(&self, i: u32) -> (u32, Option<bool>) {
        // Immediate value with rotate:
        if test_bit(i, 25) {
            let shift = (i >> 7) & 0x1E;
            let n = i & 0xFF;
            return (n.rotate_right(shift), None);
        }
        // Register value with optional shift:
        let r_val = self.read_reg((i & 0xF) as usize);
        let shift_type = (i >> 5) & 3;
            // Shift by register:
        let shift = if test_bit(i, 4) {
            let shift_reg = (i >> 8) & 0xF;
            self.read_reg(shift_reg as usize) & 0xFF

            // Shift by immediate:
        } else {
            let shift_amount = (i >> 7) & 0x1F;
            // Special shift cases when the immediate is 0:
            if shift_amount == 0 {
                return match shift_type {
                    0 => (r_val, None),
                    1 => (0, Some(test_bit(r_val, 31))),
                    2 => ((r_val as i32).wrapping_shr(31) as u32, Some(test_bit(r_val, 31))),
                    3 => {  // RRX #1
                        let carry = if self.read_cpsr().contains(CPSR::C) {bit(31)} else {0};
                        ((r_val >> 1) | carry, Some(test_bit(r_val, 0)))
                    },
                    _ => unreachable!()
                };
            }
            shift_amount
        };
        // TODO: the following need to be accurate when reg shift > 31
        match shift_type {
            0 => (r_val.wrapping_shl(shift), Some(test_bit(r_val, (32 - shift) as usize))),
            1 => (r_val.wrapping_shr(shift), Some(test_bit(r_val, (shift - 1) as usize))),
            2 => ((r_val as i32).wrapping_shr(shift) as u32, Some(test_bit(r_val, (shift - 1) as usize))),
            3 => (r_val.rotate_right(shift), None),
            _ => unreachable!()
        }
    }

    /// Writeback a data processing instruction.
    /// Also sets the n and z flags optionally.
    fn writeback(&mut self, s: bool, rd: usize, result: u32, set_c: Option<bool>) {
        self.write_reg(rd, result);
        if s {
            if rd == PC_REG {
                self.return_from_exception();
            } else {
                let mut cpsr = self.read_cpsr();
                cpsr.set(CPSR::N, test_bit(result, 31));
                cpsr.set(CPSR::Z, result == 0);
                if let Some(c) = set_c {
                    cpsr.set(CPSR::C, c);
                }
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
    fn decode_transfer_halfword(&mut self, i: u32) {
        let base_reg = {
            const SHIFT: usize = 16;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        let data_reg = {
            const SHIFT: usize = 12;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        let offset = self.halfword_offset(i);

        let base_addr = if base_reg == PC_REG {
            self.read_reg(base_reg).wrapping_add(8)
        } else {
            self.read_reg(base_reg)
        };
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

        if test_bit(i, 20) {
            self.ldrh(transfer_addr, data_reg);
        } else {
            self.strh(transfer_addr, data_reg);
        }

        if !pre_index || test_bit(i, 21) {
            // Write offset address back into base register.
            self.write_reg(base_reg, offset_addr);
        }
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
    fn transfer_multiple(&mut self, i: u32) {
        let base_reg = {
            const SHIFT: usize = 16;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        if base_reg == PC_REG {
            self.trigger_exception(Exception::UndefinedInstruction);
            return;
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

        if test_bit(i, 20) {
            self.ldm(transfer_addr, data_regs, pre_index);
        } else {
            self.stm(transfer_addr, data_regs, pre_index);
        }

        if test_bit(i, 21) {
            // Write offset address back into base register.
            self.write_reg(base_reg, writeback_addr);
        }
    }

    /// Called when an undefined instruction is encountered.
    fn undefined(&mut self) {
        self.trigger_exception(Exception::UndefinedInstruction);
    }

    // Logic

    /// AND
    /// Bitwise AND
    fn and(&mut self, s: bool, rd: usize, op1: u32, op2: u32, set_c: Option<bool>) {
        let result = op1 & op2;
        self.writeback(s, rd, result, set_c);
    }

    /// EOR
    /// Bitwise exclusive OR (xor)
    fn eor(&mut self, s: bool, rd: usize, op1: u32, op2: u32, set_c: Option<bool>) {
        let result = op1 ^ op2;
        self.writeback(s, rd, result, set_c);
    }

    /// ORR
    /// Bitwise inclusive OR
    fn orr(&mut self, s: bool, rd: usize, op1: u32, op2: u32, set_c: Option<bool>) {
        let result = op1 | op2;
        self.writeback(s, rd, result, set_c);
    }

    /// BIC
    /// NOT op2 with AND
    fn bic(&mut self, s: bool, rd: usize, op1: u32, op2: u32, set_c: Option<bool>) {
        let result = op1 & !op2;
        self.writeback(s, rd, result, set_c);
    }

    /// MOV
    /// Perform bitwise NOT on op2 to get MVN.
    fn mov(&mut self, s: bool, rd: usize, op2: u32, set_c: Option<bool>) {
        self.writeback(s, rd, op2, set_c);
    }

    // Comparisons

    /// TST
    /// Bitwise AND and set flags.
    fn tst(&mut self, op1: u32, op2: u32, set_c: Option<bool>) {
        let result = op1 & op2;
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::N, test_bit(result, 31));
        cpsr.set(CPSR::Z, result == 0);
        if let Some(c) = set_c {
            cpsr.set(CPSR::C, c);
        }
        self.write_cpsr(cpsr);
    }

    /// TEQ
    /// Bitwise OR and set flags.
    fn teq(&mut self, op1: u32, op2: u32, set_c: Option<bool>) {
        let result = op1 | op2;
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::N, test_bit(result, 31));
        cpsr.set(CPSR::Z, result == 0);
        if let Some(c) = set_c {
            cpsr.set(CPSR::C, c);
        }
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

    /// MUL
    /// Multiply
    fn mul(&mut self, s: bool, rd: usize, rs: usize, rm: usize) {
        let result = self.read_reg(rs).wrapping_mul(self.read_reg(rm));
        self.write_reg(rd, result);
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.remove(CPSR::C);
            self.write_cpsr(cpsr);
        }
    }

    /// MLA
    /// Multiply and accumulate
    fn mla(&mut self, s: bool, rd: usize, rn: usize, rs: usize, rm: usize) {
        let mul_result = self.read_reg(rs).wrapping_mul(self.read_reg(rm));
        let result = mul_result.wrapping_add(self.read_reg(rn));
        self.write_reg(rd, result);
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.remove(CPSR::C);
            self.write_cpsr(cpsr);
        }
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
        self.write_reg(PC_REG, current_pc.wrapping_add(4).wrapping_add(offset));
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
        self.write_reg(LINK_REG, current_pc);
        self.write_reg(PC_REG, current_pc.wrapping_add(4).wrapping_add(offset));
    }

    /// BX
    fn bx(&mut self, i: u32) {
        let reg = self.read_reg((i & 0xF) as usize);
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::T, (reg & 1) != 0);
        self.write_cpsr(cpsr);
        self.write_reg(PC_REG, reg & 0xFFFFFFFE);
    }

    /// SWI
    /// Software interrupt
    fn swi(&mut self) {
        self.trigger_exception(Exception::SoftwareInterrupt);
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
    fn swp(&mut self, i: u32) {
        let rn = {
            const SHIFT: usize = 16;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        let rd = {
            const SHIFT: usize = 12;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        let rm = {
            const MASK: u32 = 0xF;
            (i & MASK) as usize
        };

        let addr = self.read_reg(rn);
        if test_bit(i, 22) {
            let data = self.load_byte(addr) as u32;
            self.write_reg(rd, data);
            self.store_byte(addr, self.read_reg(rm) as u8);
        } else {
            let data = self.load_word(addr);
            self.write_reg(rd, data);
            self.store_word(addr, self.read_reg(rm));
        }
    }

    /// LDR
    /// Load a single word or byte from memory and store it in a register.
    fn ldr(&mut self, byte: bool, addr: u32, dest_reg: usize) {
        let data = if byte {
            let data_byte = self.load_byte(addr);
            data_byte as u32
        } else {
            self.load_word(addr)
        };
        self.write_reg(dest_reg, data);
    }

    /// STR
    /// Store a single word or byte into memory.
    fn str(&mut self, byte: bool, addr: u32, src_reg: usize) {
        let data = if src_reg == PC_REG {
            self.read_reg(src_reg).wrapping_add(12)
        } else {
            self.read_reg(src_reg)
        };
        if byte {
            self.store_byte(addr, data as u8);
        } else {
            self.store_word(addr, data);
        }
    }

    /// LDRH
    /// Load 2 bytes from memory.
    fn ldrh(&mut self, addr: u32, dest_reg: usize) {
        let data = self.load_halfword(addr) as u32;
        self.write_reg(dest_reg, data);
    }

    /// STRH
    /// Store 2 bytes into memory.
    fn strh(&mut self, addr: u32, src_reg: usize) {
        let data = if src_reg == PC_REG {
            self.read_reg(src_reg).wrapping_add(12)
        } else {
            self.read_reg(src_reg)
        };
        self.store_halfword(addr, data as u16);
    }

    /// LDM
    /// Block load from memory into registers.
    /// Start from the base address (addr). Load each register in low-high.
    /// Add 4 to the address before loading if pre == true.
    fn ldm(&mut self, mut addr: u32, regs: u32, pre: bool) {
        if pre {
            for reg in 0..16 {
                if test_bit(regs, reg) {
                    addr += 4;
                    let data = self.load_word(addr);
                    self.write_reg(reg, data);
                }
            }
        } else {
            for reg in 0..16 {
                if test_bit(regs, reg) {
                    let data = self.load_word(addr);
                    self.write_reg(reg, data);
                    addr += 4;
                }
            }
        }
    }

    /// STM
    /// Block store from registers into memory.
    /// Start from the base address (addr). Store each register in low-high.
    /// Add 4 to the address before storing if pre == true.
    fn stm(&mut self, mut addr: u32, regs: u32, pre: bool) {
        if pre {
            for reg in 0..16 {
                if test_bit(regs, reg) {
                    addr += 4;
                    let data = self.read_reg(reg);
                    self.store_word(addr, data);
                }
            }
        } else {
            for reg in 0..16 {
                if test_bit(regs, reg) {
                    let data = self.read_reg(reg);
                    self.store_word(addr, data);
                    addr += 4;
                }
            }
        }
    }
}