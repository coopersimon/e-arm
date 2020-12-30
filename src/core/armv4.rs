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
        const BRANCH: u32 = bits(24, 27);
        const MUL_HI: u32 = bits(25, 27);
        const MUL_LO: u32 = bits(4, 7);
        const DATA_PROC: u32 = bits(26, 27);

        if self.check_cond(i) {
            // Decode
            if (i & BRANCH) == (0b1111 << 24) {
                self.swi();
            } else if (i & BRANCH) == (0b1010 << 24) {
                self.b(i);
            } else if (i & BRANCH) == (0b1011 << 24) {
                self.bl(i);
            } else if (i & bits(8, 27)) == (0b0001_0010_1111_1111_1111_0001 << 4) {
                self.bx(i);
            } else if (i & MUL_HI) == 0 {
                if (i & MUL_LO) == (0b1001 << 4) {
                    self.exec_mul(i);
                } else {
                    self.halfword(i);
                }
            } else if (i & DATA_PROC) == 0 {
                self.data_proc(i);
            } else if (i & DATA_PROC) == (1 << 26) {
                self.load_store(i);
            } else {
                self.trigger_exception(Exception::UndefinedInstruction);
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

    /// Decode a multiply instruction.
    fn exec_mul(&mut self, i: u32) {
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
    fn data_proc(&mut self, i: u32) {
        let set_flags = test_bit(i, 20);
        const fn rn(i: u32) -> usize {
            const SHIFT: usize = 16;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        const fn rd(i: u32) -> usize {
            const SHIFT: usize = 12;
            const MASK: u32 = 0xF;
            ((i >> SHIFT) & MASK) as usize
        };
        let (op2, shift_carry) = self.op2(i);

        const SHIFT: usize = 21;
        const MASK: u32 = 0xF;
        match (i >> SHIFT) & MASK {
            0x0 => self.and(set_flags, rd(i), self.read_reg(rn(i)), op2, shift_carry),  // AND
            0x1 => self.eor(set_flags, rd(i), self.read_reg(rn(i)), op2, shift_carry),  // EOR
            0x2 => self.sub(set_flags, rd(i), self.read_reg(rn(i)), op2),               // SUB
            0x3 => self.sub(set_flags, rd(i), op2, self.read_reg(rn(i))),               // RSB
            0x4 => self.add(set_flags, rd(i), self.read_reg(rn(i)), op2),               // ADD
            0x5 => self.adc(set_flags, rd(i), self.read_reg(rn(i)), op2),               // ADC
            0x6 => self.adc(set_flags, rd(i), self.read_reg(rn(i)), !op2),              // SBC
            0x7 => self.adc(set_flags, rd(i), op2, !self.read_reg(rn(i))),              // RSC
            0x8 => if set_flags {
                self.tst(self.read_reg(rn(i)), op2, shift_carry)                        // TST
            } else {
                self.mrs(false, rd(i))
            },
            0x9 => if set_flags {
                self.teq(self.read_reg(rn(i)), op2, shift_carry)                        // TEQ
            } else {
                self.msr(false, i)
            },
            0xA => if set_flags {
                self.cmp(self.read_reg(rn(i)), op2)                                     // CMP
            } else {
                self.mrs(true, rd(i))
            },
            0xB => if set_flags {
                self.cmn(self.read_reg(rn(i)), op2)                                     // CMN
            } else {
                self.msr(true, i)
            },
            0xC => self.orr(set_flags, rd(i), self.read_reg(rn(i)), op2, shift_carry),  // ORR
            0xD => self.mov(set_flags, rd(i), op2, shift_carry),                        // MOV
            0xE => self.bic(set_flags, rd(i), self.read_reg(rn(i)), op2, shift_carry),  // BIC
            0xF => self.mov(set_flags, rd(i), !op2, shift_carry),                       // MVN
            _ => unreachable!()
        }
    }

    /// Calculate the second operand of an arithmetic / logic instruction.
    /// Can involve a shift.
    /// If the carry flag should be modified for logic ops, it is returned via the second return value.
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

    /// Decode a load or store instruction.
    fn load_store(&mut self, i: u32) {
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
    fn halfword(&mut self, i: u32) {
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
}