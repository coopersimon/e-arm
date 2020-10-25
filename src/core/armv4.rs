/// ARMv4 Instruction Set

use super::*;
use crate::common::*;

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

pub trait ARMv4: ARMCore {
    /// Decode and execute the instruction.
    fn execute_instruction(&mut self, i: u32) {
        const DATA_PROC: u32 = bits(26, 27);


        if self.check_cond(i) {
            // Decode
            if (i & DATA_PROC) == 0 {
                self.data_proc(i);
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

    /// Decode a data processing instruction.
    fn data_proc(&mut self, i: u32) {
        const fn set_flags(i: u32) -> bool {
            test_bit(i, 20)
        }
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
            0x0 => self.and(set_flags(i), rd(i), self.read_reg(rn(i)), op2, shift_carry),   // AND
            0x1 => self.eor(set_flags(i), rd(i), self.read_reg(rn(i)), op2, shift_carry),   // EOR
            0x2 => self.sub(set_flags(i), rd(i), self.read_reg(rn(i)), op2),                // SUB
            0x3 => self.sub(set_flags(i), rd(i), op2, self.read_reg(rn(i))),                // RSB
            0x4 => self.add(set_flags(i), rd(i), self.read_reg(rn(i)), op2),                // ADD
            0x5 => self.adc(set_flags(i), rd(i), self.read_reg(rn(i)), op2),                // ADC
            0x6 => self.adc(set_flags(i), rd(i), self.read_reg(rn(i)), !op2),               // SBC
            0x7 => self.adc(set_flags(i), rd(i), op2, !self.read_reg(rn(i))),               // RSC
            0x8 => self.tst(self.read_reg(rn(i)), op2, shift_carry),                        // TST
            0x9 => self.teq(self.read_reg(rn(i)), op2, shift_carry),                        // TEQ
            0xA => self.cmp(self.read_reg(rn(i)), op2),                                     // CMP
            0xB => self.cmn(self.read_reg(rn(i)), op2),                                     // CMN
            0xC => self.orr(set_flags(i), rd(i), self.read_reg(rn(i)), op2, shift_carry),   // ORR
            0xD => self.mov(set_flags(i), rd(i), op2, shift_carry),                         // MOV
            0xE => self.bic(set_flags(i), rd(i), self.read_reg(rn(i)), op2, shift_carry),   // BIC
            0xF => self.mov(set_flags(i), rd(i), !op2, shift_carry),                        // MVN
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
            // Shift by register:
        let shift = if test_bit(i, 4) {
            let shift_reg = (i >> 8) & 0xF;
            self.read_reg(shift_reg as usize) & 0xFF

            // Shift by immediate:
        } else {
            let shift_amount = (i >> 7) & 0x1F;
            // Special shift cases when the immediate is 0:
            if shift_amount == 0 {
                return match (i >> 5) & 3 {
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
        match (i >> 5) & 3 {
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
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            if let Some(c) = set_c {
                cpsr.set(CPSR::C, c);
            }
            self.write_cpsr(cpsr);
        }
    }

    // Logic

    /// AND
    fn and(&mut self, s: bool, rd: usize, op1: u32, op2: u32, set_c: Option<bool>) {
        let result = op1 & op2;
        self.writeback(s, rd, result, set_c);
    }

    /// EOR
    fn eor(&mut self, s: bool, rd: usize, op1: u32, op2: u32, set_c: Option<bool>) {
        let result = op1 ^ op2;
        self.writeback(s, rd, result, set_c);
    }

    /// ORR
    fn orr(&mut self, s: bool, rd: usize, op1: u32, op2: u32, set_c: Option<bool>) {
        let result = op1 | op2;
        self.writeback(s, rd, result, set_c);
    }

    /// BIC
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
    fn cmp(&mut self, op1: u32, op2: u32) {
        let (result, overflow) = op1.overflowing_sub(op2);
        let mut cpsr = self.read_cpsr();
        cpsr.set(CPSR::N, test_bit(result, 31));
        cpsr.set(CPSR::Z, result == 0);
        cpsr.set(CPSR::C, overflow);
        cpsr.set(CPSR::V, test_bit((op1 ^ op2) & (op1 ^ result), 31));
        self.write_cpsr(cpsr);
    }

    /// CMP
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
    fn add(&mut self, s: bool, rd: usize, op1: u32, op2: u32) {
        let (result, overflow) = op1.overflowing_add(op2);
        self.write_reg(rd, result);
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.set(CPSR::C, overflow);
            cpsr.set(CPSR::V, test_bit(!(op1 ^ op2) & (op1 ^ result), 31));
            self.write_cpsr(cpsr);
        }
    }

    /// SUB / RSB
    /// TODO: Check if we can use add with negated op2.
    fn sub(&mut self, s: bool, rd: usize, op1: u32, op2: u32) {
        let (result, overflow) = op1.overflowing_sub(op2);
        self.write_reg(rd, result);
        if s {
            let mut cpsr = self.read_cpsr();
            cpsr.set(CPSR::N, test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.set(CPSR::C, overflow);
            cpsr.set(CPSR::V, test_bit((op1 ^ op2) & (op1 ^ result), 31));
            self.write_cpsr(cpsr);
        }
    }

    /// ADC / SBC / RSC
    /// Perform bitwise NOT on op2 to get SBC/RSC.
    fn adc(&mut self, s: bool, rd: usize, op1: u32, op2: u32) {
        let mut cpsr = self.read_cpsr();
        let (r1, o1) = op1.overflowing_add(op2);
        let (result, o2) = r1.overflowing_add(cpsr.carry());
        self.write_reg(rd, result);
        if s {
            cpsr.set(CPSR::N, test_bit(result, 31));
            cpsr.set(CPSR::Z, result == 0);
            cpsr.set(CPSR::C, o1 || o2);
            cpsr.set(CPSR::V, test_bit(!(op1 ^ op2) & (op1 ^ result), 31));
            self.write_cpsr(cpsr);
        }
    }
}