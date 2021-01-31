/// ARMv4 Thumb Instruction Set

use super::{
    constants::*,
    armv4::ARMv4
};
use crate::common::u16::*;
use crate::memory::Mem32;

pub trait Thumbv4<M: Mem32<Addr = u32>>: ARMv4<M> {
    /// Decode and execute the instruction.
    /// 
    /// Returns the number of cycles needed to execute it.
    /// This includes any internal (I) cycles and non-seq loads and stores (N).
    /// It does _not_ include the initial fetch cycles (S) or any pipeline flush stall cycles.
    fn execute_thumb(&mut self, i: u16) -> usize {
        const ALU: u16          = 0b000 << 13;
        const ALU_IMM: u16      = 0b001 << 13;
        const OTHER_LO: u16     = 0b010 << 13;
        const TRANSFER: u16     = 0b011 << 13;
        const TRANSFER_EXT: u16 = 0b100 << 13;
        const STACK: u16        = 0b101 << 13;
        const OTHER_HI: u16     = 0b110 << 13;
        const BRANCH: u16       = 0b111 << 13;
        match i & bits(13, 15) {
            ALU =>          self.decode_thumb_alu(i),
            ALU_IMM =>      self.decode_thumb_alu_imm(i),
            OTHER_LO =>     self.decode_thumb_other_lo(i),
            TRANSFER =>     self.decode_thumb_transfer(i),
            TRANSFER_EXT => self.decode_thumb_transfer_ext(i),
            STACK =>        self.decode_thumb_stack(i),
            OTHER_HI =>     self.decode_thumb_other_hi(i),
            BRANCH =>       self.decode_thumb_branch(i),
            _ => unreachable!(),
        }
    }

    /// Decode shift & arithmetic
    fn decode_thumb_alu(&mut self, i: u16) -> usize {
        let rs = ((i >> 3) & 0x7) as usize;
        let rd = (i & 0x7) as usize;
        match (i & bits(11, 12)) >> 11 {
            0b00 => {
                let shift_amount = (i >> 6) & 0x1F;
                let val = self.lsl(shift_amount != 0, self.read_reg(rs), shift_amount as u32);
                self.write_reg(rd, val);
            },
            0b01 => {
                let shift_amount = (i >> 6) & 0x1F;
                let val = self.lsr(shift_amount != 0, self.read_reg(rs), shift_amount as u32);
                self.write_reg(rd, val);
            }
            0b10 => {
                let shift_amount = (i >> 6) & 0x1F;
                let val = self.asr(shift_amount != 0, self.read_reg(rs), shift_amount as u32);
                self.write_reg(rd, val);
            }
            0b11 => {
                let op2 = (i >> 6) & 0x7;
                match (i & bits(9, 10)) >> 9 {
                    0b00 => self.add(true, rd, self.read_reg(rs), self.read_reg(op2 as usize)),
                    0b01 => self.sub(true, rd, self.read_reg(rs), self.read_reg(op2 as usize)),
                    0b10 => self.add(true, rd, self.read_reg(rs), op2 as u32),
                    0b11 => self.sub(true, rd, self.read_reg(rs), op2 as u32),
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
        0
    }

    /// Decode move/compare/arith with 8-bit immediate
    fn decode_thumb_alu_imm(&mut self, i: u16) -> usize {
        let rd = ((i >> 8) & 0x7) as usize;
        let imm = (i & 0xFF) as u32;
        match (i & bits(11, 12)) >> 11 {
            0b00 => self.mov(true, rd, imm),
            0b01 => self.cmp(self.read_reg(rd), imm),
            0b10 => self.add(true, rd, self.read_reg(rd), imm),
            0b11 => self.sub(true, rd, self.read_reg(rd), imm),
            _ => unreachable!()
        }
        0
    }

    /// Decode other ALU operations, branch exchange, and register offset memory ops
    fn decode_thumb_other_lo(&mut self, i: u16) -> usize {
        if test_bit(i, 12) {
            self.decode_thumb_transfer_reg(i)
        } else if test_bit(i, 11) {
            // Load PC-relative
            let rd = ((i >> 8) & 0x7) as usize;
            let offset = ((i & 0xFF) << 2) as u32;
            let addr = self.read_reg(PC_REG).wrapping_add(offset);
            self.ldr(false, addr, rd)
        } else if test_bit(i, 10) {
            self.decode_thumb_hi_reg_ops(i)
        } else {
            self.decode_thumb_alu_ops(i)
        }
    }

    /// Decode transfer of data with register offset
    fn decode_thumb_transfer_reg(&mut self, i: u16) -> usize {
        let ro = ((i >> 6) & 0x7) as usize;
        let rb = ((i >> 3) & 0x7) as usize;
        let rd = (i & 0x7) as usize;
        let addr = self.read_reg(rb).wrapping_add(self.read_reg(ro));

        if test_bit(i, 9) {
            match (i & bits(10, 11)) >> 10 {
                0b00 => self.strh(addr, rd),
                0b01 => 0, // LDSB
                0b10 => self.ldrh(addr, rd),
                0b11 => 0, // LDSH
                _ => unreachable!()
            }
        } else {
            match (i & bits(10, 11)) >> 10 {
                0b00 => self.str(false, addr, rd),
                0b01 => self.str(true, addr, rd),
                0b10 => self.ldr(false, addr, rd),
                0b11 => self.ldr(true, addr, rd),
                _ => unreachable!()
            }
        }
    }

    /// Decode alu operations
    fn decode_thumb_hi_reg_ops(&mut self, i: u16) -> usize {
        let rs = ((i >> 3) & 0xF) as usize;
        let rd = (((i >> 4) & 8) | (i & 7)) as usize;
        match (i & bits(8, 9)) >> 8 {
            0b00 => self.add(false, rd, self.read_reg(rd), self.read_reg(rs)),
            0b01 => self.cmp(self.read_reg(rd), self.read_reg(rs)),
            0b10 => self.mov(false, rd, self.read_reg(rs)),
            0b11 => self.bx(self.read_reg(rs)),
            _ => unreachable!(),
        }
        0
    }

    /// Decode alu operations
    fn decode_thumb_alu_ops(&mut self, i: u16) -> usize {
        let rs = ((i >> 3) & 7) as usize;
        let rd = (i & 7) as usize;
        match (i & bits(6, 9)) >> 6 {
            0x0 => self.and(true, rd, self.read_reg(rd), self.read_reg(rs)),
            0x1 => self.eor(true, rd, self.read_reg(rd), self.read_reg(rs)),
            0x2 => {
                let shift_amount = self.read_reg(rs) & 0x1F;
                let shifted = self.lsl(shift_amount != 0, self.read_reg(rd), shift_amount);
                self.write_reg(rd, shifted);
                return 1;
            },
            0x3 => {
                let shift_amount = self.read_reg(rs) & 0x1F;
                let shifted = self.lsr(shift_amount != 0, self.read_reg(rd), shift_amount);
                self.write_reg(rd, shifted);
                return 1;
            },
            0x4 => {
                let shift_amount = self.read_reg(rs) & 0x1F;
                let shifted = self.asr(shift_amount != 0, self.read_reg(rd), shift_amount);
                self.write_reg(rd, shifted);
                return 1;
            },
            0x5 => self.adc(true, rd, self.read_reg(rd), self.read_reg(rs)),
            0x6 => self.adc(true, rd, self.read_reg(rd), !self.read_reg(rs)),
            0x7 => {
                let shift_amount = self.read_reg(rs) & 0x1F;
                let shifted = self.ror(self.read_reg(rd), shift_amount);
                self.write_reg(rd, shifted);
                return 1;
            },
            0x8 => self.tst(self.read_reg(rd), self.read_reg(rs)),
            0x9 => self.sub(true, rd, 0, self.read_reg(rs)),
            0xA => self.cmp(self.read_reg(rd), self.read_reg(rs)),
            0xB => self.cmn(self.read_reg(rd), self.read_reg(rs)),
            0xC => self.orr(true, rd, self.read_reg(rd), self.read_reg(rs)),
            0xD => return self.mul(true, rd, rd, rs),
            0xE => self.bic(true, rd, self.read_reg(rd), self.read_reg(rs)),
            0xF => self.mov(true, rd, !self.read_reg(rs)),
            _ => unreachable!()
        }
        0
    }

    /// Decode transfer of words and bytes with immediate offset
    fn decode_thumb_transfer(&mut self, i: u16) -> usize {
        let offset = ((i >> 6) & 0x1F) as u32;
        let rb = ((i >> 3) & 0x7) as usize;
        let rd = (i & 0x7) as usize;
        let base_addr = self.read_reg(rb);
        match (i & bits(11, 12)) >> 11 {
            0b00 => self.str(false, base_addr.wrapping_add(offset << 2), rd),
            0b01 => self.ldr(false, base_addr.wrapping_add(offset << 2), rd),
            0b10 => self.str(true, base_addr.wrapping_add(offset), rd),
            0b11 => self.ldr(true, base_addr.wrapping_add(offset), rd),
            _ => unreachable!()
        }
    }

    /// Decode transfer of halfwords and stack-relative transfers
    fn decode_thumb_transfer_ext(&mut self, i: u16) -> usize {
        if test_bit(i, 12) {
            let offset = ((i >> 5) & 0x3E) as u32;
            let rb = ((i >> 3) & 0x7) as usize;
            let rd = (i & 0x7) as usize;
            let base_addr = self.read_reg(rb);
            if test_bit(i, 11) {
                self.ldrh(base_addr.wrapping_add(offset), rd)
            } else {
                self.strh(base_addr.wrapping_add(offset), rd)
            }
        } else {
            let rd = ((i >> 8) & 0x7) as usize;
            let offset = ((i & 0xFF) << 2) as u32;
            let base_addr = self.read_reg(SP_REG);
            if test_bit(i, 11) {
                self.ldr(false, base_addr.wrapping_add(offset), rd)
            } else {
                self.str(false, base_addr.wrapping_add(offset), rd)
            }
        }
    }

    /// Decode stack-related instructions
    fn decode_thumb_stack(&mut self, i: u16) -> usize {
        if test_bit(i, 12) {
            if test_bit(i, 10) {
                self.decode_push_pop(i)
            } else {
                let offset = ((i & 0x7F) << 2) as u32;
                if test_bit(i, 7) {
                    self.add(false, SP_REG, self.read_reg(SP_REG), !offset + 1);
                } else {
                    self.add(false, SP_REG, self.read_reg(SP_REG), offset);
                }
                0
            }
        } else {
            let rd = ((i >> 8) & 0x7) as usize;
            let offset = ((i & 0xFF) << 2) as u32;
            if test_bit(i, 11) {
                self.add(false, rd, self.read_reg(SP_REG), offset);
            } else {
                self.add(false, rd, self.read_reg(PC_REG), offset);
            }
            0
        }
    }

    /// Decode push and pop for thumb
    fn decode_push_pop(&mut self, i: u16) -> usize {
        let mut rlist = i & 0xFF;
        let base_addr = self.read_reg(SP_REG);
        if test_bit(i, 11) {
            // POP
            rlist |= (i & bit(8)) << 7;
            let offset = rlist.count_ones() * 4;
            let end_addr = base_addr.wrapping_add(offset);
            self.write_reg(SP_REG, end_addr);
            self.ldm(base_addr, rlist as u32, false, false)
        } else {
            // PUSH
            rlist |= (i & bit(8)) << 6;
            let offset = rlist.count_ones() * 4;
            let end_addr = base_addr.wrapping_sub(offset);
            self.write_reg(SP_REG, end_addr);
            self.stm(end_addr, rlist as u32, false, false)
        }
    }

    /// Decode load/store multiple and conditional branches
    fn decode_thumb_other_hi(&mut self, i: u16) -> usize {
        if test_bit(i, 12) {
            self.decode_thumb_cond_branch(i)
        } else {
            self.decode_thumb_transfer_mul(i)
        }
    }

    /// Decode conditional branches and software interrupt
    fn decode_thumb_cond_branch(&mut self, i: u16) -> usize {
        let cond = ((i >> 8) & 0xF) as u32;
        if cond == 0xF {
            self.swi();
        } else if self.check_cond(cond) {
            let offset_i = ((i & 0xFF) as u8) as i8;
            let offset = ((offset_i as i32) << 1) as u32;
            let current_pc = self.read_reg(PC_REG).wrapping_sub(2);
            self.write_reg(PC_REG, current_pc.wrapping_add(offset));
        }
        0
    }

    /// Decode LDMIA and STMIA
    fn decode_thumb_transfer_mul(&mut self, i: u16) -> usize {
        let rb = ((i >> 8) & 0x7) as usize;
        let rlist = (i & 0xFF) as u32;
        let base_addr = self.read_reg(rb);
        let offset = rlist.count_ones() * 4;
        let end_addr = base_addr.wrapping_add(offset);
        self.write_reg(rb, end_addr);
        if test_bit(i, 11) {
            self.ldm(base_addr, rlist, false, false)
        } else {
            self.stm(base_addr, rlist, false, false)
        }
    }

    /// Decode branches
    fn decode_thumb_branch(&mut self, i: u16) -> usize {
        match (i & bits(11, 12)) >> 11 {
            0b00 => {   // B
                let mut offset_imm = (i & bits(0, 10)) * 2;
                if test_bit(offset_imm, 11) {
                    offset_imm |= bits(12, 15);
                }
                let offset = ((offset_imm as i16) as i32) as u32;
                let pc = self.read_reg(PC_REG).wrapping_sub(2);
                self.write_reg(PC_REG, pc.wrapping_add(offset));
            },
            0b01 => {self.undefined();},
            0b10 => {   // Long branch
                let mut imm = i & bits(0, 10);
                if test_bit(imm, 10) {
                    imm |= bits(11, 15);
                }
                let imm32 = ((imm as i16) as i32) as u32;
                let target_addr = self.read_reg(PC_REG).wrapping_sub(2).wrapping_add(imm32 << 12);
                self.write_reg(LINK_REG, target_addr);
            },
            0b11 => {   // BL
                let imm = ((i & bits(0, 10)) * 2) as u32;
                let return_addr = self.read_reg(PC_REG).wrapping_sub(2);
                let dest = self.read_reg(LINK_REG).wrapping_add(imm);
                self.write_reg(PC_REG, dest);
                self.write_reg(LINK_REG, return_addr | 1);
            },
            _ => unreachable!()
        }
        0
    }
}