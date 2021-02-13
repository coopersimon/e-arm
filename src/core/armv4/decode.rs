
use super::{
    super::{
        constants::*,
        ARMCore, ARMCondition
    },
    instructions::*
};
use crate::common::u32::*;
use crate::memory::Mem32;

pub trait ARMv4Decode<M: Mem32<Addr = u32>>: ARMCore<M> {
    /// Decode the instruction.
    fn decode_instruction(&mut self, i: u32) -> ARMv4Instruction {
        const COPROC: u32 = 0b11 << 26;
        const BRANCH: u32 = 0b10 << 26;
        const TRANSFER: u32 = 0b01 << 26;
        const ALU: u32 = 0b00 << 26;
        let cond = self.decode_cond(i >> 28);
        let instr = match i & bits(26, 27) {
            COPROC      => self.decode_coproc(i),
            BRANCH      => self.decode_branch(i),
            TRANSFER    => self.decode_transfer(i),
            ALU         => self.decode_alu(i),
            _ => unreachable!(),
        };
        ARMv4Instruction::new(cond, instr)
    }

    /// Decode the condition from the instruction.
    fn decode_cond(&self, cond_bits: u32) -> ARMCondition {
        use ARMCondition::*;
        match cond_bits {
            0x0 => EQ,
            0x1 => NE,
            0x2 => CS,
            0x3 => CC,
            0x4 => MI,
            0x5 => PL,
            0x6 => VS,
            0x7 => VC,
            0x8 => HI,
            0x9 => LS,
            0xA => GE,
            0xB => LT,
            0xC => GT,
            0xD => LE,
            0xE => AL,
            0xF => AL,  // reserved.
            _ => unreachable!()
        }
    }

    // Decoding functions

    /// Decode a coprocessor or SWI instruction.
    /// i has the value cccc11...
    fn decode_coproc(&mut self, i: u32) -> ARMv4InstructionType {
        if test_bit(i, 25) {
            if test_bit(i, 24) {
                ARMv4InstructionType::SWI{comment: i & 0xFF_FFFF}
            } else {
                self.decode_coproc_op(i)
            }
        } else {
            self.decode_coproc_transfer(i)
        }
    }

    /// Decode a coprocessor register transfer or data op.
    fn decode_coproc_op(&mut self, i: u32) -> ARMv4InstructionType {
        let reg_n = ((i >> 16) & 0xF) as usize;
        let reg_d = ((i >> 12) & 0xF) as usize;
        let coproc = ((i >> 8) & 0xF) as usize;
        let info = (i >> 5) & 0x7;
        let reg_m = (i & 0xF) as usize;
        if test_bit(i, 4) {
            let op = (i >> 21) & 0x7;
            if test_bit(i, 20) {
                ARMv4InstructionType::MRC{coproc, coproc_reg: reg_n, arm_reg: reg_d, op_reg: reg_m, op, info}
            } else {
                ARMv4InstructionType::MCR{coproc, coproc_reg: reg_n, arm_reg: reg_d, op_reg: reg_m, op, info}
            }
        } else {
            let op = (i >> 20) & 0xF;
            ARMv4InstructionType::CDP{op, reg_n, reg_d, info, reg_m, coproc}
        }
    }

    /// Decode a coprocessor data transfer with memory.
    fn decode_coproc_transfer(&mut self, i: u32) -> ARMv4InstructionType {
        let base_reg = ((i >> 16) & 0xF) as usize;
        let coproc_reg = ((i >> 12) & 0xF) as usize;
        let coproc = ((i >> 8) & 0xF) as usize;
        let offset = (i & 0xFF) << 2;

        let transfer_len = test_bit(i, 22);
        let inc = test_bit(i, 23);
        let pre_index = test_bit(i, 24);
        let writeback = test_bit(i, 21);
        let transfer_params = TransferParams {
            base_reg, inc, pre_index, writeback
        };
        if test_bit(i, 20) {
            ARMv4InstructionType::LDC{coproc, coproc_reg, transfer_len, transfer_params, offset}
        } else {
            ARMv4InstructionType::STC{coproc, coproc_reg, transfer_len, transfer_params, offset}
        }
    }

    /// Decode a branch or block transfer instruction.
    /// i has the value cccc10...
    fn decode_branch(&mut self, i: u32) -> ARMv4InstructionType {
        if test_bit(i, 25) {
            let raw_offset = i & 0xFFFFFF;
            let offset = if test_bit(raw_offset, 23) {
                (raw_offset | 0xFF000000) << 2
            } else {
                raw_offset << 2
            };

            if test_bit(i, 24) {
                ARMv4InstructionType::BL{offset}
            } else {
                ARMv4InstructionType::B{offset}
            }
        } else {
            self.decode_transfer_multiple(i)
        }
    }

    /// Decode a transfer multiple instruction.
    /// Writes registers low-high into low-high addresses.
    fn decode_transfer_multiple(&mut self, i: u32) -> ARMv4InstructionType {
        let base_reg = ((i >> 16) & 0xF) as usize;
        if base_reg == PC_REG {
            return ARMv4InstructionType::UND;
        }
        let inc = test_bit(i, 23);
        let pre_index = test_bit(i, 24);
        let writeback = test_bit(i, 21);
        let transfer_params = TransferParams {
            base_reg, inc, pre_index, writeback
        };
        let reg_list = i & 0xFFFF;
        let load_from_user = test_bit(i, 22);

        if test_bit(i, 20) {
            ARMv4InstructionType::LDM{transfer_params, reg_list, load_from_user}
        } else {
            ARMv4InstructionType::STM{transfer_params, reg_list, load_from_user}
        }
    }

    /// Decode a single transfer instruction (load or store).
    /// i has the value cccc01...
    fn decode_transfer(&mut self, i: u32) -> ARMv4InstructionType {
        let base_reg = ((i >> 16) & 0xF) as usize;
        let data_reg = ((i >> 12) & 0xF) as usize;
        let offset = self.decode_offset(i);

        let byte = test_bit(i, 22);
        let inc = test_bit(i, 23);
        let pre_index = test_bit(i, 24);
        let writeback = test_bit(i, 21);
        let transfer_params = TransferParams {
            base_reg, inc, pre_index, writeback
        };
        if test_bit(i, 20) {
            if byte {
                ARMv4InstructionType::LDRB{transfer_params, data_reg, offset}
            } else {
                ARMv4InstructionType::LDR{transfer_params, data_reg, offset}
            }
        } else {
            if byte {
                ARMv4InstructionType::STRB{transfer_params, data_reg, offset}
            } else {
                ARMv4InstructionType::STR{transfer_params, data_reg, offset}
            }
        }
    }

    /// Decode an ALU instruction.
    /// i has the value cccc00...
    fn decode_alu(&mut self, i: u32) -> ARMv4InstructionType {
        // If bit 25 is 0, then bits 7 and 4 can determine if it's an extension instruction.
        if !test_bit(i, 25) && test_bit(i, 7) && test_bit(i, 4) {   // TODO: optimise this check
            self.decode_other(i)
        } else {
            self.decode_data_proc(i)
        }
    }

    /// Decode extended transfers, SWP and multiplication.
    /// All of these instructions have bit25 = 0 and bits7 and 4 = 1.
    /// The exact instruction depends on the value of bits 24, 6 and 5.
    fn decode_other(&mut self, i: u32) -> ARMv4InstructionType {
        if (i & bits(5, 6)) == 0 {
            if test_bit(i, 24) {
                self.decode_swp(i)
            } else {
                self.decode_multiply(i)
            }
        } else {
            self.decode_transfer_halfword(i)
        }
    }

    /// Decode a data swap.
    fn decode_swp(&mut self, i: u32) -> ARMv4InstructionType {
        const CLEARED_BITS: u32 = bit(23) | bits(20, 21) | bits(8, 11);
        if i & CLEARED_BITS == 0 {
            let rn = ((i >> 16) & 0xF) as usize;
            let rd = ((i >> 12) & 0xF) as usize;
            let rm = (i & 0xF) as usize;
            if test_bit(i, 22) {
                ARMv4InstructionType::SWPB{rn, rd, rm}
            } else {
                ARMv4InstructionType::SWP{rn, rd, rm}
            }
        } else {
            ARMv4InstructionType::UND
        }
    }

    /// Decode a multiply instruction.
    fn decode_multiply(&mut self, i: u32) -> ARMv4InstructionType {
        let set_flags = test_bit(i, 20);
        let rd = ((i >> 16) & 0xF) as usize;
        let rn = ((i >> 12) & 0xF) as usize;
        let rs = ((i >> 8) & 0xF) as usize;
        let rm = (i & 0xF) as usize;
        match (i >> 21) & 0xF {
            0x0 => ARMv4InstructionType::MUL{set_flags, rd, rs, rm},
            0x1 => ARMv4InstructionType::MLA{set_flags, rd, rn, rs, rm},
            0x4 => ARMv4InstructionType::UMULL{set_flags, rd_hi: rd, rd_lo: rn, rs, rm},
            0x5 => ARMv4InstructionType::UMLAL{set_flags, rd_hi: rd, rd_lo: rn, rs, rm},
            0x6 => ARMv4InstructionType::SMULL{set_flags, rd_hi: rd, rd_lo: rn, rs, rm},
            0x7 => ARMv4InstructionType::SMLAL{set_flags, rd_hi: rd, rd_lo: rn, rs, rm},
            _ => unreachable!("unknown MUL instruction"),
        }
    }

    /// Decode a data processing instruction.
    fn decode_data_proc(&mut self, i: u32) -> ARMv4InstructionType {
        let set_flags = test_bit(i, 20);
        let rn = ((i >> 16) & 0xF) as usize;
        let rd = ((i >> 12) & 0xF) as usize;

        let compare = || {
            set_flags && (rd == 0)
        };

        match (i >> 21) & 0xF {
            0x0 => ARMv4InstructionType::AND{rd, rn, op2: self.decode_alu_op2(i), set_flags},
            0x1 => ARMv4InstructionType::EOR{rd, rn, op2: self.decode_alu_op2(i), set_flags},
            0x2 => ARMv4InstructionType::SUB{rd, rn, op2: self.decode_alu_op2(i), set_flags},
            0x3 => ARMv4InstructionType::RSB{rd, rn, op2: self.decode_alu_op2(i), set_flags},
            0x4 => ARMv4InstructionType::ADD{rd, rn, op2: self.decode_alu_op2(i), set_flags},
            0x5 => ARMv4InstructionType::ADC{rd, rn, op2: self.decode_alu_op2(i), set_flags},
            0x6 => ARMv4InstructionType::SBC{rd, rn, op2: self.decode_alu_op2(i), set_flags},
            0x7 => ARMv4InstructionType::RSC{rd, rn, op2: self.decode_alu_op2(i), set_flags},
            0xC => ARMv4InstructionType::ORR{rd, rn, op2: self.decode_alu_op2(i), set_flags},
            0xE => ARMv4InstructionType::BIC{rd, rn, op2: self.decode_alu_op2(i), set_flags},
            0x8 => if compare() {
                ARMv4InstructionType::TST{rn, op2: self.decode_alu_op2(i)}
            } else {
                self.decode_ext_mode(i, rn, rd, false, false)
            },
            0x9 => if compare() {
                ARMv4InstructionType::TEQ{rn, op2: self.decode_alu_op2(i)}
            } else {
                self.decode_ext_mode(i, rn, rd, true, false)
            },
            0xA => if compare() {
                ARMv4InstructionType::CMP{rn, op2: self.decode_alu_op2(i)}
            } else {
                self.decode_ext_mode(i, rn, rd, false, true)
            },
            0xB => if compare() {
                ARMv4InstructionType::CMN{rn, op2: self.decode_alu_op2(i)}
            } else {
                self.decode_ext_mode(i, rn, rd, true, true)
            },
            0xD => ARMv4InstructionType::MOV{rd, op2: self.decode_alu_op2(i), set_flags},
            0xF => ARMv4InstructionType::MVN{rd, op2: self.decode_alu_op2(i), set_flags},
            _ => ARMv4InstructionType::UND,
        }
    }

    /// Decodes extension mode instructions.
    /// This includes MRS, MSR and BX
    /// This has multiple entry points so some data has already been decoded.
    fn decode_ext_mode(&mut self, i: u32, rn: usize, rd: usize, msr: bool, spsr: bool) -> ARMv4InstructionType {
        if msr && (rd == 0xF) {
            if !test_bit(i, 25) {
                match i & 0xFF0 {
                    0xF10 if rn == 0xF => ARMv4InstructionType::BX{reg: (i & 0xF) as usize},
                    0x000 => {
                        let mask = super::fsxc_mask(i);
                        let data = OpData::Register((i & 0xF) as usize);
                        ARMv4InstructionType::MSR{spsr, mask, data}
                    },
                    _ => ARMv4InstructionType::UND,
                }
            } else {
                let mask = super::fsxc_mask(i);
                let shift = (i >> 7) & 0x1E;
                let imm = i & 0xFF;
                let data = OpData::Immediate(imm.rotate_right(shift));
                ARMv4InstructionType::MSR{spsr, mask, data}
            }
        } else if !msr && (rn == 0xF) && (i & 0xFFF) == 0 {
            ARMv4InstructionType::MRS{spsr, rd}
        } else {
            ARMv4InstructionType::UND
        }
    }

    /// Decode the second operand of an arithmetic / logic instruction.
    /// Can involve a shift.
    /// This instruction _may_ set the carry flag for certain instructions.
    /// It _will_ increment the program counter if a shift-by-register is involved.
    /// 
    /// Extracts a value from the lower 12 bits based on the 25th bit.
    fn decode_alu_op2(&mut self, i: u32) -> ALUOperand {
        // Immediate value with rotate:
        if test_bit(i, 25) {
            let shift = (i >> 7) & 0x1E;
            let n = i & 0xFF;
            return ALUOperand::Normal(ShiftOperand::Immediate(n.rotate_right(shift)));
        }
        // Register value with optional shift:
        let reg = (i & 0xF) as usize;
        let shift_type = (i >> 5) & 3;
        if test_bit(i, 4) {
            let shift_reg = ((i >> 8) & 0xF) as usize;
            match shift_type {
                0 => ALUOperand::RegShift{op: RegShiftOperand::LSL, shift_reg, reg},
                1 => ALUOperand::RegShift{op: RegShiftOperand::LSR, shift_reg, reg},
                2 => ALUOperand::RegShift{op: RegShiftOperand::ASR, shift_reg, reg},
                3 => ALUOperand::RegShift{op: RegShiftOperand::ROR, shift_reg, reg},
                _ => unreachable!()
            }
        } else {
            let shift_amount = (i >> 7) & 0x1F;
            // Special shift cases when the immediate is 0:
            if shift_amount == 0 {
                return match shift_type {
                    0 => ALUOperand::Normal(ShiftOperand::Register(reg)),
                    1 => ALUOperand::Normal(ShiftOperand::LSR32{reg}),
                    2 => ALUOperand::Normal(ShiftOperand::ASR32{reg}),
                    3 => ALUOperand::Normal(ShiftOperand::RRX{reg}),
                    _ => unreachable!()
                };
            }
            match shift_type {
                0 => ALUOperand::Normal(ShiftOperand::LSL{shift_amount, reg}),
                1 => ALUOperand::Normal(ShiftOperand::LSR{shift_amount, reg}),
                2 => ALUOperand::Normal(ShiftOperand::ASR{shift_amount, reg}),
                3 => ALUOperand::Normal(ShiftOperand::ROR{shift_amount, reg}),
                _ => unreachable!()
            }
        }        
    }

    /// Decode the offset of a load/store instruction.
    fn decode_offset(&self, i: u32) -> ShiftOperand {
        // Immediate offset:
        if !test_bit(i, 25) {
            return ShiftOperand::Immediate(i & 0xFFF);
        }
        let reg = (i & 0xF) as usize;
        // Register shifted by immediate:
        let shift_amount = (i >> 7) & 0x1F;
        let shift_type = (i >> 5) & 3;
        if shift_amount == 0 {
            return match shift_type {
                0 => ShiftOperand::Register(reg),
                1 => ShiftOperand::LSR32{reg},
                2 => ShiftOperand::ASR32{reg},
                3 => ShiftOperand::RRX{reg},
                _ => unreachable!()
            };
        }
        match shift_type {
            0 => ShiftOperand::LSL{shift_amount, reg},
            1 => ShiftOperand::LSR{shift_amount, reg},
            2 => ShiftOperand::ASR{shift_amount, reg},
            3 => ShiftOperand::ROR{shift_amount, reg},
            _ => unreachable!()
        }
    }

    /// Decode a load or store halfword instruction.
    fn decode_transfer_halfword(&mut self, i: u32) -> ARMv4InstructionType {
        let base_reg = ((i >> 16) & 0xF) as usize;
        let data_reg = ((i >> 12) & 0xF) as usize;
        let offset = self.decode_halfword_offset(i);

        let inc = test_bit(i, 23);
        let pre_index = test_bit(i, 24);
        let writeback = test_bit(i, 21);
        let transfer_params = TransferParams {
            base_reg, inc, pre_index, writeback
        };

        if test_bit(i, 20) {
            match (i & bits(5, 6)) >> 5 {
                0b00 => ARMv4InstructionType::UND,  // MUL
                0b01 => ARMv4InstructionType::LDRH{transfer_params, data_reg, offset},
                0b10 => ARMv4InstructionType::LDRSB{transfer_params, data_reg, offset},
                0b11 => ARMv4InstructionType::LDRSH{transfer_params, data_reg, offset},
                _ => unreachable!()
            }
        } else {
            match (i & bits(5, 6)) >> 5 {
                0b00 => ARMv4InstructionType::UND,  // SWP
                0b01 => ARMv4InstructionType::STRH{transfer_params, data_reg, offset},
                0b10 => ARMv4InstructionType::UND,
                0b11 => ARMv4InstructionType::UND,
                _ => unreachable!()
            }
        }
    }

    /// Decode the offset of a halfword load/store instruction.
    fn decode_halfword_offset(&self, i: u32) -> OpData {
        if test_bit(i, 22) {
            OpData::Immediate(((i >> 4) & 0xF0) | (i & 0xF))
        } else {
            OpData::Register((i & 0xF) as usize)
        }
    }
}