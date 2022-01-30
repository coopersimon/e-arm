
use super::decode_cond;
use crate::{
    core::{
        ARMCondition,
        constants::*,
        armv4::instructions::*,
        armv5::instructions::*
    },
    common::u32::*
};

/// Decode an ARM instruction.
pub fn decode_instruction(i: u32) -> ARMv5Instruction {
    const COPROC: u32 = 0b11 << 26;
    const BRANCH: u32 = 0b10 << 26;
    const TRANSFER: u32 = 0b01 << 26;
    const ALU: u32 = 0b00 << 26;
    let cond = decode_cond(i >> 28);
    let never = cond == ARMCondition::NV;
    let instr = match i & bits(26, 27) {
        COPROC      => decode_coproc(never, i),
        BRANCH      => decode_branch(never, i),
        TRANSFER    => decode_transfer(never, i),
        ALU         => decode_alu(i),
        _ => unreachable!(),
    };
    ARMv5Instruction{cond, instr}
}

/// Decode a coprocessor or SWI instruction.
/// i has the value cccc11...
fn decode_coproc(never: bool, i: u32) -> ARMv5InstructionType {
    if test_bit(i, 25) {
        if test_bit(i, 24) {
            ARMv4InstructionType::SWI{comment: i & 0xFF_FFFF}.into()
        } else {
            decode_coproc_op(never, i)
        }
    } else {
        decode_coproc_transfer(never, i)
    }
}

/// Decode a coprocessor register transfer or data op.
fn decode_coproc_op(never: bool, i: u32) -> ARMv5InstructionType {
    let reg_n = ((i >> 16) & 0xF) as usize;
    let reg_d = ((i >> 12) & 0xF) as usize;
    let coproc = ((i >> 8) & 0xF) as usize;
    let info = (i >> 5) & 0x7;
    let reg_m = (i & 0xF) as usize;
    if test_bit(i, 4) {
        let op = (i >> 21) & 0x7;
        if test_bit(i, 20) {
            if never {
                ARMv5InstructionType::MRC2{coproc, coproc_reg: reg_n, arm_reg: reg_d, op_reg: reg_m, op, info}
            } else {
                ARMv4InstructionType::MRC{coproc, coproc_reg: reg_n, arm_reg: reg_d, op_reg: reg_m, op, info}.into()
            }
        } else {
            if never {
                ARMv5InstructionType::MCR2{coproc, coproc_reg: reg_n, arm_reg: reg_d, op_reg: reg_m, op, info}
            } else {
                ARMv4InstructionType::MCR{coproc, coproc_reg: reg_n, arm_reg: reg_d, op_reg: reg_m, op, info}.into()
            }
        }
    } else {
        let op = (i >> 20) & 0xF;
        if never {
            ARMv5InstructionType::CDP2{op, reg_n, reg_d, info, reg_m, coproc}
        } else {
            ARMv4InstructionType::CDP{op, reg_n, reg_d, info, reg_m, coproc}.into()
        }
    }
}

/// Decode a coprocessor data transfer with memory.
fn decode_coproc_transfer(never: bool, i: u32) -> ARMv5InstructionType {
    if i & bits(21, 24) == bit(22) {
        // Post index + Writeback = Double-reg transfer
        decode_coproc_double_register(i)
    } else {
        decode_coproc_memory_transfer(never, i)
    }
}

fn decode_coproc_double_register(i: u32) -> ARMv5InstructionType {
    let arm_reg_n = ((i >> 16) & 0xF) as usize;
    let arm_reg_d = ((i >> 12) & 0xF) as usize;
    let coproc = ((i >> 8) & 0xF) as usize;
    let op_reg = (i & 0xF) as usize;
    let op = (i >> 4) & 0xF;
    if test_bit(i, 20) {
        ARMv5InstructionType::MRRC{coproc, arm_reg_n, arm_reg_d, op_reg, op}
    } else {
        ARMv5InstructionType::MCRR{coproc, arm_reg_n, arm_reg_d, op_reg, op}
    }
}

fn decode_coproc_memory_transfer(never: bool, i: u32) -> ARMv5InstructionType {
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
        if never {
            ARMv5InstructionType::LDC2{coproc, coproc_reg, transfer_len, transfer_params, offset}
        } else {
            ARMv4InstructionType::LDC{coproc, coproc_reg, transfer_len, transfer_params, offset}.into()
        }
    } else {
        if never {
            ARMv5InstructionType::STC2{coproc, coproc_reg, transfer_len, transfer_params, offset}
        } else {
            ARMv4InstructionType::STC{coproc, coproc_reg, transfer_len, transfer_params, offset}.into()
        }
    }
}

/// Decode a branch or block transfer instruction.
/// i has the value cccc10...
fn decode_branch(never: bool, i: u32) -> ARMv5InstructionType {
    if test_bit(i, 25) {
        let raw_offset = i & 0xFFFFFF;

        if never {
            let halfword_offset = (i >> 23) & 2;
            let offset = if test_bit(raw_offset, 23) {
                (raw_offset | 0xFF000000) << 2
            } else {
                raw_offset << 2
            } | halfword_offset;

            ARMv5InstructionType::BLXI{offset}
        } else {
            let offset = if test_bit(raw_offset, 23) {
                (raw_offset | 0xFF000000) << 2
            } else {
                raw_offset << 2
            };
    
            if test_bit(i, 24) {
                ARMv4InstructionType::BL{offset}.into()
            } else {
                ARMv4InstructionType::B{offset}.into()
            }
        }
    } else {
        decode_transfer_multiple(i).into()
    }
}

/// Decode a transfer multiple instruction.
/// Writes registers low-high into low-high addresses.
fn decode_transfer_multiple(i: u32) -> ARMv4InstructionType {
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
fn decode_transfer(never: bool, i: u32) -> ARMv5InstructionType {
    let base_reg = ((i >> 16) & 0xF) as usize;
    let data_reg = ((i >> 12) & 0xF) as usize;
    let offset = decode_offset(i);

    let byte = test_bit(i, 22);
    let inc = test_bit(i, 23);
    let pre_index = test_bit(i, 24);
    let writeback = test_bit(i, 21);
    let transfer_params = TransferParams {
        base_reg, inc, pre_index, writeback
    };

    if test_bit(i, 20) {
        if never && pre_index && byte && !writeback && data_reg == PC_REG {
            ARMv5InstructionType::PLD{transfer_params, offset}
        } else if byte {
            ARMv4InstructionType::LDRB{transfer_params, data_reg, offset}.into()
        } else {
            ARMv4InstructionType::LDR{transfer_params, data_reg, offset}.into()
        }
    } else {
        if byte {
            ARMv4InstructionType::STRB{transfer_params, data_reg, offset}.into()
        } else {
            ARMv4InstructionType::STR{transfer_params, data_reg, offset}.into()
        }
    }
}

/// Decode an ALU instruction.
/// i has the value cccc00...
fn decode_alu(i: u32) -> ARMv5InstructionType {
    // If bit 25 is 0, then bits 7 and 4 can determine if it's an extension instruction.
    if !test_bit(i, 25) && test_bit(i, 7) && test_bit(i, 4) {   // TODO: optimise this check
        decode_other(i)
    } else {
        decode_data_proc(i)
    }
}

/// Decode extended transfers, SWP and multiplication.
/// All of these instructions have bit25 = 0 and bits7 and 4 = 1.
/// The exact instruction depends on the value of bits 24, 6 and 5.
fn decode_other(i: u32) -> ARMv5InstructionType {
    if (i & bits(5, 6)) == 0 {
        if test_bit(i, 24) {
            decode_swp(i).into()
        } else {
            decode_multiply(i).into()
        }
    } else {
        decode_transfer_halfword(i)
    }
}

/// Decode a data swap.
fn decode_swp(i: u32) -> ARMv4InstructionType {
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
fn decode_multiply(i: u32) -> ARMv4InstructionType {
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
        _ => ARMv4InstructionType::UND, // ARMv6+: Other multiplies
    }
}

/// Decode a data processing instruction.
fn decode_data_proc(i: u32) -> ARMv5InstructionType {
    let set_flags = test_bit(i, 20);
    let rn = ((i >> 16) & 0xF) as usize;
    let rd = ((i >> 12) & 0xF) as usize;

    let compare = || {
        set_flags && (rd == 0)
    };

    match (i >> 21) & 0xF {
        0x0 => ARMv4InstructionType::AND{rd, rn, op2: decode_alu_op2(i), set_flags}.into(),
        0x1 => ARMv4InstructionType::EOR{rd, rn, op2: decode_alu_op2(i), set_flags}.into(),
        0x2 => ARMv4InstructionType::SUB{rd, rn, op2: decode_alu_op2(i), set_flags}.into(),
        0x3 => ARMv4InstructionType::RSB{rd, rn, op2: decode_alu_op2(i), set_flags}.into(),
        0x4 => ARMv4InstructionType::ADD{rd, rn, op2: decode_alu_op2(i), set_flags}.into(),
        0x5 => ARMv4InstructionType::ADC{rd, rn, op2: decode_alu_op2(i), set_flags}.into(),
        0x6 => ARMv4InstructionType::SBC{rd, rn, op2: decode_alu_op2(i), set_flags}.into(),
        0x7 => ARMv4InstructionType::RSC{rd, rn, op2: decode_alu_op2(i), set_flags}.into(),
        0xC => ARMv4InstructionType::ORR{rd, rn, op2: decode_alu_op2(i), set_flags}.into(),
        0xE => ARMv4InstructionType::BIC{rd, rn, op2: decode_alu_op2(i), set_flags}.into(),
        0x8 => if compare() {
            ARMv4InstructionType::TST{rn, op2: decode_alu_op2(i)}.into()
        } else {
            decode_data_proc_ext_00(i, rn, rd)
        },
        0x9 => if compare() {
            ARMv4InstructionType::TEQ{rn, op2: decode_alu_op2(i)}.into()
        } else {
            decode_data_proc_ext_01(i, rn, rd)
        },
        0xA => if compare() {
            ARMv4InstructionType::CMP{rn, op2: decode_alu_op2(i)}.into()
        } else {
            decode_data_proc_ext_10(i, rn, rd)
        },
        0xB => if compare() {
            ARMv4InstructionType::CMN{rn, op2: decode_alu_op2(i)}.into()
        } else {
            decode_data_proc_ext_11(i, rn, rd)
        },
        0xD => ARMv4InstructionType::MOV{rd, op2: decode_alu_op2(i), set_flags}.into(),
        0xF => ARMv4InstructionType::MVN{rd, op2: decode_alu_op2(i), set_flags}.into(),
        _ => unreachable!(),
    }
}

/// Decode the second operand of an arithmetic / logic instruction.
/// Can involve a shift.
/// This instruction _may_ set the carry flag for certain instructions.
/// It _will_ increment the program counter if a shift-by-register is involved.
/// 
/// Extracts a value from the lower 12 bits based on the 25th bit.
fn decode_alu_op2(i: u32) -> ALUOperand {
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
fn decode_offset(i: u32) -> ShiftOperand {
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

fn decode_data_proc_ext_00(i: u32, rn: usize, rd: usize) -> ARMv5InstructionType {
    if rn == 0xF {
        if (i & 0xFFF) == 0 {
            ARMv4InstructionType::MRS{spsr: false, rd}.into()
        } else {
            ARMv4InstructionType::UND.into()
        }
    } else {
        let rm = (i & 0xF) as usize;
        if i & 0xFF0 == 0x050 {
            ARMv5InstructionType::QADD{rd, rm, rn}
        } else if test_bit(i, 7) && !test_bit(i, 4) {
            let rs = ((i >> 8) & 0xF) as usize;
            ARMv5InstructionType::SMLAxy{rd: rn, rs, rm, rn: rd, y: test_bit(i, 6), x: test_bit(i, 5)}
        } else {
            ARMv4InstructionType::UND.into()
        }
    }
}

fn decode_data_proc_ext_01(i: u32, rn: usize, rd: usize) -> ARMv5InstructionType {
    if rd == 0xF {
        decode_msr(i, false)
    } else {
        let rm = (i & 0xF) as usize;
        if i & 0xFF0 == 0x050 {
            ARMv5InstructionType::QSUB{rd, rm, rn}
        } else if test_bit(i, 7) && !test_bit(i, 4) {
            let rs = ((i >> 8) & 0xF) as usize;
            if test_bit(i, 5) {
                ARMv5InstructionType::SMULWy{rd: rn, rs, rm, y: test_bit(i, 6)}
            } else {
                ARMv5InstructionType::SMLAWy{rd: rn, rs, rm, rn: rd, y: test_bit(i, 6)}
            }
        } else {
            ARMv4InstructionType::UND.into()
        }
    }
}

fn decode_data_proc_ext_10(i: u32, rn: usize, rd: usize) -> ARMv5InstructionType {
    if rn == 0xF {
        if (i & 0xFFF) == 0 {
            ARMv4InstructionType::MRS{spsr: true, rd}.into()
        } else {
            ARMv4InstructionType::UND.into()
        }
    } else {
        let rm = (i & 0xF) as usize;
        if i & 0xFF0 == 0x050 {
            ARMv5InstructionType::QDADD{rd, rm, rn}
        } else if test_bit(i, 7) && !test_bit(i, 4) {
            let rs = ((i >> 8) & 0xF) as usize;
            ARMv5InstructionType::SMLALxy{rd_hi: rn, rd_lo: rd, rs, rm, y: test_bit(i, 6), x: test_bit(i, 5)}
        } else {
            ARMv4InstructionType::UND.into()
        }
    }
}

fn decode_data_proc_ext_11(i: u32, rn: usize, rd: usize) -> ARMv5InstructionType {
    if rd == 0xF {
        decode_msr(i, true)
    } else {
        let rm = (i & 0xF) as usize;
        if i & 0xFF0 == 0x050 {
            ARMv5InstructionType::QDSUB{rd, rm, rn}
        } else if i & 0xFF0 == 0xF10 {
            ARMv5InstructionType::CLZ{rd, rm}
        } else if test_bit(i, 7) && !test_bit(i, 4) {
            let rs = ((i >> 8) & 0xF) as usize;
            ARMv5InstructionType::SMULxy{rd: rn, rs, rm, y: test_bit(i, 6), x: test_bit(i, 5)}
        } else {
            ARMv4InstructionType::UND.into()
        }
    }
}

/// Decode MSR.
/// Also decode BX, BLX, BKPT if spsr is clear.
fn decode_msr(i: u32, spsr: bool) -> ARMv5InstructionType {
    if test_bit(i, 25) {
        let mask = fsxc_mask(i);
        let shift = (i >> 7) & 0x1E;
        let imm = i & 0xFF;
        let data = OpData::Immediate(imm.rotate_right(shift));
        ARMv4InstructionType::MSR{spsr, mask, data}.into()
    } else if (i & 0xFF0) == 0 {
        let mask = fsxc_mask(i);
        let data = OpData::Register((i & 0xF) as usize);
        ARMv4InstructionType::MSR{spsr, mask, data}.into()
    } else if !spsr {
        let op = (i >> 4) & 0xF;
        match op {
            1 => ARMv4InstructionType::BX{reg: (i & 0xF) as usize}.into(),
            // 2 => BXJ
            3 => ARMv5InstructionType::BLXR{reg: (i & 0xF) as usize},
            7 => ARMv5InstructionType::BKPT{comment: (i & 0xFFF00) >> 4 | (i & 0xF)},
            _ => ARMv4InstructionType::UND.into()
        }
    } else {
        ARMv4InstructionType::UND.into()
    }
}

/// Decode a load or store halfword instruction.
fn decode_transfer_halfword(i: u32) -> ARMv5InstructionType {
    let base_reg = ((i >> 16) & 0xF) as usize;
    let data_reg = ((i >> 12) & 0xF) as usize;
    let offset = decode_halfword_offset(i);

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
        }.into()
    } else {
        match (i & bits(5, 6)) >> 5 {
            0b00 => ARMv4InstructionType::UND.into(),  // SWP
            0b01 => ARMv4InstructionType::STRH{transfer_params, data_reg, offset}.into(),
            0b10 => ARMv5InstructionType::LDRD{transfer_params, data_reg, offset},
            0b11 => ARMv5InstructionType::STRD{transfer_params, data_reg, offset},
            _ => unreachable!()
        }
    }
}

/// Decode the offset of a halfword load/store instruction.
fn decode_halfword_offset(i: u32) -> OpData {
    if test_bit(i, 22) {
        OpData::Immediate(((i >> 4) & 0xF0) | (i & 0xF))
    } else {
        OpData::Register((i & 0xF) as usize)
    }
}

/// Make fsxc mask for manipulating PSR.
/// Creates a 32-bit mask based on the status of certain bits in the input value.
/// Bit 19: flags. Bits 24-31
/// Bit 18: status. Bits 16-23
/// Bit 17: extension. Bits 8-15
/// Bit 16: control. Bits 0-7
const fn fsxc_mask(from: u32) -> u32 {
    let mut mask = 0;
    if test_bit(from, 19) {
        mask |= 0xFF00_0000;
    }
    if test_bit(from, 18) {
        mask |= 0x00FF_0000;
    }
    if test_bit(from, 17) {
        mask |= 0x0000_FF00;
    }
    if test_bit(from, 16) {
        mask |= 0x0000_00FF;
    }
    mask
}
