/// ARM Thumb Instruction Set

use super::decode_cond;
use crate::{
    core::{
        constants::*,
        ARMCondition,
        armv4::instructions::*,
        armv5::instructions::*
    },
    common::u16::*
};

/// Decode the thumb instruction.
pub fn decode_thumb(i: u16) -> ARMv5Instruction {
    const ALU: u16          = 0b000 << 13;
    const ALU_IMM: u16      = 0b001 << 13;
    const OTHER_LO: u16     = 0b010 << 13;
    const TRANSFER: u16     = 0b011 << 13;
    const TRANSFER_EXT: u16 = 0b100 << 13;
    const STACK: u16        = 0b101 << 13;
    const OTHER_HI: u16     = 0b110 << 13;
    const BRANCH: u16       = 0b111 << 13;
    let instr = match i & bits(13, 15) {
        ALU =>          decode_thumb_alu(i).into(),
        ALU_IMM =>      decode_thumb_alu_imm(i).into(),
        OTHER_LO =>     decode_thumb_other_lo(i),
        TRANSFER =>     decode_thumb_transfer(i).into(),
        TRANSFER_EXT => decode_thumb_transfer_ext(i).into(),
        STACK =>        decode_thumb_stack(i),
        OTHER_HI =>     return decode_thumb_other_hi(i),
        BRANCH =>       decode_thumb_branch(i),
        _ => unreachable!(),
    };
    ARMv5Instruction{cond: ARMCondition::AL, instr}
}

/// Decode shift & arithmetic
fn decode_thumb_alu(i: u16) -> ARMv4InstructionType {
    let rs = ((i >> 3) & 0x7) as usize;
    let rd = (i & 0x7) as usize;
    match (i & bits(11, 12)) >> 11 {
        0b00 => {
            let shift_amount = ((i >> 6) & 0x1F) as u32;
            let op2 = ALUOperand::Normal(ShiftOperand::LSL{shift_amount, reg: rs});
            ARMv4InstructionType::MOV{rd, op2, set_flags: true}
        },
        0b01 => {
            let shift_amount = ((i >> 6) & 0x1F) as u32;
            let op2 = if shift_amount == 0 {
                ALUOperand::Normal(ShiftOperand::LSR32{reg: rs})
            } else {
                ALUOperand::Normal(ShiftOperand::LSR{shift_amount, reg: rs})
            };
            ARMv4InstructionType::MOV{rd, op2, set_flags: true}
        }
        0b10 => {
            let shift_amount = ((i >> 6) & 0x1F) as u32;
            let op2 = if shift_amount == 0 {
                ALUOperand::Normal(ShiftOperand::ASR32{reg: rs})
            } else {
                ALUOperand::Normal(ShiftOperand::ASR{shift_amount, reg: rs})
            };
            ARMv4InstructionType::MOV{rd, op2, set_flags: true}
        }
        0b11 => {
            let op2 = (i >> 6) & 0x7;
            match (i & bits(9, 10)) >> 9 {
                0b00 => ARMv4InstructionType::ADD{rd, rn: rs, op2: ALUOperand::Normal(ShiftOperand::Register(op2 as usize)), set_flags: true},
                0b01 => ARMv4InstructionType::SUB{rd, rn: rs, op2: ALUOperand::Normal(ShiftOperand::Register(op2 as usize)), set_flags: true},
                0b10 => ARMv4InstructionType::ADD{rd, rn: rs, op2: ALUOperand::Normal(ShiftOperand::Immediate(op2 as u32)), set_flags: true},
                0b11 => ARMv4InstructionType::SUB{rd, rn: rs, op2: ALUOperand::Normal(ShiftOperand::Immediate(op2 as u32)), set_flags: true},
                _ => unreachable!()
            }
        }
        _ => unreachable!()
    }
}

/// Decode move/compare/arith with 8-bit immediate
fn decode_thumb_alu_imm(i: u16) -> ARMv4InstructionType {
    let rd = ((i >> 8) & 0x7) as usize;
    let imm = (i & 0xFF) as u32;
    let op2 = ALUOperand::Normal(ShiftOperand::Immediate(imm));
    match (i & bits(11, 12)) >> 11 {
        0b00 => ARMv4InstructionType::MOV{rd, op2, set_flags: true},
        0b01 => ARMv4InstructionType::CMP{rn: rd, op2},
        0b10 => ARMv4InstructionType::ADD{rd, rn: rd, op2, set_flags: true},
        0b11 => ARMv4InstructionType::SUB{rd, rn: rd, op2, set_flags: true},
        _ => unreachable!()
    }
}

/// Decode other ALU operations, branch exchange, and register offset memory ops
fn decode_thumb_other_lo(i: u16) -> ARMv5InstructionType {
    if test_bit(i, 12) {
        decode_thumb_transfer_reg(i).into()
    } else if test_bit(i, 11) {
        // Load PC-relative
        let rd = ((i >> 8) & 0x7) as usize;
        let offset = ((i & 0xFF) << 2) as u32;
        ARMv4InstructionType::TLDRPC{data_reg: rd, offset}.into()
    } else if test_bit(i, 10) {
        decode_thumb_hi_reg_ops(i)
    } else {
        decode_thumb_alu_ops(i).into()
    }
}

/// Decode transfer of data with register offset
fn decode_thumb_transfer_reg(i: u16) -> ARMv4InstructionType {
    let ro = ((i >> 6) & 0x7) as usize;
    let rb = ((i >> 3) & 0x7) as usize;
    let rd = (i & 0x7) as usize;

    let transfer_params = TransferParams{
        base_reg: rb,
        inc: true,
        pre_index: true,
        writeback: false,
    };
    if test_bit(i, 9) {
        let offset = OpData::Register(ro);
        match (i & bits(10, 11)) >> 10 {
            0b00 => ARMv4InstructionType::STRH{transfer_params, data_reg: rd, offset},
            0b01 => ARMv4InstructionType::LDRSB{transfer_params, data_reg: rd, offset},
            0b10 => ARMv4InstructionType::LDRH{transfer_params, data_reg: rd, offset},
            0b11 => ARMv4InstructionType::LDRSH{transfer_params, data_reg: rd, offset},
            _ => unreachable!()
        }
    } else {
        let offset = ShiftOperand::Register(ro);
        match (i & bits(10, 11)) >> 10 {
            0b00 => ARMv4InstructionType::STR{transfer_params, data_reg: rd, offset},
            0b01 => ARMv4InstructionType::STRB{transfer_params, data_reg: rd, offset},
            0b10 => ARMv4InstructionType::LDR{transfer_params, data_reg: rd, offset},
            0b11 => ARMv4InstructionType::LDRB{transfer_params, data_reg: rd, offset},
            _ => unreachable!()
        }
    }
}

/// Decode alu operations
fn decode_thumb_hi_reg_ops(i: u16) -> ARMv5InstructionType {
    let rs = ((i >> 3) & 0xF) as usize;
    let rd = (((i >> 4) & 8) | (i & 7)) as usize;
    match (i & bits(8, 9)) >> 8 {
        0b00 => ARMv4InstructionType::ADD{rd, rn: rd, op2: ALUOperand::Normal(ShiftOperand::Register(rs)), set_flags: false}.into(),
        0b01 => ARMv4InstructionType::CMP{rn: rd, op2: ALUOperand::Normal(ShiftOperand::Register(rs))}.into(),
        0b10 => ARMv4InstructionType::MOV{rd, op2: ALUOperand::Normal(ShiftOperand::Register(rs)), set_flags: false}.into(),
        0b11 => if test_bit(i, 7) {
            ARMv5InstructionType::TBLXR{reg: rs}
        } else {
            ARMv4InstructionType::BX{reg: rs}.into()
        },
        _ => unreachable!(),
    }
}

/// Decode alu operations
fn decode_thumb_alu_ops(i: u16) -> ARMv4InstructionType {
    let rs = ((i >> 3) & 7) as usize;
    let rd = (i & 7) as usize;
    let op2 = ALUOperand::Normal(ShiftOperand::Register(rs));
    match (i & bits(6, 9)) >> 6 {
        0x0 => ARMv4InstructionType::AND{rd, rn: rd, op2, set_flags: true},
        0x1 => ARMv4InstructionType::EOR{rd, rn: rd, op2, set_flags: true},
        0x2 => {
            let op2 = ALUOperand::RegShift{op: RegShiftOperand::LSL, shift_reg: rs, reg: rd};
            ARMv4InstructionType::MOV{rd, op2, set_flags: true}
        },
        0x3 => {
            let op2 = ALUOperand::RegShift{op: RegShiftOperand::LSR, shift_reg: rs, reg: rd};
            ARMv4InstructionType::MOV{rd, op2, set_flags: true}
        },
        0x4 => {
            let op2 = ALUOperand::RegShift{op: RegShiftOperand::ASR, shift_reg: rs, reg: rd};
            ARMv4InstructionType::MOV{rd, op2, set_flags: true}
        },
        0x5 => ARMv4InstructionType::ADC{rd, rn: rd, op2, set_flags: true},
        0x6 => ARMv4InstructionType::SBC{rd, rn: rd, op2, set_flags: true},
        0x7 => {
            let op2 = ALUOperand::RegShift{op: RegShiftOperand::ROR, shift_reg: rs, reg: rd};
            ARMv4InstructionType::MOV{rd, op2, set_flags: true}
        },
        0x8 => ARMv4InstructionType::TST{rn: rd, op2},
        0x9 => ARMv4InstructionType::RSB{rd, rn: rs, op2: ALUOperand::Normal(ShiftOperand::Immediate(0)), set_flags: true},
        0xA => ARMv4InstructionType::CMP{rn: rd, op2},
        0xB => ARMv4InstructionType::CMN{rn: rd, op2},
        0xC => ARMv4InstructionType::ORR{rd, rn: rd, op2, set_flags: true},
        0xD => ARMv4InstructionType::MUL{set_flags: true, rd, rs: rd, rm: rs},
        0xE => ARMv4InstructionType::BIC{rd, rn: rd, op2, set_flags: true},
        0xF => ARMv4InstructionType::MVN{rd, op2, set_flags: true},
        _ => unreachable!()
    }
}

/// Decode transfer of words and bytes with immediate offset
fn decode_thumb_transfer(i: u16) -> ARMv4InstructionType {
    let offset = ((i >> 6) & 0x1F) as u32;
    let rb = ((i >> 3) & 0x7) as usize;
    let rd = (i & 0x7) as usize;
    let transfer_params = TransferParams{
        base_reg: rb,
        inc: true,
        pre_index: true,
        writeback: false,
    };
    match (i & bits(11, 12)) >> 11 {
        0b00 => ARMv4InstructionType::STR{transfer_params, data_reg: rd, offset: ShiftOperand::Immediate(offset << 2)},
        0b01 => ARMv4InstructionType::LDR{transfer_params, data_reg: rd, offset: ShiftOperand::Immediate(offset << 2)},
        0b10 => ARMv4InstructionType::STRB{transfer_params, data_reg: rd, offset: ShiftOperand::Immediate(offset)},
        0b11 => ARMv4InstructionType::LDRB{transfer_params, data_reg: rd, offset: ShiftOperand::Immediate(offset)},
        _ => unreachable!()
    }
}

/// Decode transfer of halfwords and stack-relative transfers
fn decode_thumb_transfer_ext(i: u16) -> ARMv4InstructionType {
    if test_bit(i, 12) {
        let rd = ((i >> 8) & 0x7) as usize;
        let offset = ((i & 0xFF) << 2) as u32;
        let transfer_params = TransferParams{
            base_reg: SP_REG,
            inc: true,
            pre_index: true,
            writeback: false,
        };
        if test_bit(i, 11) {
            ARMv4InstructionType::LDR{transfer_params, data_reg: rd, offset: ShiftOperand::Immediate(offset)}
        } else {
            ARMv4InstructionType::STR{transfer_params, data_reg: rd, offset: ShiftOperand::Immediate(offset)}
        }
    } else {
        let offset = ((i >> 5) & 0x3E) as u32;
        let rb = ((i >> 3) & 0x7) as usize;
        let rd = (i & 0x7) as usize;
        let transfer_params = TransferParams{
            base_reg: rb,
            inc: true,
            pre_index: true,
            writeback: false,
        };
        if test_bit(i, 11) {
            ARMv4InstructionType::LDRH{transfer_params, data_reg: rd, offset: OpData::Immediate(offset)}
        } else {
            ARMv4InstructionType::STRH{transfer_params, data_reg: rd, offset: OpData::Immediate(offset)}
        }
    }
}

/// Decode stack-related instructions
fn decode_thumb_stack(i: u16) -> ARMv5InstructionType {
    if test_bit(i, 12) {
        if test_bit(i, 10) {
            if test_bit(i, 9) {
                ARMv5InstructionType::BKPT{comment: (i & 0xFF) as u32}
            } else {
                decode_push_pop(i).into()
            }
        } else {
            let offset = ((i & 0x7F) << 2) as u32;
            let op2 = if test_bit(i, 7) {
                ALUOperand::Normal(ShiftOperand::Immediate(!offset + 1))
            } else {
                ALUOperand::Normal(ShiftOperand::Immediate(offset))
            };
            ARMv4InstructionType::ADD{rd: SP_REG, rn: SP_REG, op2, set_flags: false}.into()
        }
    } else {
        let rd = ((i >> 8) & 0x7) as usize;
        let offset = ((i & 0xFF) << 2) as u32;
        if test_bit(i, 11) {
            let op2 = ALUOperand::Normal(ShiftOperand::Immediate(offset));
            ARMv4InstructionType::ADD{rd, rn: SP_REG, op2, set_flags: false}.into()
        } else {
            ARMv4InstructionType::TADDPC{rd, op2: offset}.into()
        }
    }
}

/// Decode push and pop for thumb
fn decode_push_pop(i: u16) -> ARMv4InstructionType {
    let mut reg_list = i & 0xFF;
    if test_bit(i, 11) {
        // POP
        let transfer_params = TransferParams{
            base_reg: SP_REG,
            inc: true,
            pre_index: false,
            writeback: true,
        };
        reg_list |= (i & bit(8)) << 7;
        ARMv4InstructionType::LDM{transfer_params, reg_list: reg_list as u32, load_from_user: false}
    } else {
        // PUSH
        let transfer_params = TransferParams{
            base_reg: SP_REG,
            inc: false,
            pre_index: true,
            writeback: true,
        };
        reg_list |= (i & bit(8)) << 6;
        ARMv4InstructionType::STM{transfer_params, reg_list: reg_list as u32, load_from_user: false}
    }
}

/// Decode load/store multiple and conditional branches
fn decode_thumb_other_hi(i: u16) -> ARMv5Instruction {
    if test_bit(i, 12) {
        decode_thumb_cond_branch(i)
    } else {
        ARMv5Instruction{cond: ARMCondition::AL, instr: decode_thumb_transfer_mul(i).into()}
    }
}

/// Decode conditional branches and software interrupt
fn decode_thumb_cond_branch(i: u16) -> ARMv5Instruction {
    let cond_bits = ((i >> 8) & 0xF) as u32;
    if cond_bits == 0xF {
        ARMv5Instruction{cond: ARMCondition::AL, instr: ARMv4InstructionType::SWI{comment: (i & 0xFF) as u32}.into()}
    } else {
        let cond = decode_cond(cond_bits);
        let offset_i = ((i & 0xFF) as u8) as i8;
        let offset = ((offset_i as i32) << 1) as u32;
        ARMv5Instruction{cond, instr: ARMv4InstructionType::TB{offset}.into()}
    }
}

/// Decode LDMIA and STMIA
fn decode_thumb_transfer_mul(i: u16) -> ARMv4InstructionType {
    let rb = ((i >> 8) & 0x7) as usize;
    let reg_list = (i & 0xFF) as u32;
    let transfer_params = TransferParams{
        base_reg: rb,
        inc: true,
        pre_index: false,
        writeback: true,
    };
    if test_bit(i, 11) {
        ARMv4InstructionType::LDM{transfer_params, reg_list, load_from_user: false}
    } else {
        ARMv4InstructionType::STM{transfer_params, reg_list, load_from_user: false}
    }
}

/// Decode branches
fn decode_thumb_branch(i: u16) -> ARMv5InstructionType {
    match (i & bits(11, 12)) >> 11 {
        0b00 => {   // B
            let offset_imm = i & bits(0, 10);
            let offset = ((offset_imm as i32) << 21) >> 20;
            ARMv4InstructionType::TB{offset: offset as u32}.into()
        },
        0b01 => {   // BLX
            let offset = ((i & bits(0, 10)) << 1) as u32;
            ARMv5InstructionType::TBLXHI{offset}
        },
        0b10 => {   // Long branch
            let offset_imm = i & bits(0, 10);
            let offset = ((offset_imm as i32) << 21) >> 9;
            ARMv4InstructionType::TBLLO{offset: offset as u32}.into()
        },
        0b11 => {   // BL
            let offset = ((i & bits(0, 10)) << 1) as u32;
            ARMv4InstructionType::TBLHI{offset}.into()
        },
        _ => unreachable!()
    }
}