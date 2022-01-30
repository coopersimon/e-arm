// Instructions

use crate::{
    core::{ARMCondition, ARMv5},
    core::armv4::instructions::{
        ARMv4Instruction, ARMv4InstructionType, TransferParams, ShiftOperand, OpData
    },
    memory::Mem32
};
use std::fmt;

#[derive(Clone)]
pub struct ARMv5Instruction {
    pub cond:   ARMCondition,
    pub instr:  ARMv5InstructionType,
}

impl ARMv5Instruction {
    /// Execute the instruction on the core provided.
    /// 
    /// Returns the number of cycles needed to execute it.
    /// This includes any internal (I) cycles and non-seq loads and stores (N).
    /// It does _not_ include the initial fetch cycles (S) or any pipeline flush stall cycles.
    pub fn execute<M: Mem32<Addr = u32>, A: ARMv5<M>>(self, core: &mut A) -> usize {
        if self.cond.eval(core) {
            self.instr.execute(core)
        } else {
            0
        }
    }
}

impl From<ARMv4InstructionType> for ARMv5InstructionType {
    fn from(i: ARMv4InstructionType) -> Self {
        Self::ARMv4(i)
    }
}

/// Decoded instructions.
/// 
/// Each instruction has a set of parameters.
#[derive(Clone)]
pub enum ARMv5InstructionType {
    ARMv4(ARMv4InstructionType),
    // Co-processor
    MRC2{coproc: usize, coproc_reg: usize, arm_reg: usize, op_reg: usize, op: u32, info: u32},
    MCR2{coproc: usize, coproc_reg: usize, arm_reg: usize, op_reg: usize, op: u32, info: u32},
    CDP2{op: u32, reg_n: usize, reg_d: usize, info: u32, reg_m: usize, coproc: usize},
    LDC2{coproc: usize, coproc_reg: usize, transfer_len: bool, transfer_params: TransferParams, offset: u32},
    STC2{coproc: usize, coproc_reg: usize, transfer_len: bool, transfer_params: TransferParams, offset: u32},
    MRRC{coproc: usize, arm_reg_n: usize, arm_reg_d: usize, op_reg: usize, op: u32},
    MCRR{coproc: usize, arm_reg_n: usize, arm_reg_d: usize, op_reg: usize, op: u32},
    // Branch
    BLXI{offset: u32},
    BLXR{reg: usize},
    // Transfer
    PLD{transfer_params: TransferParams, offset: ShiftOperand},
    LDRD{transfer_params: TransferParams, data_reg: usize, offset: OpData},
    STRD{transfer_params: TransferParams, data_reg: usize, offset: OpData},
    // Q ALU
    QADD{rd: usize, rm: usize, rn: usize},
    QSUB{rd: usize, rm: usize, rn: usize},
    QDADD{rd: usize, rm: usize, rn: usize},
    QDSUB{rd: usize, rm: usize, rn: usize},
    // Signed halfword multiply
    SMULxy{rd: usize, rm: usize, rs: usize, y: bool, x: bool},
    SMULWy{rd: usize, rm: usize, rs: usize, y: bool},
    SMLAxy{rd: usize, rm: usize, rs: usize, rn: usize, y: bool, x: bool},
    SMLAWy{rd: usize, rm: usize, rs: usize, rn: usize, y: bool},
    SMLALxy{rd_hi: usize, rd_lo: usize, rm: usize, rs: usize, y: bool, x: bool},
    // Other special instructions
    BKPT{comment: u32},
    CLZ{rd: usize, rm: usize},
}

impl ARMv5InstructionType {
    /// Execute the instruction on the core provided.
    /// 
    /// Returns the number of cycles needed to execute it.
    /// This includes any internal (I) cycles and non-seq loads and stores (N).
    /// It does _not_ include the initial fetch cycles (S) or any pipeline flush stall cycles.
    pub fn execute<M: Mem32<Addr = u32>, A: ARMv5<M>>(self, core: &mut A) -> usize {
        use ARMv5InstructionType::*;
        match self {
            ARMv4(i) => i.execute(core),
            // Co-processor
            MRC2{coproc, coproc_reg, arm_reg, op_reg, op, info} => core.mrc2(coproc, coproc_reg, arm_reg, op_reg, op, info),
            MCR2{coproc, coproc_reg, arm_reg, op_reg, op, info} => core.mcr2(coproc, coproc_reg, arm_reg, op_reg, op, info),
            CDP2{op, reg_n, reg_d, info, reg_m, coproc} => core.cdp2(op, reg_n, reg_d, info, reg_m, coproc),
            LDC2{coproc, coproc_reg, transfer_len, transfer_params, offset} => core.ldc2(coproc, transfer_len, transfer_params, offset, coproc_reg),
            STC2{coproc, coproc_reg, transfer_len, transfer_params, offset} => core.stc2(coproc, transfer_len, transfer_params, offset, coproc_reg),
            MRRC{coproc, arm_reg_n, arm_reg_d, op_reg, op} => core.mrrc(coproc, arm_reg_n, arm_reg_d, op_reg, op),
            MCRR{coproc, arm_reg_n, arm_reg_d, op_reg, op} => core.mcrr(coproc, arm_reg_n, arm_reg_d, op_reg, op),
            // Branch
            BLXI{offset} => core.blxi(offset),
            BLXR{reg} => core.blxr(reg),
            // Transfer
            PLD{transfer_params, offset} => core.pld(transfer_params, offset),
            LDRD{transfer_params, data_reg, offset} => core.ldrd(transfer_params, data_reg, offset),
            STRD{transfer_params, data_reg, offset} => core.strd(transfer_params, data_reg, offset),
            // Q ALU
            QADD{rd, rm, rn} => core.qadd(rd, rm, rn),
            QSUB{rd, rm, rn} => core.qsub(rd, rm, rn),
            QDADD{rd, rm, rn} => core.qdadd(rd, rm, rn),
            QDSUB{rd, rm, rn} => core.qdsub(rd, rm, rn),
            // Signed halfword multiply
            SMULxy{rd, rm, rs, y, x} => core.smulxy(rd, rm, rs, y, x),
            SMULWy{rd, rm, rs, y} => core.smulwy(rd, rm, rs, y),
            SMLAxy{rd, rm, rs, rn, y, x} => core.smlaxy(rd, rm, rs, rn, y, x),
            SMLAWy{rd, rm, rs, rn, y} => core.smlawy(rd, rm, rs, rn, y),
            SMLALxy{rd_hi, rd_lo, rm, rs, y, x} => core.smlalxy(rd_hi, rd_lo, rm, rs, y, x),
            // Other special instructions
            BKPT{comment} => core.bkpt(comment),
            CLZ{rd, rm} => core.clz(rd, rm),
        }
    }
}

/* Display */

impl fmt::Display for ARMv5Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ARMv5InstructionType::*;
        match &self.instr {
            ARMv4(i) => ARMv4Instruction{cond: self.cond, instr: i.clone()}.fmt(f),
            // Co-processor
            MRC2{coproc, coproc_reg, arm_reg, op_reg, op, info} => write!(f, "MRC2 {},<{}>,R{},C{},C{},<{}>", coproc, op, arm_reg, coproc_reg, op_reg, info),
            MCR2{coproc, coproc_reg, arm_reg, op_reg, op, info} => write!(f, "MCR2 {},<{}>,R{},C{},C{},<{}>", coproc, op, arm_reg, coproc_reg, op_reg, info),
            CDP2{op, reg_n, reg_d, info, reg_m, coproc} => write!(f, "CDP2 {},<{}>,R{},R{},R{},<{}>", coproc, op, reg_d, reg_n, reg_m, info),
            LDC2{coproc, coproc_reg, transfer_len, transfer_params, offset} => write!(f, "LDC2{},{},C{},{}", if *transfer_len {"L"} else {""}, coproc, coproc_reg, transfer_params.si(offset)),
            STC2{coproc, coproc_reg, transfer_len, transfer_params, offset} => write!(f, "STC2{},{},C{},{}", if *transfer_len {"L"} else {""}, coproc, coproc_reg, transfer_params.si(offset)),
            MRRC{coproc, arm_reg_n, arm_reg_d, op_reg, op} => write!(f, "MRRC{} {},<{}>,R{},R{},C{}", self.cond, coproc, op, arm_reg_d, arm_reg_n, op_reg),
            MCRR{coproc, arm_reg_n, arm_reg_d, op_reg, op} => write!(f, "MCRR{} {},<{}>,R{},R{},C{}", self.cond, coproc, op, arm_reg_d, arm_reg_n, op_reg),
            // Branch
            BLXI{offset} => write!(f, "BLX #{:X}", offset),
            BLXR{reg} => write!(f, "BLX{} R{}", self.cond, reg),
            // Transfer
            PLD{transfer_params, offset} => write!(f, "PLD {}", offset),
            LDRD{transfer_params, data_reg, offset} => write!(f, "LDR{}D R{},{}", self.cond, data_reg, transfer_params.so(offset)),
            STRD{transfer_params, data_reg, offset} => write!(f, "STR{}D R{},{}", self.cond, data_reg, transfer_params.so(offset)),
            // Q ALU
            QADD{rd, rm, rn} => write!(f, "QADD R{},R{},R{}", rd, rm, rn),
            QSUB{rd, rm, rn} => write!(f, "QSUB R{},R{},R{}", rd, rm, rn),
            QDADD{rd, rm, rn} => write!(f, "QDADD R{},R{},R{}", rd, rm, rn),
            QDSUB{rd, rm, rn} => write!(f, "QDSUB R{},R{},R{}", rd, rm, rn),
            // Signed halfword multiply
            SMULxy{rd, rm, rs, y, x} => write!(f, "SMUL{}{}{} R{},R{},R{}", if *x {"T"} else {"B"}, if *y {"T"} else {"B"}, self.cond, rd, rm, rs),
            SMULWy{rd, rm, rs, y} => write!(f, "SMULW{}{} R{},R{},R{}", if *y {"T"} else {"B"}, self.cond, rd, rm, rs),
            SMLAxy{rd, rm, rs, rn, y, x} => write!(f, "SMLA{}{}{} R{},R{},R{},R{}", if *x {"T"} else {"B"}, if *y {"T"} else {"B"}, self.cond, rd, rm, rs, rn),
            SMLAWy{rd, rm, rs, rn, y} => write!(f, "SMLAW{}{} R{},R{},R{},R{}", if *y {"T"} else {"B"}, self.cond, rd, rm, rs, rn),
            SMLALxy{rd_hi, rd_lo, rm, rs, y, x} => write!(f, "SMLAL{}{}{} R{},R{},R{},R{}", if *x {"T"} else {"B"}, if *y {"T"} else {"B"}, self.cond, rd_lo, rd_hi, rm, rs),
            // Other special instructions
            BKPT{comment} => write!(f, "BKPT {:X}", comment),
            CLZ{rd, rm} => write!(f, "CLZ{} R{},R{}", self.cond, rd, rm),
        }
    }
}
