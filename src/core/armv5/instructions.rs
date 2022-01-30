// Instructions

use crate::{
    core::{ARMCondition, ARMv5},
    core::armv4::instructions::{
        ARMv4InstructionType, TransferParams, ShiftOperand, OpData
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
    SMULxy{rd: usize, rs: usize, rm: usize},
    SMULWy{rd: usize, rs: usize, rm: usize},
    SMLAxy{rd: usize, rs: usize, rm: usize, rn: usize},
    SMLAWy{rd: usize, rs: usize, rm: usize, rn: usize},
    SMLALxy{rd_hi: usize, rd_lo: usize, rs: usize, rm: usize},
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
            _ => 0

        }
    }
}
