// Instructions

use crate::{
    core::{ARMCondition, ARMv4},
    memory::Mem32
};
use std::fmt;

pub struct ARMv4Instruction {
    cond:   ARMCondition,
    instr:  ARMv4InstructionType,
}

impl ARMv4Instruction {
    pub fn new(cond: ARMCondition, instr: ARMv4InstructionType) -> Self {
        Self {
            cond, instr
        }
    }

    /// Execute the instruction on the core provided.
    /// 
    /// Returns the number of cycles needed to execute it.
    /// This includes any internal (I) cycles and non-seq loads and stores (N).
    /// It does _not_ include the initial fetch cycles (S) or any pipeline flush stall cycles.
    pub fn execute<M: Mem32<Addr = u32>, A: ARMv4<M>>(self, core: &mut A) -> usize {
        if self.cond.eval(core) {
            self.instr.execute(core)
        } else {
            0
        }
    }
}

fn reg_list_to_str(reg_list: u32) -> String {
    use crate::common::u32;
    let mut out = String::new();
    let mut prev_reg: Option<usize> = None;
    let mut consecutive = false;
    for reg in 0..16 {
        if u32::test_bit(reg_list, reg) {
            if let Some(prev) = prev_reg {
                if (prev + 1) != reg {
                    if consecutive {
                        out.push_str(&format!("-{}", prev));
                    }
                    out.push_str(&format!(",R{}", reg));
                    consecutive = false;
                } else {
                    consecutive = true;
                }
            } else {
                out.push_str(&format!("R{}", reg));
            }

            prev_reg = Some(reg);
        }
    }
    if let Some(prev) = prev_reg {
        if consecutive {
            out.push_str(&format!("-{}", prev));
        }
    }
    out
}

impl fmt::Display for ARMv4Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ARMv4InstructionType::*;
        match &self.instr {
            SWI{comment} => write!(f, "SWI{} {:X}", self.cond, comment),
            UND => write!(f, "UND"),

            MRC{coproc, coproc_reg, arm_reg, op_reg, op, info} => write!(f, "MRC{} {},<{}>,{},{},{},<{}>", self.cond, coproc, op, arm_reg, coproc_reg, op_reg, info),
            MCR{coproc, coproc_reg, arm_reg, op_reg, op, info} => write!(f, "MCR{} {},<{}>,{},{},{},<{}>", self.cond, coproc, op, arm_reg, coproc_reg, op_reg, info),
            CDP{op, reg_n, reg_d, info, reg_m, coproc} => write!(f, "CDP{} {},<{}>,{},{},{},<{}>", self.cond, coproc, op, reg_d, reg_n, reg_m, info),
            LDC{coproc, coproc_reg, transfer_len, transfer_params, offset} => write!(f, "LDC{}{},{},{},{}", self.cond, if *transfer_len {"L"} else {""}, coproc, coproc_reg, transfer_params.si(offset)),
            STC{coproc, coproc_reg, transfer_len, transfer_params, offset} => write!(f, "STC{}{},{},{},{}", self.cond, if *transfer_len {"L"} else {""}, coproc, coproc_reg, transfer_params.si(offset)),

            B{offset} => write!(f, "B{} #{:X}", self.cond, offset),
            TB{offset} => write!(f, "B{} #{:X}", self.cond, offset),
            BL{offset} => write!(f, "BL{} #{:X}", self.cond, offset),
            TBLLO{offset} => write!(f, "BL{} #{:X}", self.cond, offset),
            TBLHI{offset} => write!(f, "BL{} #{:X}", self.cond, offset),
            BX{reg} => write!(f, "BX{} R{}", self.cond, reg),

            SWP{rn, rd, rm} => write!(f, "SWP{} R{},R{},[R{}]", self.cond, rd, rm, rn),
            SWPB{rn, rd, rm} => write!(f, "SWPB{} R{},R{},[R{}]", self.cond, rd, rm, rn),
            LDR{transfer_params, data_reg, offset} => write!(f, "LDR{} R{},{}", self.cond, data_reg, transfer_params.ss(offset)),
            TLDRPC{data_reg, offset} => write!(f, "LDR R{},[PC,#{:X}]", data_reg, offset),
            STR{transfer_params, data_reg, offset} => write!(f, "STR{} R{},{}", self.cond, data_reg, transfer_params.ss(offset)),
            LDRB{transfer_params, data_reg, offset} => write!(f, "LDR{}B R{},{}", self.cond, data_reg, transfer_params.ss(offset)),
            STRB{transfer_params, data_reg, offset} => write!(f, "STR{}B R{},{}", self.cond, data_reg, transfer_params.ss(offset)),
            LDRH{transfer_params, data_reg, offset} => write!(f, "LDR{}H R{},{}", self.cond, data_reg, transfer_params.so(offset)),
            STRH{transfer_params, data_reg, offset} => write!(f, "STR{}H R{},{}", self.cond, data_reg, transfer_params.so(offset)),
            LDRSB{transfer_params, data_reg, offset} => write!(f, "LDR{}SB R{},{}", self.cond, data_reg, transfer_params.so(offset)),
            LDRSH{transfer_params, data_reg, offset} => write!(f, "LDR{}SH R{},{}", self.cond, data_reg, transfer_params.so(offset)),
            LDM{transfer_params, reg_list, load_from_user} => write!(f,
                "LDM{} R{}{},{{{}}}{}", self.cond, transfer_params.base_reg, if transfer_params.writeback {"!"} else {""}, reg_list_to_str(*reg_list), if *load_from_user {"^"} else {""}
            ),
            STM{transfer_params, reg_list, load_from_user} => write!(f,
                "STM{} R{}{},{{{}}}{}", self.cond, transfer_params.base_reg, if transfer_params.writeback {"!"} else {""}, reg_list_to_str(*reg_list), if *load_from_user {"^"} else {""}
            ),

            AND{rd, rn, op2, set_flags} => write!(f, "AND{}{} R{},R{},{}", if *set_flags {"S"} else {""}, self.cond, rd, rn, op2),
            EOR{rd, rn, op2, set_flags} => write!(f, "EOR{}{} R{},R{},{}", if *set_flags {"S"} else {""}, self.cond, rd, rn, op2),
            ORR{rd, rn, op2, set_flags} => write!(f, "ORR{}{} R{},R{},{}", if *set_flags {"S"} else {""}, self.cond, rd, rn, op2),
            BIC{rd, rn, op2, set_flags} => write!(f, "BIC{}{} R{},R{},{}", if *set_flags {"S"} else {""}, self.cond, rd, rn, op2),
            ADD{rd, rn, op2, set_flags} => write!(f, "ADD{}{} R{},R{},{}", if *set_flags {"S"} else {""}, self.cond, rd, rn, op2),
            TADDPC{rd, op2} => write!(f, "ADD{} R{},PC,{}", self.cond, rd, op2),
            SUB{rd, rn, op2, set_flags} => write!(f, "SUB{}{} R{},R{},{}", if *set_flags {"S"} else {""}, self.cond, rd, rn, op2),
            RSB{rd, rn, op2, set_flags} => write!(f, "RSB{}{} R{},R{},{}", if *set_flags {"S"} else {""}, self.cond, rd, rn, op2),
            ADC{rd, rn, op2, set_flags} => write!(f, "ADC{}{} R{},R{},{}", if *set_flags {"S"} else {""}, self.cond, rd, rn, op2),
            SBC{rd, rn, op2, set_flags} => write!(f, "SBC{}{} R{},R{},{}", if *set_flags {"S"} else {""}, self.cond, rd, rn, op2),
            RSC{rd, rn, op2, set_flags} => write!(f, "RSC{}{} R{},R{},{}", if *set_flags {"S"} else {""}, self.cond, rd, rn, op2),
            TST{rn, op2} => write!(f, "TST{} R{},{}", self.cond, rn, op2),
            TEQ{rn, op2} => write!(f, "TEQ{} R{},{}", self.cond, rn, op2),
            CMP{rn, op2} => write!(f, "CMP{} R{},{}", self.cond, rn, op2),
            CMN{rn, op2} => write!(f, "CMN{} R{},{}", self.cond, rn, op2),
            MOV{rd, op2, set_flags} => write!(f, "MOV{}{} R{},{}", if *set_flags {"S"} else {""}, self.cond, rd, op2),
            MVN{rd, op2, set_flags} => write!(f, "MVN{}{} R{},{}", if *set_flags {"S"} else {""}, self.cond, rd, op2),
            
            MUL{set_flags, rd, rs, rm} => write!(f, "MUL{}{} R{},R{},R{}", if *set_flags {"S"} else {""}, self.cond, rd, rs, rm),
            MLA{set_flags, rd, rn, rs, rm} => write!(f, "MLA{}{} R{},R{},R{},R{}", if *set_flags {"S"} else {""}, self.cond, rd, rs, rm, rn),
            UMULL{set_flags, rd_hi, rd_lo, rs, rm} => write!(f, "UMULL{}{} R{},R{},R{},R{}", if *set_flags {"S"} else {""}, self.cond, rd_lo, rd_hi, rm, rs),
            UMLAL{set_flags, rd_hi, rd_lo, rs, rm} => write!(f, "UMLAL{}{} R{},R{},R{},R{}", if *set_flags {"S"} else {""}, self.cond, rd_lo, rd_hi, rm, rs),
            SMULL{set_flags, rd_hi, rd_lo, rs, rm} => write!(f, "SMULL{}{} R{},R{},R{},R{}", if *set_flags {"S"} else {""}, self.cond, rd_lo, rd_hi, rm, rs),
            SMLAL{set_flags, rd_hi, rd_lo, rs, rm} => write!(f, "SMLAL{}{} R{},R{},R{},R{}", if *set_flags {"S"} else {""}, self.cond, rd_lo, rd_hi, rm, rs),

            MSR{spsr, mask, data} => write!(f, "MSR{} {} {:X},{}", self.cond, if *spsr {"SPSR"} else {"CPSR"}, mask, data),
            MRS{spsr, rd} => write!(f, "MRS{} R{},{}", self.cond, rd, if *spsr {"SPSR"} else {"CPSR"}),
        }
    }
}

/// Common parameters for transfer instructions.
pub struct TransferParams {
    pub base_reg:   usize,
    pub inc:        bool,
    pub pre_index:  bool,
    pub writeback:  bool,
}

// Convert to string.
impl TransferParams {
    fn ss(&self, offset: &ShiftOperand) -> String {
        let is_offset = match offset {
            ShiftOperand::Immediate(0) => false,
            _ => true,
        };
        if is_offset {
            if self.pre_index {
                format!("[R{},{}{}]{}", self.base_reg, if self.inc {""} else {"-"}, offset, if self.writeback {"!"} else {""})
            } else {
                format!("[R{}],{}{}", self.base_reg, if self.inc {""} else {"-"}, offset)
            }
        } else {
            format!("[R{}]", self.base_reg)
        }
    }

    fn so(&self, offset: &OpData) -> String {
        let is_offset = match offset {
            OpData::Immediate(0) => false,
            _ => true,
        };
        if is_offset {
            if self.pre_index {
                format!("[R{},{}{}]{}", self.base_reg, if self.inc {""} else {"-"}, offset, if self.writeback {"!"} else {""})
            } else {
                format!("[R{}],{}{}", self.base_reg, if self.inc {""} else {"-"}, offset)
            }
        } else {
            format!("[R{}]", self.base_reg)
        }
    }

    fn si(&self, offset: &u32) -> String {
        if *offset != 0 {
            if self.pre_index {
                format!("[R{},{}{}]{}", self.base_reg, if self.inc {""} else {"-"}, *offset, if self.writeback {"!"} else {""})
            } else {
                format!("[R{}],{}{}", self.base_reg, if self.inc {""} else {"-"}, *offset)
            }
        } else {
            format!("[R{}]", self.base_reg)
        }
    }
}

/// ALU 2nd operand types.
pub enum ALUOperand {
    Normal(ShiftOperand),
    /// Shift "reg" by the value in "shift_reg".
    RegShift{
        op: RegShiftOperand,
        shift_reg: usize,
        reg: usize
    }
}

impl fmt::Display for ALUOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ALUOperand::*;
        match self {
            Normal(op) => write!(f, "{}", op),
            RegShift{op, shift_reg, reg} => write!(f, "R{},{} R{}", reg, op, shift_reg),
        }
    }
}

/// ALU 2nd operand types with a shift by register value.
/// 
/// These all take an extra cycle.
pub enum RegShiftOperand {
    LSL,
    LSR,
    ASR,
    ROR,
}

impl fmt::Display for RegShiftOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RegShiftOperand::*;
        match self {
            LSL => write!(f, "LSL"),
            LSR => write!(f, "LSR"),
            ASR => write!(f, "ASR"),
            ROR => write!(f, "ROR"),
        }
    }
}

/// Transfer offset and ALU 2nd operand types.
/// 
/// None of these take an extra cycle when used in the ALU.
pub enum ShiftOperand {
    Immediate(u32),
    Register(usize),
    LSL{shift_amount: u32, reg: usize},
    LSR{shift_amount: u32, reg: usize},
    ASR{shift_amount: u32, reg: usize},
    ROR{shift_amount: u32, reg: usize},
    LSR32{reg: usize},
    ASR32{reg: usize},
    RRX{reg: usize},
}

impl fmt::Display for ShiftOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ShiftOperand::*;
        match self {
            Immediate(i) => write!(f, "#{:X}", i),
            Register(r) => write!(f, "R{}", r),
            LSL{shift_amount, reg} => write!(f, "R{}, LSL #{}", reg, shift_amount),
            LSR{shift_amount, reg} => write!(f, "R{}, LSR #{}", reg, shift_amount),
            ASR{shift_amount, reg} => write!(f, "R{}, ASR #{}", reg, shift_amount),
            ROR{shift_amount, reg} => write!(f, "R{}, ROR #{}", reg, shift_amount),
            LSR32{reg} => write!(f, "R{}, LSR #32", reg),
            ASR32{reg} => write!(f, "R{}, ASR #32", reg),
            RRX{reg} => write!(f, "R{}, RRX", reg),
        }
    }
}

/// Simple data types, used for halfword offset and msr.
pub enum OpData {
    Immediate(u32),
    Register(usize)
}

impl fmt::Display for OpData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use OpData::*;
        match self {
            Immediate(i) => write!(f, "#{:X}", i),
            Register(r) => write!(f, "R{}", r),
        }
    }
}

/// Decoded instructions.
/// 
/// Each instruction has a set of parameters.
pub enum ARMv4InstructionType {
    SWI{comment: u32},
    UND,
    // Coproc
    MRC{coproc: usize, coproc_reg: usize, arm_reg: usize, op_reg: usize, op: u32, info: u32},
    MCR{coproc: usize, coproc_reg: usize, arm_reg: usize, op_reg: usize, op: u32, info: u32},
    CDP{op: u32, reg_n: usize, reg_d: usize, info: u32, reg_m: usize, coproc: usize},
    LDC{coproc: usize, coproc_reg: usize, transfer_len: bool, transfer_params: TransferParams, offset: u32},
    STC{coproc: usize, coproc_reg: usize, transfer_len: bool, transfer_params: TransferParams, offset: u32},
    // Branch
    B{offset: u32},
    TB{offset: u32},
    BL{offset: u32},
    TBLLO{offset: u32},
    TBLHI{offset: u32},
    BX{reg: usize},
    // Transfer
    SWP{rn: usize, rd: usize, rm: usize},
    SWPB{rn: usize, rd: usize, rm: usize},
    LDR{transfer_params: TransferParams, data_reg: usize, offset: ShiftOperand},
    /// Thumb PC-relative load
    TLDRPC{data_reg: usize, offset: u32},
    STR{transfer_params: TransferParams, data_reg: usize, offset: ShiftOperand},
    LDRB{transfer_params: TransferParams, data_reg: usize, offset: ShiftOperand},
    STRB{transfer_params: TransferParams, data_reg: usize, offset: ShiftOperand},
    LDRH{transfer_params: TransferParams, data_reg: usize, offset: OpData},
    STRH{transfer_params: TransferParams, data_reg: usize, offset: OpData},
    LDRSB{transfer_params: TransferParams, data_reg: usize, offset: OpData},
    LDRSH{transfer_params: TransferParams, data_reg: usize, offset: OpData},
    LDM{transfer_params: TransferParams, reg_list: u32, load_from_user: bool},
    STM{transfer_params: TransferParams, reg_list: u32, load_from_user: bool},
    // ALU
    AND{rd: usize, rn: usize, op2: ALUOperand, set_flags: bool},
    EOR{rd: usize, rn: usize, op2: ALUOperand, set_flags: bool},
    SUB{rd: usize, rn: usize, op2: ALUOperand, set_flags: bool},
    RSB{rd: usize, rn: usize, op2: ALUOperand, set_flags: bool},
    ADD{rd: usize, rn: usize, op2: ALUOperand, set_flags: bool},
    /// Thumb PC-relative add
    TADDPC{rd: usize, op2: u32},
    ADC{rd: usize, rn: usize, op2: ALUOperand, set_flags: bool},
    SBC{rd: usize, rn: usize, op2: ALUOperand, set_flags: bool},
    RSC{rd: usize, rn: usize, op2: ALUOperand, set_flags: bool},
    ORR{rd: usize, rn: usize, op2: ALUOperand, set_flags: bool},
    BIC{rd: usize, rn: usize, op2: ALUOperand, set_flags: bool},
    TST{rn: usize, op2: ALUOperand},
    TEQ{rn: usize, op2: ALUOperand},
    CMP{rn: usize, op2: ALUOperand},
    CMN{rn: usize, op2: ALUOperand},
    MOV{rd: usize, op2: ALUOperand, set_flags: bool},
    MVN{rd: usize, op2: ALUOperand, set_flags: bool},
    // Multiply
    MUL{set_flags: bool, rd: usize, rs: usize, rm: usize},
    MLA{set_flags: bool, rd: usize, rn: usize, rs: usize, rm: usize},
    UMULL{set_flags: bool, rd_hi: usize, rd_lo: usize, rs: usize, rm: usize},
    UMLAL{set_flags: bool, rd_hi: usize, rd_lo: usize, rs: usize, rm: usize},
    SMULL{set_flags: bool, rd_hi: usize, rd_lo: usize, rs: usize, rm: usize},
    SMLAL{set_flags: bool, rd_hi: usize, rd_lo: usize, rs: usize, rm: usize},
    // Other
    MSR{spsr: bool, mask: u32, data: OpData},
    MRS{spsr: bool, rd: usize},
}

impl ARMv4InstructionType {
    /// Execute the instruction on the core provided.
    /// 
    /// Returns the number of cycles needed to execute it.
    /// This includes any internal (I) cycles and non-seq loads and stores (N).
    /// It does _not_ include the initial fetch cycles (S) or any pipeline flush stall cycles.
    fn execute<M: Mem32<Addr = u32>, A: ARMv4<M>>(self, core: &mut A) -> usize {
        use ARMv4InstructionType::*;
        match self {
            SWI{comment: _} => core.swi(),
            UND => core.undefined(),

            MRC{coproc, coproc_reg, arm_reg, op_reg, op, info} => core.mrc(coproc, coproc_reg, arm_reg, op_reg, op, info),
            MCR{coproc, coproc_reg, arm_reg, op_reg, op, info} => core.mcr(coproc, coproc_reg, arm_reg, op_reg, op, info),
            CDP{op, reg_n, reg_d, info, reg_m, coproc} => core.cdp(op, reg_n, reg_d, info, reg_m, coproc),
            LDC{coproc, coproc_reg, transfer_len, transfer_params, offset} => core.ldc(coproc, transfer_len, transfer_params, offset, coproc_reg),
            STC{coproc, coproc_reg, transfer_len, transfer_params, offset} => core.stc(coproc, transfer_len, transfer_params, offset, coproc_reg),

            B{offset} => core.b(offset),
            TB{offset} => core.tb(offset),
            BL{offset} => core.bl(offset),
            TBLLO{offset} => core.tbl_lo(offset),
            TBLHI{offset} => core.tbl_hi(offset),
            BX{reg} => core.bx(reg),

            SWP{rn, rd, rm} => core.swp(rn, rd, rm),
            SWPB{rn, rd, rm} => core.swpb(rn, rd, rm),
            LDR{transfer_params, data_reg, offset} => core.ldr(transfer_params, data_reg, offset),
            TLDRPC{data_reg, offset} => core.tldrpc(data_reg, offset),
            STR{transfer_params, data_reg, offset} => core.str(transfer_params, data_reg, offset),
            LDRB{transfer_params, data_reg, offset} => core.ldrb(transfer_params, data_reg, offset),
            STRB{transfer_params, data_reg, offset} => core.strb(transfer_params, data_reg, offset),
            LDRH{transfer_params, data_reg, offset} => core.ldrh(transfer_params, data_reg, offset),
            STRH{transfer_params, data_reg, offset} => core.strh(transfer_params, data_reg, offset),
            LDRSB{transfer_params, data_reg, offset} => core.ldrsb(transfer_params, data_reg, offset),
            LDRSH{transfer_params, data_reg, offset} => core.ldrsh(transfer_params, data_reg, offset),
            LDM{transfer_params, reg_list, load_from_user} => core.ldm(transfer_params, reg_list, load_from_user),
            STM{transfer_params, reg_list, load_from_user} => core.stm(transfer_params, reg_list, load_from_user),

            AND{rd, rn, op2, set_flags} => core.and(set_flags, rd, rn, op2),
            EOR{rd, rn, op2, set_flags} => core.eor(set_flags, rd, rn, op2),
            ORR{rd, rn, op2, set_flags} => core.orr(set_flags, rd, rn, op2),
            BIC{rd, rn, op2, set_flags} => core.bic(set_flags, rd, rn, op2),
            ADD{rd, rn, op2, set_flags} => core.add(set_flags, rd, rn, op2),
            TADDPC{rd, op2} => core.taddpc(rd, op2),
            SUB{rd, rn, op2, set_flags} => core.sub(set_flags, rd, rn, op2),
            RSB{rd, rn, op2, set_flags} => core.rsb(set_flags, rd, rn, op2),
            ADC{rd, rn, op2, set_flags} => core.adc(set_flags, rd, rn, op2),
            SBC{rd, rn, op2, set_flags} => core.sbc(set_flags, rd, rn, op2),
            RSC{rd, rn, op2, set_flags} => core.rsc(set_flags, rd, rn, op2),
            TST{rn, op2} => core.tst(rn, op2),
            TEQ{rn, op2} => core.teq(rn, op2),
            CMP{rn, op2} => core.cmp(rn, op2),
            CMN{rn, op2} => core.cmn(rn, op2),
            MOV{rd, op2, set_flags} => core.mov(set_flags, rd, op2),
            MVN{rd, op2, set_flags} => core.mvn(set_flags, rd, op2),
            
            MUL{set_flags, rd, rs, rm} => core.mul(set_flags, rd, rs, rm),
            MLA{set_flags, rd, rn, rs, rm} => core.mla(set_flags, rd, rn, rs, rm),
            UMULL{set_flags, rd_hi, rd_lo, rs, rm} => core.umull(set_flags, rd_hi, rd_lo, rs, rm),
            UMLAL{set_flags, rd_hi, rd_lo, rs, rm} => core.umlal(set_flags, rd_hi, rd_lo, rs, rm),
            SMULL{set_flags, rd_hi, rd_lo, rs, rm} => core.smull(set_flags, rd_hi, rd_lo, rs, rm),
            SMLAL{set_flags, rd_hi, rd_lo, rs, rm} => core.smlal(set_flags, rd_hi, rd_lo, rs, rm),

            MSR{spsr, mask, data} => core.msr(spsr, mask, data),
            MRS{spsr, rd} => core.mrs(spsr, rd),
        }
    }
}
