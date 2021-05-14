
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};
use std::rc::Rc;
use std::collections::{BTreeSet, BTreeMap};

use crate::{
    Mem32, MemCycleType,
    core::{ARMCore, JITObject, JITRoutine, CompilerError, ReturnLocation, constants}
};
use super::{
    instructions::{ARMv4Instruction, ARMv4InstructionType, TransferParams, ALUOperand, ShiftOperand},
    decode::*
};

/// Maximum length of subroutine in instructions.
const SUBROUTINE_MAX_LEN: u32 = 256;

struct DecodedInstruction {
    /// The instruction itself.
    instruction: ARMv4Instruction,
    /// Number of cycles needed to fetch the instruction.
    fetch_cycles: usize,
}

#[derive(PartialEq, Eq, Clone)]
struct DecodeInfo {
    ret: ReturnLocation,
    stack_offset: i32,
}

/// An object to compile ARM code into the host platform's machine code.
pub struct ARMv4Compiler {
    /// Decoded instructions that are part of the subroutine.
    instructions: Vec<DecodedInstruction>,

    // Compiling:
    /// Branch destinations that need labels emitted before them.
    labels: BTreeSet<u32>,
}

impl ARMv4Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),

            labels: BTreeSet::new(),
        }
    }

    /// Try and compile a subroutine.
    pub fn compile<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, addr: u32, mem: &mut M) -> Result<Rc<JITObject<M, T>>, CompilerError> {
        self.decode::<M, T>(addr, mem)?;
        Err(CompilerError::TooShort)
    }
}

impl ARMv4Compiler {
    /// Decode and analyse subroutine.
    /// 
    /// Independent of target architecture.
    fn decode<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, addr: u32, mem: &mut M) -> Result<(), CompilerError> {
        // Some state we want to keep track of during the decoding sequence:
        let mut current_addr = addr;
        let mut branches = BTreeSet::new();
        let mut instr_meta = BTreeMap::new();
        let mut current_meta = DecodeInfo {
            ret: ReturnLocation::Reg(constants::LINK_REG),
            stack_offset: 0,
        };

        let end = addr + (SUBROUTINE_MAX_LEN * constants::I_SIZE);
        loop {
            // Check if we have encountered any branch destinations.
            branches.remove(&current_addr);
            // Decode the next instruction.
            let (i, cycles) = mem.load_word(MemCycleType::N, current_addr);
            let decoded = decode_arm_v4(i);
            // Take certain actions depending on the instruction...
            match &decoded.instr {
                // Check the instruction is allowed:
                ARMv4InstructionType::LDM{load_from_user: true, ..} |
                ARMv4InstructionType::STM{load_from_user: true, ..} |
                ARMv4InstructionType::MRS{..} |
                ARMv4InstructionType::MSR{..} => return Err(CompilerError::IllegalInstruction),

                // Take a note of internal branch destinations:
                ARMv4InstructionType::B{offset} => {
                    let dest = current_addr.wrapping_add(*offset);
                    if dest < addr {
                        return Err(CompilerError::BranchBeforeStart);
                    } else if dest >= end {
                        return Err(CompilerError::TooLong);
                    }
                    branches.insert(dest);
                    self.labels.insert(dest);

                    // Check that meta data is valid:
                    if let Some(meta_data) = instr_meta.get(&dest) {
                        if *meta_data != current_meta {
                            return Err(CompilerError::InvalidLocalBranch);
                        }
                    } else {
                        instr_meta.insert(dest, current_meta.clone());
                    }
                },

                // Track the stack & return address:
                ARMv4InstructionType::MOV{rd, op2: ALUOperand::Normal(ShiftOperand::Register(rs)), ..} if current_meta.ret.is_in_reg(*rs) => {
                    current_meta.ret = ReturnLocation::Reg(*rd);
                },
                ARMv4InstructionType::STM{transfer_params: TransferParams{base_reg: constants::SP_REG, inc, pre_index, writeback}, reg_list, ..} => {
                    let offset = reg_list.count_ones() * 4;
                    let final_stack_offset = if *inc {current_meta.stack_offset + (offset as i32)} else {current_meta.stack_offset - (offset as i32)};
                    let mut current_stack_offset = if *inc {current_meta.stack_offset} else {final_stack_offset};
                    (0..16).filter(|reg| crate::common::u32::test_bit(*reg_list, *reg)).for_each(|reg| {
                        current_stack_offset += 4;
                        if current_meta.ret.is_in_reg(reg) {
                            current_meta.ret = ReturnLocation::Stack(if *pre_index {current_stack_offset - 4} else {current_stack_offset});
                        }
                    });
                    if *writeback {
                        current_meta.stack_offset = final_stack_offset;
                    }
                },
                ARMv4InstructionType::STR{transfer_params: TransferParams{base_reg: constants::SP_REG, inc, pre_index, writeback}, data_reg, offset} => {
                    match *offset {
                        ShiftOperand::Immediate(offset) => {
                            // Figure out where current_ret should go
                            let new_stack_offset = if *inc {current_meta.stack_offset + (offset as i32)} else {current_meta.stack_offset - (offset as i32)};
                            if current_meta.ret.is_in_reg(*data_reg) {
                                current_meta.ret = ReturnLocation::Stack(if *pre_index {new_stack_offset} else {current_meta.stack_offset});
                            }
                            if *writeback {
                                current_meta.stack_offset = new_stack_offset;
                            }
                        },
                        _ => return Err(CompilerError::DynamicStackManipulation),
                    }
                },
                ARMv4InstructionType::LDM{transfer_params: TransferParams{base_reg: constants::SP_REG, inc, pre_index, writeback}, reg_list, ..} => {
                    let offset = reg_list.count_ones() * 4;
                    let final_stack_offset = if *inc {current_meta.stack_offset + (offset as i32)} else {current_meta.stack_offset - (offset as i32)};
                    let mut current_stack_offset = if *inc {current_meta.stack_offset} else {final_stack_offset};
                    (0..16).filter(|reg| crate::common::u32::test_bit(*reg_list, *reg)).for_each(|reg| {
                        current_stack_offset += 4;
                        if current_meta.ret.is_in_stack(if *pre_index {current_stack_offset - 4} else {current_stack_offset}) {
                            current_meta.ret = ReturnLocation::Reg(reg);
                        }
                    });
                    if *writeback {
                        current_meta.stack_offset = final_stack_offset;
                    }
                },
                ARMv4InstructionType::LDR{transfer_params: TransferParams{base_reg: constants::SP_REG, inc, pre_index, writeback}, data_reg, offset} => {
                    match *offset {
                        ShiftOperand::Immediate(offset) => {
                            // Figure out where current_ret should go
                            let new_stack_offset = if *inc {current_meta.stack_offset + (offset as i32)} else {current_meta.stack_offset - (offset as i32)};
                            if current_meta.ret.is_in_stack(if *pre_index {new_stack_offset} else {current_meta.stack_offset}) {
                                current_meta.ret = ReturnLocation::Reg(*data_reg);
                            }
                            if *writeback {
                                current_meta.stack_offset = new_stack_offset;
                            }
                        },
                        _ => return Err(CompilerError::DynamicStackManipulation),
                    }
                },

                // Check if we are returning now:
                ARMv4InstructionType::MOV{rd: constants::PC_REG, op2: ALUOperand::Normal(ShiftOperand::Register(r)), ..} => {
                    // We only return if we have covered all the possible branches, and the return value is written to PC.
                    if current_meta.ret.is_in_reg(*r) && branches.is_empty() {
                        return Ok(());
                    }
                },
                ARMv4InstructionType::BX{reg} => if current_meta.ret.is_in_reg(*reg) {
                    // We only return if we have covered all the possible branches, and the return value is written to PC.
                    if branches.is_empty() {
                        return Ok(());
                    }
                } else {
                    // Only return BX instructions are allowed.
                    return Err(CompilerError::IllegalInstruction);
                },
                _ => {}
            }

            // If this is a branch dest, check the meta is the same.
            if let Some(meta_data) = instr_meta.get(&current_addr) {
                if *meta_data != current_meta {
                    return Err(CompilerError::InvalidLocalBranch);
                }
            } else {
                instr_meta.insert(current_addr, current_meta.clone());
            }

            // Check the subroutine isn't too long.
            current_addr += constants::I_SIZE;
            if current_addr >= end {
                return Err(CompilerError::TooLong);
            }

            self.instructions.push(DecodedInstruction {
                instruction: decoded,
                fetch_cycles: cycles,
            });
        }
    }
}


pub unsafe extern "Rust" fn wrap_call_subroutine<M: Mem32<Addr = u32>, T: ARMCore<M>>(ts: *mut T, dest: u32) {
    ts.as_mut().unwrap().call_subroutine(dest);
}

pub fn compile<M: Mem32<Addr = u32>, T: ARMCore<M>>() -> JITRoutine<T> {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();

    let call_subroutine = wrap_call_subroutine::<M, T> as i64;

    let rax = 0;

    // ts should be in reg RDI
    dynasm!(ops
        ; .arch x64
        ; mov rsi, 100
        ; mov Rq(rax), QWORD call_subroutine
        ; call Rq(rax)
        ; ret
    );

    let buf = ops.finalize().unwrap();

    let test_func: extern "Rust" fn(ts: &mut T) = unsafe { std::mem::transmute(buf.ptr(dynasmrt::AssemblyOffset(0))) };

    test_func
}
