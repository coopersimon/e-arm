// Validation of subroutine.
// There are a set of requirements that must be met for a subroutine to be compiled.

use std::collections::{BTreeSet, BTreeMap};

use crate::{
    Mem32, MemCycleType,
    core::{ARMCore, CompilerError, ReturnLocation, constants}
};
use super::{
    codegen::DecodedInstruction,
    super::{
        instructions::{ARMv4InstructionType, TransferParams, ALUOperand, ShiftOperand},
        decode::*
    }
};

/// Maximum length of subroutine in instructions.
const SUBROUTINE_MAX_LEN: u32 = 256;

#[derive(PartialEq, Eq, Clone)]
struct DecodeInfo {
    ret: ReturnLocation,
    stack_offset: i32,
}

pub struct Validator {
    /// The address the validator is currently validating.
    current_addr:   u32,
    /// The first address of the subroutine.
    start_addr:     u32,
    /// The last possible address the subroutine can reach.
    end_addr:       u32,
    /// The decode info at the address the validator is validating.
    current_meta:   DecodeInfo,
    /// A list of unresolved branches.
    branches:       BTreeSet<u32>,
    /// Decode info at each address encountered so far.
    instr_meta:     BTreeMap<u32, DecodeInfo>,
}

impl Validator {
    pub fn new(addr: u32) -> Self {
        Self {
            current_addr: addr,
            start_addr: addr,
            end_addr: addr + (SUBROUTINE_MAX_LEN * constants::I_SIZE),
            current_meta: DecodeInfo {
                ret: ReturnLocation::Reg(constants::LINK_REG),
                stack_offset: 0,
            },
            branches: BTreeSet::new(),
            instr_meta: BTreeMap::new(),
        }
    }

    pub fn decode_and_validate<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, mem: &mut M) -> Result<Vec<DecodedInstruction>, CompilerError> {
        let mut instructions = Vec::new();
        let mut labels = BTreeMap::new();
        let mut label_idx = 0;

        // Validate each instruction.
        loop {
            // Check if we have encountered any branch destinations.
            self.branches.remove(&self.current_addr);
            // Decode the next instruction.
            let (i, cycles) = mem.load_word(MemCycleType::N, self.current_addr);
            let decoded = decode_arm_v4(i);
            println!("Encountered i: {}", decoded);
            let mut instruction = DecodedInstruction {
                instruction: decoded.clone(),
                fetch_cycles: cycles,
                label: None,
                branch_to: None,
                ret: false,
            };
            // Take certain actions depending on the instruction...
            match &decoded.instr {
                // Check the instruction is allowed:
                ARMv4InstructionType::LDM{load_from_user: true, ..} |
                ARMv4InstructionType::STM{load_from_user: true, ..} |
                ARMv4InstructionType::MRS{..} |
                ARMv4InstructionType::MSR{..} => return Err(CompilerError::IllegalInstruction),

                // Take a note of internal branch destinations:
                ARMv4InstructionType::B{offset} => {
                    let dest = self.validate_branch(*offset)?;
                    instruction.branch_to = Some(label_idx);
                    labels.insert(dest, label_idx);
                    label_idx += 1;
                },

                // Track the stack & return address:
                ARMv4InstructionType::MOV{rd, op2: ALUOperand::Normal(ShiftOperand::Register(rs)), ..} if self.current_meta.ret.is_in_reg(*rs) => {
                    // Check if we are returning now:
                    if *rd == constants::PC_REG {
                        instruction.ret = true;
                        if self.branches.is_empty() {
                            return self.resolve_labels(instructions, labels);
                        }
                    }
                    self.current_meta.ret = ReturnLocation::Reg(*rd);
                },
                ARMv4InstructionType::STM{transfer_params: TransferParams{base_reg: constants::SP_REG, inc, pre_index, writeback}, reg_list, ..} => {
                    self.validate_stm(*inc, *pre_index, *writeback, *reg_list)?;
                },
                ARMv4InstructionType::STR{transfer_params: TransferParams{base_reg: constants::SP_REG, inc, pre_index, writeback}, data_reg, offset} => {
                    self.validate_str(*inc, *pre_index, *writeback, *data_reg, offset.clone())?;
                },
                ARMv4InstructionType::LDM{transfer_params: TransferParams{base_reg: constants::SP_REG, inc, pre_index, writeback}, reg_list, ..} => {
                    let ret_in_pc = self.validate_ldm(*inc, *pre_index, *writeback, *reg_list)?;
                    if ret_in_pc {
                        instruction.ret = true;
                        if self.branches.is_empty() {
                            return self.resolve_labels(instructions, labels);
                        }
                    }
                },
                ARMv4InstructionType::LDR{transfer_params: TransferParams{base_reg: constants::SP_REG, inc, pre_index, writeback}, data_reg, offset} => {
                    let ret_in_pc = self.validate_ldr(*inc, *pre_index, *writeback, *data_reg, offset.clone())?;
                    if ret_in_pc {
                        instruction.ret = true;
                        if self.branches.is_empty() {
                            return self.resolve_labels(instructions, labels);
                        }
                    }
                },

                // Check if we are returning now:
                ARMv4InstructionType::BX{reg} => if self.current_meta.ret.is_in_reg(*reg) {
                    instruction.ret = true;
                    // We only return if we have covered all the possible branches, and the return value is written to PC.
                    if self.branches.is_empty() {
                        return self.resolve_labels(instructions, labels);
                    }
                } else {
                    // Only return BX instructions are allowed.
                    return Err(CompilerError::IllegalInstruction);
                },
                _ => {}
            }

            // If this is a branch dest, check the meta is the same.
            if let Some(meta_data) = self.instr_meta.get(&self.current_addr) {
                if *meta_data != self.current_meta {
                    return Err(CompilerError::InvalidLocalBranch);
                }
            } else {
                self.instr_meta.insert(self.current_addr, self.current_meta.clone());
            }

            // Check the subroutine isn't too long.
            self.current_addr += constants::I_SIZE;
            if self.current_addr >= self.end_addr {
                return Err(CompilerError::TooLong);
            }

            instructions.push(instruction);
        }
    }
}

impl Validator {
    // Resolve label destinations.
    fn resolve_labels(&self, mut instructions: Vec<DecodedInstruction>, labels: BTreeMap<u32, usize>) -> Result<Vec<DecodedInstruction>, CompilerError> {
        for (label_addr, label_idx) in labels.iter() {
            let i = (label_addr - self.start_addr) / constants::I_SIZE;
            instructions[i as usize].label = Some(*label_idx);
        }

        Ok(instructions)
    }

    // Return dest address.
    fn validate_branch(&mut self, offset: u32) -> Result<u32, CompilerError> {
        let dest = self.current_addr.wrapping_add(offset);
        if dest < self.start_addr {
            return Err(CompilerError::BranchBeforeStart);
        } else if dest >= self.end_addr {
            return Err(CompilerError::TooLong);
        }
        self.branches.insert(dest);

        // Check that meta data is valid:
        if let Some(meta_data) = self.instr_meta.get(&dest) {
            if *meta_data != self.current_meta {
                return Err(CompilerError::InvalidLocalBranch);
            }
        } else {
            self.instr_meta.insert(dest, self.current_meta.clone());
        }
        Ok(dest)
    }

    fn validate_stm(&mut self, inc: bool, pre_index: bool, writeback: bool, reg_list: u32) -> Result<(), CompilerError> {
        let offset = reg_list.count_ones() * 4;
        let final_stack_offset = if inc {self.current_meta.stack_offset + (offset as i32)} else {self.current_meta.stack_offset - (offset as i32)};
        let mut current_stack_offset = if inc {self.current_meta.stack_offset} else {final_stack_offset};
        for reg in (0..16).filter(|reg| crate::common::u32::test_bit(reg_list, *reg)) {
            current_stack_offset += 4;
            if self.current_meta.ret.is_in_reg(reg) {
                self.current_meta.ret = ReturnLocation::Stack(if pre_index {current_stack_offset - 4} else {current_stack_offset});
            }
        }
        if writeback {
            self.current_meta.stack_offset = final_stack_offset;
        }
        Ok(())
    }

    fn validate_str(&mut self, inc: bool, pre_index: bool, writeback: bool, data_reg: usize, offset: ShiftOperand) -> Result<(), CompilerError> {
        match offset {
            ShiftOperand::Immediate(offset) => {
                // Figure out where current_ret should go
                let new_stack_offset = if inc {self.current_meta.stack_offset + (offset as i32)} else {self.current_meta.stack_offset - (offset as i32)};
                if self.current_meta.ret.is_in_reg(data_reg) {
                    self.current_meta.ret = ReturnLocation::Stack(if pre_index {new_stack_offset} else {self.current_meta.stack_offset});
                }
                if writeback {
                    self.current_meta.stack_offset = new_stack_offset;
                }
                Ok(())
            },
            _ => Err(CompilerError::DynamicStackManipulation),
        }
    }

    // Return true if return address is loaded into PC.
    fn validate_ldm(&mut self, inc: bool, pre_index: bool, writeback: bool, reg_list: u32) -> Result<bool, CompilerError> {
        let offset = reg_list.count_ones() * 4;
        let final_stack_offset = if inc {self.current_meta.stack_offset + (offset as i32)} else {self.current_meta.stack_offset - (offset as i32)};
        let mut current_stack_offset = if inc {self.current_meta.stack_offset} else {final_stack_offset};
        let mut write_ret_to_pc = false;
        for reg in (0..16).filter(|reg| crate::common::u32::test_bit(reg_list, *reg)) {
            current_stack_offset += 4;
            if self.current_meta.ret.is_in_stack(if pre_index {current_stack_offset - 4} else {current_stack_offset}) {
                self.current_meta.ret = ReturnLocation::Reg(reg);
                if reg == constants::PC_REG {
                    write_ret_to_pc = true;
                }
            } else if reg == constants::PC_REG {
                // Not allowed to write arbitrary data to PC.
                return Err(CompilerError::DynamicPCManipulation);
            }
        }
        if writeback {
            self.current_meta.stack_offset = final_stack_offset;
        }
        Ok(write_ret_to_pc)
    }

    // Return true if return address is loaded into PC.
    fn validate_ldr(&mut self, inc: bool, pre_index: bool, writeback: bool, data_reg: usize, offset: ShiftOperand) -> Result<bool, CompilerError> {
        match offset {
            ShiftOperand::Immediate(offset) => {
                // Figure out where current_ret should go
                let new_stack_offset = if inc {self.current_meta.stack_offset + (offset as i32)} else {self.current_meta.stack_offset - (offset as i32)};
                if self.current_meta.ret.is_in_stack(if pre_index {new_stack_offset} else {self.current_meta.stack_offset}) {
                    self.current_meta.ret = ReturnLocation::Reg(data_reg);
                } else if data_reg == constants::PC_REG {
                    // Not allowed to write arbitrary data to PC.
                    return Err(CompilerError::DynamicPCManipulation);
                }
                if writeback {
                    self.current_meta.stack_offset = new_stack_offset;
                }
                Ok(data_reg == constants::PC_REG)
            },
            _ => Err(CompilerError::DynamicStackManipulation),
        }
    }
}