// Validation of subroutine.
// There are a set of requirements that must be met for a subroutine to be compiled.

use std::collections::{BTreeSet, BTreeMap};

use crate::{
    Mem32, MemCycleType,
    core::{ARMCore, CompilerError, ReturnLocation, constants}
};
use super::{
    DecodedInstruction,
    super::{
        instructions::{ARMv4InstructionType, TransferParams, ALUOperand, ShiftOperand},
        decode::*,
    }
};

/// Minimum length of subroutine in instructions.
const SUBROUTINE_MIN_LEN: usize = 10;
/// Maximum length of subroutine in instructions.
const SUBROUTINE_MAX_LEN: u32 = 256;
/// Maximum amount of cycles between clock calls.
const MAX_CYCLES: usize = 30;

#[derive(PartialEq, Eq, Clone, Debug)]
struct DecodeInfo {
    ret: ReturnLocation,
    stack_offset: i32,
}

pub struct Validator {
    /// The list of instructions.
    instructions:   Vec<DecodedInstruction>,
    /// The address the validator is currently validating.
    current_addr:   u32,
    /// The first address of the subroutine.
    start_addr:     u32,
    /// The last possible address the subroutine can reach.
    end_addr:       u32,
    /// The decode info at the address the validator is validating.
    current_meta:   DecodeInfo,
    /// Number of cycles since the last clock.
    cycles_since_clock: usize,
    /// A list of unresolved branches.
    branches:       BTreeSet<u32>,
    /// Decode info at each address encountered so far.
    instr_meta:     BTreeMap<u32, DecodeInfo>,
}

impl Validator {
    pub fn new(addr: u32) -> Self {
        Self {
            instructions: Vec::with_capacity(SUBROUTINE_MAX_LEN as usize),
            current_addr: addr,
            start_addr: addr,
            end_addr: addr + (SUBROUTINE_MAX_LEN * constants::I_SIZE),
            current_meta: DecodeInfo {
                ret: ReturnLocation::Reg(constants::LINK_REG),
                stack_offset: 0,
            },
            cycles_since_clock: 0,
            branches: BTreeSet::new(),
            instr_meta: BTreeMap::new(),
        }
    }

    pub fn decode_and_validate<M: Mem32<Addr = u32>, T: ARMCore<M>>(mut self, mem: &mut M, thumb: bool) -> Result<Vec<DecodedInstruction>, CompilerError> {
        let i_size = if thumb {constants::T_SIZE} else {constants::I_SIZE};
        let mut labels = BTreeMap::new();
        let mut label_idx = 0;

        // Validate each instruction.
        loop {
            // Check if we have encountered any branch destinations.
            self.branches.remove(&self.current_addr);
            // Decode the next instruction.
            let (decoded, cycles) = if thumb {
                let (i, cycles) = mem.load_halfword(MemCycleType::S, self.current_addr);
                (decode_thumb_v4(i), cycles)
            } else {
                let (i, cycles) = mem.load_word(MemCycleType::S, self.current_addr);
                (decode_arm_v4(i), cycles)
            };
            //println!("Encountered i: {}", decoded);

            let i_cycles = cycles + self.internal_cycles(&decoded.instr);
            let mut instruction = DecodedInstruction {
                instruction:    decoded.clone(),
                cycles:         i_cycles,
                label:          None,
                branch_to:      None,
                ret:            false,
                clock:          false,
            };
            self.cycles_since_clock += i_cycles;
            if self.cycles_since_clock >= MAX_CYCLES {
                self.cycles_since_clock = 0;
                instruction.clock = true;
            }

            // If this is a branch dest, check the meta is the same.
            if let Some(meta_data) = self.instr_meta.get(&self.current_addr) {
                if meta_data.ret != self.current_meta.ret {
                    //println!("Dest: {:?} != {:?}", *meta_data, self.current_meta);
                    return Err(CompilerError::InvalidLocalBranch);
                } else if meta_data.stack_offset != self.current_meta.stack_offset {
                    self.current_meta.stack_offset = meta_data.stack_offset;
                }
            }

            // Take certain actions depending on the instruction...
            match &decoded.instr {
                // Check the instruction is allowed:
                ARMv4InstructionType::LDM{load_from_user: true, ..} |
                ARMv4InstructionType::STM{load_from_user: true, ..} |
                //ARMv4InstructionType::SWI{..} |
                ARMv4InstructionType::UND |

                ARMv4InstructionType::MRC{..} |
                ARMv4InstructionType::MCR{..} |
                ARMv4InstructionType::CDP{..} |
                ARMv4InstructionType::LDC{..} |
                ARMv4InstructionType::STC{..} |
                ARMv4InstructionType::MRS{..} |
                ARMv4InstructionType::MSR{..} => {
                    //println!("{}", decoded);
                    return Err(CompilerError::IllegalInstruction);
                },

                // Take a note of internal branch destinations:
                ARMv4InstructionType::B{offset} |
                ARMv4InstructionType::TB{offset} => {
                    let final_offset = offset.wrapping_add(i_size * 2);
                    let dest = self.validate_branch(final_offset)?;
                    if let Some(existing_label_idx) = labels.get(&dest) {
                        instruction.branch_to = Some(*existing_label_idx);
                    } else {
                        instruction.branch_to = Some(label_idx);
                        labels.insert(dest, label_idx);
                        label_idx += 1;
                    }

                    self.cycles_since_clock = 0;
                    instruction.clock = true;
                },

                // Track the stack & return address:
                ARMv4InstructionType::MOV{rd, op2: ALUOperand::Normal(ShiftOperand::Register(rs)), ..} if self.current_meta.ret.is_in_reg(*rs) => {
                    // Check if we are returning now:
                    if *rd == constants::PC_REG {
                        self.cycles_since_clock = 0;
                        instruction.clock = true;
                        instruction.ret = true;
                        if self.branches.is_empty() {
                            self.instructions.push(instruction);
                            return self.resolve_labels(labels, i_size);
                        }
                    } else {
                        self.current_meta.ret = ReturnLocation::Reg(*rd);
                    }
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
                        self.cycles_since_clock = 0;
                        instruction.clock = true;
                        instruction.ret = true;
                        if self.branches.is_empty() {
                            self.instructions.push(instruction);
                            return self.resolve_labels(labels, i_size);
                        }
                    }
                },
                ARMv4InstructionType::LDR{transfer_params: TransferParams{base_reg: constants::SP_REG, inc, pre_index, writeback}, data_reg, offset} => {
                    let ret_in_pc = self.validate_ldr(*inc, *pre_index, *writeback, *data_reg, offset.clone())?;
                    if ret_in_pc {
                        self.cycles_since_clock = 0;
                        instruction.clock = true;
                        instruction.ret = true;
                        if self.branches.is_empty() {
                            self.instructions.push(instruction);
                            return self.resolve_labels(labels, i_size);
                        }
                    }
                },

                // Check if we are returning now:
                ARMv4InstructionType::BX{reg} => if self.current_meta.ret.is_in_reg(*reg) {
                    self.cycles_since_clock = 0;
                    instruction.clock = true;
                    instruction.ret = true;
                    // We only return if we have covered all the possible branches, and the return value is written to PC.
                    if self.branches.is_empty() {
                        self.instructions.push(instruction);
                        return self.resolve_labels(labels, i_size);
                    }
                } else {
                    // Only return BX instructions are allowed.
                    //println!("{} - return is @ {:?}", decoded, self.current_meta);
                    return Err(CompilerError::IllegalInstruction);
                },

                // Check for specific THUMB instruction: ADD SP, #nn
                ARMv4InstructionType::ADD{rd: constants::SP_REG, rn: constants::SP_REG, op2: ALUOperand::Normal(ShiftOperand::Immediate(i)), set_flags: false} => {
                    self.current_meta.stack_offset += *i as i32;
                },

                // Check for valid ALU ops
                ARMv4InstructionType::MOV{rd: constants::PC_REG, ..} |
                ARMv4InstructionType::MVN{rd: constants::PC_REG, ..} |
                ARMv4InstructionType::AND{rd: constants::PC_REG, ..} |
                ARMv4InstructionType::EOR{rd: constants::PC_REG, ..} |
                ARMv4InstructionType::ORR{rd: constants::PC_REG, ..} |
                ARMv4InstructionType::BIC{rd: constants::PC_REG, ..} |
                ARMv4InstructionType::ADD{rd: constants::PC_REG, ..} |
                ARMv4InstructionType::SUB{rd: constants::PC_REG, ..} |
                ARMv4InstructionType::RSB{rd: constants::PC_REG, ..} |
                ARMv4InstructionType::ADC{rd: constants::PC_REG, ..} |
                ARMv4InstructionType::SBC{rd: constants::PC_REG, ..} |
                ARMv4InstructionType::RSC{rd: constants::PC_REG, ..} => return Err(CompilerError::DynamicPCManipulation),
                _ => {}
            }

            // Check the subroutine isn't too long.
            self.current_addr += i_size;
            if self.current_addr >= self.end_addr {
                return Err(CompilerError::TooLong);
            }

            self.instructions.push(instruction);
        }
    }
}

impl Validator {
    // Resolve label destinations.
    fn resolve_labels(mut self, labels: BTreeMap<u32, usize>, i_size: u32) -> Result<Vec<DecodedInstruction>, CompilerError> {
        if self.instructions.len() < SUBROUTINE_MIN_LEN {
            return Err(CompilerError::TooShort);
        }

        for (label_addr, label_idx) in labels.iter() {
            let i = (label_addr - self.start_addr) / i_size;
            self.instructions[i as usize].label = Some(*label_idx);
        }

        Ok(self.instructions)
    }

    // Return dest address.
    fn validate_branch(&mut self, offset: u32) -> Result<u32, CompilerError> {
        let dest = self.current_addr.wrapping_add(offset);
        if dest < self.start_addr {
            return Err(CompilerError::BranchBeforeStart);
        } else if dest >= self.end_addr {
            return Err(CompilerError::TooLong);
        }

        if dest > self.current_addr {
            self.branches.insert(dest);
        }

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
        let pre = pre_index == inc;
        for reg in (0..16).filter(|reg| crate::common::u32::test_bit(reg_list, *reg)) {
            if pre {
                current_stack_offset += 4;
            }
            if self.current_meta.ret.is_in_reg(reg) {
                self.current_meta.ret = ReturnLocation::Stack(current_stack_offset);
            }
            if !pre {
                current_stack_offset += 4;
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
        let pre = pre_index == inc;
        for reg in (0..16).filter(|reg| crate::common::u32::test_bit(reg_list, *reg)) {
            if pre {
                current_stack_offset += 4;
            }
            if self.current_meta.ret.is_in_stack(current_stack_offset) {
                self.current_meta.ret = ReturnLocation::Reg(reg);
                if reg == constants::PC_REG {
                    write_ret_to_pc = true;
                }
            } else if reg == constants::PC_REG {
                // Not allowed to write arbitrary data to PC.
                return Err(CompilerError::DynamicPCManipulation);
            }
            if !pre {
                current_stack_offset += 4;
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

    // Get the number of internal cycles that an instruction uses.
    fn internal_cycles(&self, instruction: &ARMv4InstructionType) -> usize {
        const MUL_CYCLES: usize = 2;
        match instruction {
            ARMv4InstructionType::MUL{..} => MUL_CYCLES,
            ARMv4InstructionType::MLA{..} => MUL_CYCLES + 1,
            ARMv4InstructionType::UMULL{..} => MUL_CYCLES + 1,
            ARMv4InstructionType::UMLAL{..} => MUL_CYCLES + 2,
            ARMv4InstructionType::SMULL{..} => MUL_CYCLES + 1,
            ARMv4InstructionType::SMLAL{..} => MUL_CYCLES + 2,

            ARMv4InstructionType::SWP{..} |
            ARMv4InstructionType::SWPB{..} |
            ARMv4InstructionType::LDR{..} |
            ARMv4InstructionType::LDRB{..} |
            ARMv4InstructionType::LDRSB{..} |
            ARMv4InstructionType::LDRH{..} |
            ARMv4InstructionType::LDRSH{..} |
            ARMv4InstructionType::LDM{..} => 1,

            ARMv4InstructionType::MOV{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::MVN{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::AND{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::EOR{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::ORR{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::BIC{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::ADD{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::SUB{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::RSB{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::ADC{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::SBC{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::RSC{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::TST{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::TEQ{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::CMP{op2: ALUOperand::RegShift{..}, ..} |
            ARMv4InstructionType::CMN{op2: ALUOperand::RegShift{..}, ..} => 1,

            _ => 0,
        }
    }
}