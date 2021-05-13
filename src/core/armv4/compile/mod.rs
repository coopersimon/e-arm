
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};
use std::rc::Rc;
use std::collections::BTreeSet;

use crate::{
    Mem32, MemCycleType,
    core::{ARMCore, JITObject, JITRoutine, CompilerError, ReturnLocation, constants}
};
use super::{
    instructions::{ARMv4Instruction, ARMv4InstructionType, ALUOperand, ShiftOperand},
    decode::*
};

/// Maximum length of subroutine in instructions.
const SUBROUTINE_MAX_LEN: u32 = 256;

/// An object to compile ARM code into the host platform's machine code.
pub struct ARMv4Compiler {
    /// Decoded instructions that are part of the subroutine.
    instructions: Vec<ARMv4Instruction>,

    // Decoding:
    /// All possible branch destinations.
    branches: BTreeSet<u32>,
    /// Location of return address.
    ret:    ReturnLocation,

    // Compiling:
    /// Branch destinations that need labels emitted before them.
    labels: BTreeSet<u32>,
}

impl ARMv4Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),

            branches: BTreeSet::new(),
            ret:    ReturnLocation::Reg(constants::LINK_REG),

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
        let mut current_addr = addr;
        let end = addr + (SUBROUTINE_MAX_LEN * constants::I_SIZE);
        loop {
            // Check if we have encountered any branch destinations.
            self.branches.remove(&current_addr);
            // Decode the next instruction.
            let (i, cycles) = mem.load_word(MemCycleType::N, current_addr);
            let decoded = decode_arm_v4(i);
            // Take certain actions depending on the instruction...
            match decoded.instr {
                // Check the instruction is allowed:
                // TODO: STM/LDM with user regs
                ARMv4InstructionType::BX{..} |
                ARMv4InstructionType::MRS{..} |
                ARMv4InstructionType::MSR{..} => return Err(CompilerError::IllegalInstruction),

                // Take a note of branch destinations:
                ARMv4InstructionType::B{offset} => {
                    let dest = addr.wrapping_add(offset);
                    if dest < addr {
                        return Err(CompilerError::BranchBeforeStart);
                    } else if dest >= end {
                        return Err(CompilerError::TooLong);
                    }
                    self.branches.insert(dest);
                    self.labels.insert(dest);
                },

                // Track the return address:
                //ARMv4InstructionType::STM{}

                // Check if we are returning now:
                ARMv4InstructionType::MOV{rd: constants::PC_REG, op2: ALUOperand::Normal(ShiftOperand::Register(r)), ..} => {
                    // We only return if we have covered all the possible branches, and the return value is written to PC.
                    if self.ret.is_in_reg(r) && self.branches.is_empty() {
                        return Ok(());
                    }
                },
                _ => {}
            }
            // Check the subroutine isn't too long.
            current_addr += constants::I_SIZE;
            if current_addr >= end {
                return Err(CompilerError::TooLong);
            }

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
