
#[allow(dead_code)]
mod test;
mod validate;
mod codegen;

use std::rc::Rc;

use crate::{
    Mem32,
    core::{ARMCoreJIT, JITObject, CompilerError, ARMv4Instruction, constants}
};

/// An object to compile ARM code into the host platform's machine code.
pub struct ARMv4Compiler {}

impl ARMv4Compiler {
    pub fn new() -> Self {
        Self {}
    }

    /// Try and compile a subroutine.
    pub fn compile_arm<M: Mem32<Addr = u32>, T: ARMCoreJIT<M>>(&mut self, addr: u32, mem: &mut M) -> Result<Rc<JITObject<T>>, CompilerError> {
        let validator = Box::new(validate::Validator::new(addr));
        let instructions = validator.decode_and_validate::<M, T>(mem, false)?;

        let pc_val = addr + (constants::I_SIZE * 2);  // PC is always 2 instructions ahead...
        let mut assembler = codegen::CodeGeneratorX64::new(pc_val, constants::I_SIZE);
        assembler.prelude();
        for i in instructions {
            assembler.codegen(&i);
        }

        Ok(assembler.finish())
    }

    /// Try and compile a THUMB subroutine.
    pub fn compile_thumb<M: Mem32<Addr = u32>, T: ARMCoreJIT<M>>(&mut self, addr: u32, mem: &mut M) -> Result<Rc<JITObject<T>>, CompilerError> {
        let validator = Box::new(validate::Validator::new(addr));
        let instructions = validator.decode_and_validate::<M, T>(mem, true)?;

        let pc_val = addr + (constants::T_SIZE * 2);  // PC is always 2 instructions ahead...
        let mut assembler = codegen::CodeGeneratorX64::new(pc_val, constants::T_SIZE);
        assembler.prelude();
        for i in instructions {
            assembler.codegen(&i);
        }

        Ok(assembler.finish())
    }
}

/// An instruction which is decoded, with meta-data, ready for code generation.
#[derive(Clone)]
pub struct DecodedInstruction {
    /// The instruction itself.
    pub instruction: ARMv4Instruction,
    /// Number of cycles needed to fetch the instruction + internal execution cycles.
    pub cycles: usize,
    /// If this instruction needs a label emitted before it.
    pub label: Option<usize>,
    /// If this instruction branches somewhere.
    pub branch_to: Option<usize>,
    /// If this instruction should be treated as a return.
    pub ret: bool,
    /// If this instruction should emit a clock call afterwards.
    pub clock: bool,
}
