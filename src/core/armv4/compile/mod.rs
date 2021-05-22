
#[allow(dead_code)]
mod test;
mod validate;
mod codegen;

use std::rc::Rc;

use crate::{
    Mem32,
    core::{ARMCore, JITObject, CompilerError, ARMv4Instruction}
};

/// An object to compile ARM code into the host platform's machine code.
pub struct ARMv4Compiler {}

impl ARMv4Compiler {
    pub fn new() -> Self {
        Self {}
    }

    /// Try and compile a subroutine.
    pub fn compile<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, mut addr: u32, mem: &mut M) -> Result<Rc<JITObject<T>>, CompilerError> {
        let mut validator = validate::Validator::new(addr);
        let instructions = validator.decode_and_validate::<M, T>(mem)?;

        let mut assembler = codegen::CodeGeneratorX64::new();
        assembler.prelude();
        for i in instructions {
            assembler.codegen(&i, addr);
            addr += 4;
        }

        Ok(assembler.finish())
    }
}

/// An instruction which is decoded, with meta-data, ready for code generation.
pub struct DecodedInstruction {
    /// The instruction itself.
    pub instruction: ARMv4Instruction,
    /// Number of cycles needed to fetch the instruction.
    pub fetch_cycles: usize,
    /// If this instruction needs a label emitted before it.
    pub label: Option<usize>,
    /// If this instruction branches somewhere.
    pub branch_to: Option<usize>,
    /// If this instruction should be treated as a return.
    pub ret: bool,
}
