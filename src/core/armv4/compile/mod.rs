
mod validate;

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

pub struct DecodedInstruction {
    /// The instruction itself.
    instruction: ARMv4Instruction,
    /// Number of cycles needed to fetch the instruction.
    fetch_cycles: usize,
    /// If this instruction needs a label emitted before it.
    label: bool,
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
        let mut validator = validate::Validator::new(addr);
        let instructions = validator.decode_and_validate::<M, T>(mem)?;
        // TODO: code-gen for instructions.
        Err(CompilerError::TooShort)
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
