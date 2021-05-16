
#[allow(dead_code)]
mod test;
mod validate;
mod codegen;

use std::rc::Rc;

use crate::{
    Mem32,
    core::{ARMCore, JITObject, CompilerError}
};

/// An object to compile ARM code into the host platform's machine code.
pub struct ARMv4Compiler {}

impl ARMv4Compiler {
    pub fn new() -> Self {
        Self {}
    }

    /// Try and compile a subroutine.
    pub fn compile<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, addr: u32, mem: &mut M) -> Result<Rc<JITObject>, CompilerError> {
        let mut validator = validate::Validator::new(addr);
        let instructions = validator.decode_and_validate::<M, T>(mem)?;

        let mut assembler = codegen::CodeGeneratorX64::new();
        assembler.prelude::<M, T>();
        for i in instructions {
            assembler.codegen::<M, T>(&i);
        }

        Ok(assembler.finish())
    }
}

/*pub unsafe extern "Rust" fn wrap_call_subroutine<M: Mem32<Addr = u32>, T: ARMCore<M>>(ts: *mut T, dest: u32) {
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
}*/
