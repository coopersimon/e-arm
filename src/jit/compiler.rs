
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};
use std::rc::Rc;
use super::*;

/// Possible reasons why subroutine could not be compiled.
pub enum CompilerError {
    TooShort,
    TooLong,
    // ...
}

/// An object to compile ARM code into the host platform's machine code.
pub struct Compiler {

}

impl Compiler {
    pub fn new() -> Self {
        Self {

        }
    }

    /// Try and compile a subroutine.
    pub fn compile<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, addr: u32) -> Result<Rc<JITObject<M, T>>, CompilerError> {
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
