// Just-in-time compilation

use std::rc::Rc;
use std::marker::PhantomData;

use super::ARMCore;
use crate::memory::Mem32;

/// A subroutine to execute.
pub enum Subroutine<M: Mem32<Addr = u32>, T: ARMCore<M>> {
    /// This subroutine has run n times before.
    Run(usize),
    /// This subroutine cannot be compiled.
    CannotCompile,
    /// This subroutine has been JIT-compiled.
    Compiled(Rc<JITObject<M, T>>),
    //_Unused(Infallible, PhantomData<M>)
}

impl<M: Mem32<Addr = u32>, T: ARMCore<M>> Clone for Subroutine<M, T> {
    fn clone(&self) -> Self {
        use Subroutine::*;
        match self {
            Run(n) => Run(*n),
            CannotCompile => CannotCompile,
            Compiled(r) => Compiled(r.clone()),
        }
    }
}

pub struct JITObject<M: Mem32<Addr = u32>, T: ARMCore<M>> {
    routine: JITRoutine<T>,

    _unused: PhantomData<M>
}

impl<M: Mem32<Addr = u32>, T: ARMCore<M>> JITObject<M, T> {
    #[inline]
    pub fn call(&self, cpu: &mut T) {
        (self.routine)(cpu)
    }
}

pub type JITRoutine<ARM> = fn(&mut ARM);

/// When the subroutine is running this many times, JIT it.
pub const RUN_THRESHOLD: usize = 2;

// Compiler things:

/// Possible reasons why subroutine could not be compiled.
pub enum CompilerError {
    TooShort,
    TooLong,
    BranchBeforeStart,
    IllegalInstruction
}

/// Location of return address.
pub enum ReturnLocation {
    Reg(usize),
    Mem(u32)
}

impl ReturnLocation {
    pub fn is_in_reg(&self, reg: usize) -> bool {
        match self {
            ReturnLocation::Reg(n) => *n == reg,
            _ => false
        }
    }
}