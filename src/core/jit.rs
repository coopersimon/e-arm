// Just-in-time compilation

use std::rc::Rc;
use std::marker::PhantomData;

use super::ARMCore;
use crate::memory::Mem32;

/// A subroutine to execute.
pub enum Subroutine {
    /// This subroutine has run n times before.
    Run(usize),
    /// This subroutine cannot be compiled.
    CannotCompile,
    /// This subroutine has been JIT-compiled.
    Compiled(Rc<JITObject>),
    //_Unused(Infallible, PhantomData<M>)
}

impl Clone for Subroutine {
    fn clone(&self) -> Self {
        use Subroutine::*;
        match self {
            Run(n) => Run(*n),
            CannotCompile => CannotCompile,
            Compiled(r) => Compiled(r.clone()),
        }
    }
}

pub struct JITObject {
    routine: dynasmrt::ExecutableBuffer,
}

impl JITObject {
    pub fn new(routine: dynasmrt::ExecutableBuffer) -> Self {
        Self {
            routine: routine,
        }
    }

    #[inline]
    pub fn call<M: Mem32<Addr = u32>, T: ARMCore<M>>(&self, cpu: &mut T) {
        let routine: extern "Rust" fn(ts: &mut T) = unsafe { std::mem::transmute(self.routine.ptr(dynasmrt::AssemblyOffset(0))) };
        routine(cpu);
    }
}

/// When the subroutine is running this many times, JIT it.
pub const RUN_THRESHOLD: usize = 2;

// Compiler things:

/// Possible reasons why subroutine could not be compiled.
#[derive(Debug)]
pub enum CompilerError {
    /// Routines must be a certain length to be worth it.
    TooShort,
    /// Routines cannot be too long.
    TooLong,
    /// Branches cannot end up before the entry point of the subroutine.
    BranchBeforeStart,
    /// An instruction which is not allowed to be JITted has been encountered.
    IllegalInstruction,
    /// The PC was changed dynamically.
    DynamicPCManipulation,
    /// Stack pointer was changed non-statically.
    DynamicStackManipulation,
    /// Return address was moved non-statically.
    DynamicReturnAddr,
    /// Branch destination involved non-static stack or return addr location.
    InvalidLocalBranch
}

/// Location of return address.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ReturnLocation {
    Reg(usize),
    Stack(i32)
}

impl ReturnLocation {
    pub fn is_in_reg(&self, reg: usize) -> bool {
        match self {
            ReturnLocation::Reg(n) => *n == reg,
            _ => false
        }
    }
    pub fn is_in_stack(&self, offset: i32) -> bool {
        match self {
            ReturnLocation::Stack(n) => *n == offset,
            _ => false
        }
    }
}