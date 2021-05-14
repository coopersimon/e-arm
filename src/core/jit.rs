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
    pub fn new(routine: JITRoutine<T>,) -> Self {
        Self {
            routine: routine,
            _unused: PhantomData
        }
    }

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