// Just-in-time compilation

use std::{
    rc::Rc,
    marker::PhantomData
};

/// A subroutine to execute.
pub enum Subroutine<T> {
    /// This subroutine has run n times before.
    Run(usize),
    /// This subroutine cannot be compiled.
    CannotCompile,
    /// This subroutine has been JIT-compiled.
    Compiled(Rc<JITObject<T>>),
}

impl<T> Clone for Subroutine<T> {
    fn clone(&self) -> Self {
        use Subroutine::*;
        match self {
            Run(n) => Run(*n),
            CannotCompile => CannotCompile,
            Compiled(r) => Compiled(r.clone()),
        }
    }
}

pub struct JITObject<T> {
    routine: dynasmrt::ExecutableBuffer,

    _unused_t: PhantomData<T>,
}

impl<T> JITObject<T> {
    pub fn new(routine: dynasmrt::ExecutableBuffer) -> Self {
        Self {
            routine: routine,

            _unused_t: PhantomData
        }
    }

    #[inline]
    pub fn call(&self, cpu: &mut T) {
        let routine: extern "Rust" fn(ts: &mut T) = unsafe { std::mem::transmute(self.routine.ptr(dynasmrt::AssemblyOffset(0))) };
        routine(cpu);
    }
}

/// When the subroutine has been called this many times, JIT it.
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
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
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