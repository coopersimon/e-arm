// Just-in-time compilation

mod compiler;

use std::rc::Rc;
use std::marker::PhantomData;

use crate::core::ARMCore;
use crate::memory::Mem32;

pub use compiler::*;

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
