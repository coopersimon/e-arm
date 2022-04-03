/// Memory interface for ARM processor.

/// Cycle types for memory accesses in ARM instructions.
/// 
/// Non-sequential operations occur with random memory transfers or branches.
/// Sequential operations happen with normal instruction fetches and multi-transfers.
#[derive(Clone, Copy, PartialEq)]
pub enum MemCycleType {
    /// Sequential
    S,
    /// Non-sequential
    N
}

impl MemCycleType {
    #[inline]
    pub fn is_non_seq(self) -> bool {
        self == MemCycleType::N
    }
}

/// A 32-bit clockable memory interface.
/// Capable of loading and storing bytes (8-bit), halfwords (16-bit), and words (32-bit).
/// 
/// The memory interface must also support clocking.
/// 
/// All memory operations return the amount of cycles needed to do the transfer.
/// This value can vary based on the type of memory access.
pub trait Mem32 {
    type Addr;

    /// Fetch a thumb instruction.
    fn fetch_instr_halfword(&mut self, cycle: MemCycleType, addr: Self::Addr) -> (u16, usize) {
        self.load_halfword(cycle, addr)
    }
    /// Fetch an ARM instruction.
    fn fetch_instr_word(&mut self, cycle: MemCycleType, addr: Self::Addr) -> (u32, usize) {
        self.load_word(cycle, addr)
    }

    fn load_byte(&mut self, cycle: MemCycleType, addr: Self::Addr) -> (u8, usize);
    fn store_byte(&mut self, cycle: MemCycleType, addr: Self::Addr, data: u8) -> usize;

    fn load_halfword(&mut self, cycle: MemCycleType, addr: Self::Addr) -> (u16, usize);
    fn store_halfword(&mut self, cycle: MemCycleType, addr: Self::Addr, data: u16) -> usize;

    fn load_word(&mut self, cycle: MemCycleType, addr: Self::Addr) -> (u32, usize);
    fn store_word(&mut self, cycle: MemCycleType, addr: Self::Addr, data: u32) -> usize;

    fn clock(&mut self, cycles: usize) -> Option<super::ExternalException>;
}
