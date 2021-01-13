/// Memory interface for ARM processor.

/// A 32-bit memory interface.
/// Capable of loading and storing bytes (8-bit), halfwords (16-bit), and words (32-bit).
/// All operations return the amount of cycles needed to do the transfer.
pub trait Mem32 {
    type Addr;

    fn load_byte(&mut self, addr: Self::Addr) -> (u8, usize);
    fn store_byte(&mut self, addr: Self::Addr, data: u8) -> usize;

    fn load_halfword(&mut self, addr: Self::Addr) -> (u16, usize);
    fn store_halfword(&mut self, addr: Self::Addr, data: u16) -> usize;

    fn load_word(&mut self, addr: Self::Addr) -> (u32, usize);
    fn store_word(&mut self, addr: Self::Addr, data: u32) -> usize;
}