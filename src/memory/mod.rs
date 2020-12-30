/// Memory interface for ARM processor.

use num_traits::{
    One,
    Unsigned
};
use crate::common::{
    make_16, from_16,
    make_32, from_32
};

/// An 8-bit memory interface.
/// Capable of loading and storing bytes.
/// The Addr associated type defines the size of the memory bus.
pub trait Mem {
    type Addr: Copy + Unsigned;

    fn load_byte(&mut self, addr: Self::Addr) -> u8;
    fn store_byte(&mut self, addr: Self::Addr, data: u8);
}

/// A 32-bit memory interface.
/// Capable of loading and storing words and halfwords.
/// The default implementation might not be as efficient as a custom impl.
pub trait Mem32: Mem {
    fn load_halfword(&mut self, addr: Self::Addr) -> u16 {

        let byte_0 = self.load_byte(addr);
        let addr_1 = addr + Self::Addr::one();
        let byte_1 = self.load_byte(addr_1);

        make_16(byte_1, byte_0)
    }

    fn store_halfword(&mut self, addr: Self::Addr, data: u16) {
        
        let bytes = from_16(data);

        self.store_byte(addr, bytes[0]);
        let addr_1 = addr + Self::Addr::one();
        self.store_byte(addr_1, bytes[1]);
    }

    fn load_word(&mut self, addr: Self::Addr) -> u32 {

        let byte_0 = self.load_byte(addr);
        let addr_1 = addr + Self::Addr::one();
        let byte_1 = self.load_byte(addr_1);
        let addr_2 = addr_1 + Self::Addr::one();
        let byte_2 = self.load_byte(addr_2);
        let addr_3 = addr_2 + Self::Addr::one();
        let byte_3 = self.load_byte(addr_3);

        make_32(byte_3, byte_2, byte_1, byte_0)
    }

    fn store_word(&mut self, addr: Self::Addr, data: u32) {

        let bytes = from_32(data);

        self.store_byte(addr, bytes[0]);
        let addr_1 = addr + Self::Addr::one();
        self.store_byte(addr_1, bytes[1]);
        let addr_2 = addr_1 + Self::Addr::one();
        self.store_byte(addr_2, bytes[2]);
        let addr_3 = addr_2 + Self::Addr::one();
        self.store_byte(addr_3, bytes[3]);
    }
}