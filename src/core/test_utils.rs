use crate::memory::*;

/// Construct a word from bytes (high to low).
const fn make_32(bytes: &[u8]) -> u32 {
    ((bytes[3] as u32) << 24) |
    ((bytes[2] as u32) << 16) |
    ((bytes[1] as u32) << 8) |
    (bytes[0] as u32)
}

pub struct TestMem(Vec<u32>);

impl TestMem {
    pub fn new(size: usize) -> Self {
        Self((0..size).map(|i| (i & 0xFF) as u8)
            .collect::<Vec<_>>()
            .chunks_exact(4)
            .map(make_32)
            .collect::<Vec<_>>()
        )
    }
}

impl Mem32 for TestMem {
    type Addr = u32;

    fn load_byte(&mut self, _cycle: MemCycleType, addr: Self::Addr) -> (u8, usize) {
        let idx = (addr >> 2) as usize;
        let data = self.0[idx];
        let shift = (addr & 3) * 8;
        let ret = (data >> shift) as u8;
        (ret, 1)
    }
    fn store_byte(&mut self, _cycle: MemCycleType, addr: Self::Addr, data: u8) -> usize {
        let idx = (addr >> 2) as usize;
        let stored = self.0[idx];
        let shift = (addr & 3) * 8;
        let mask = !(0xFF << shift);
        self.0[idx] = (stored & mask) | ((data as u32) << shift);
        1
    }

    fn load_halfword(&mut self, _cycle: MemCycleType, addr: Self::Addr) -> (u16, usize) {
        let idx = (addr >> 2) as usize;
        let data = self.0[idx];
        let shift = (addr & 2) * 8;
        let ret = (data >> shift) as u16;
        (ret, 1)
    }
    fn store_halfword(&mut self, _cycle: MemCycleType, addr: Self::Addr, data: u16) -> usize {
        let idx = (addr >> 2) as usize;
        let stored = self.0[idx];
        let shift = (addr & 2) * 8;
        let mask = !(0xFFFF << shift);
        self.0[idx] = (stored & mask) | ((data as u32) << shift);
        1
    }

    fn load_word(&mut self, _cycle: MemCycleType, addr: Self::Addr) -> (u32, usize) {
        let idx = (addr >> 2) as usize;
        (self.0[idx], 1)
    }
    fn store_word(&mut self, _cycle: MemCycleType, addr: Self::Addr, data: u32) -> usize {
        let idx = (addr >> 2) as usize;
        self.0[idx] = data;
        1
    }

    fn clock(&mut self, _cycles: usize) -> Option<crate::ExternalException> {
        None
    }
}
