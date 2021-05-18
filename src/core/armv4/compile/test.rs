use std::collections::HashMap;
use crate::{
    Mem32, MemCycleType, ExternalException, ARM7TDMI, ARMCore
};

struct TestMem {
    data: Vec<u32>
}

impl Mem32 for TestMem {
    type Addr = u32;

    fn load_byte(&mut self, _cycle: MemCycleType, _addr: Self::Addr) -> (u8, usize) {
        (0, 0)
    }
    fn store_byte(&mut self, _cycle: MemCycleType, _addr: Self::Addr, _data: u8) -> usize {
        0
    }

    fn load_halfword(&mut self, _cycle: MemCycleType, _addr: Self::Addr) -> (u16, usize) {
        (0, 0)
    }
    fn store_halfword(&mut self, _cycle: MemCycleType, _addr: Self::Addr, _data: u16) -> usize {
        0
    }

    fn load_word(&mut self, _cycle: MemCycleType, addr: Self::Addr) -> (u32, usize) {
        let idx = addr / 4;
        (self.data[idx as usize], 1)
    }
    fn store_word(&mut self, _cycle: MemCycleType, addr: Self::Addr, data: u32) -> usize {
        let idx = addr / 4;
        self.data[idx as usize] = data;
        1
    }

    fn clock(&mut self, _cycles: usize) -> Option<ExternalException> {
        None
    }
}

#[test]
fn test_add_imm() {
    let mut mem = TestMem {
        data: vec![
            0xE3A0_007B,    // MOV R0, #123
            0xE280_10EA,    // ADD R1, R0, #234
            0xE1A0_F00E,    // MOV R15, R14
        ]
    };
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile::<TestMem, ARM7TDMI<TestMem>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            let mut cpu = ARM7TDMI::new(mem, HashMap::new(), None);
            routine.call(&mut cpu);
            assert_eq!(cpu.read_reg(0), 123);
            assert_eq!(cpu.read_reg(1), 357);
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_add_reg() {
    let mut mem = TestMem {
        data: vec![
            0xE280_10EA,    // ADD R1, R0, #234
            0xE1A0_F00E,    // MOV R15, R14
        ]
    };
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile::<TestMem, ARM7TDMI<TestMem>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            let mut cpu = ARM7TDMI::new(mem, HashMap::new(), None);
            cpu.write_reg(0, 345);
            routine.call(&mut cpu);
            assert_eq!(cpu.read_reg(0), 345);
            assert_eq!(cpu.read_reg(1), 579);
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_multi_add() {
    let mut mem = TestMem {
        data: vec![
            0xE080_8001,    // ADD R8, R0, R1
            0xE082_9003,    // ADD R9, R2, R3
            0xE084_A005,    // ADD R10, R4, R5
            0xE086_B007,    // ADD R11, R6, R7
            0xE088_C009,    // ADD R12, R8, R9
            0xE08C_C00A,    // ADD R12, R12, R10
            0xE08C_C00B,    // ADD R12, R12, R11
            0xE1A0_F00E,    // MOV R15, R14
        ]
    };
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile::<TestMem, ARM7TDMI<TestMem>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            let mut cpu = ARM7TDMI::new(mem, HashMap::new(), None);
            cpu.write_reg(0, 0x1000_0000);
            cpu.write_reg(1, 0x1);
            cpu.write_reg(2, 0x10);
            cpu.write_reg(3, 0x11);
            cpu.write_reg(4, 0x1234_5678);
            cpu.write_reg(5, 0x1234);
            cpu.write_reg(6, 0x6666);
            cpu.write_reg(7, 0x7777);

            routine.call(&mut cpu);

            assert_eq!(cpu.read_reg(0), 0x1000_0000);
            assert_eq!(cpu.read_reg(1), 0x1);
            assert_eq!(cpu.read_reg(2), 0x10);
            assert_eq!(cpu.read_reg(3), 0x11);
            assert_eq!(cpu.read_reg(4), 0x1234_5678);
            assert_eq!(cpu.read_reg(5), 0x1234);
            assert_eq!(cpu.read_reg(6), 0x6666);
            assert_eq!(cpu.read_reg(7), 0x7777);

            assert_eq!(cpu.read_reg(8), 0x1000_0001);
            assert_eq!(cpu.read_reg(9), 0x21);
            assert_eq!(cpu.read_reg(10), 0x1234_68AC);
            assert_eq!(cpu.read_reg(11), 0xDDDD);

            assert_eq!(cpu.read_reg(12), 0x2235_46AB);
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}