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
fn test_add_reg_2() {
    let mut mem = TestMem {
        data: vec![
            0xE080_1001,    // ADD R1, R0, R1
            0xE1A0_F00E,    // MOV R15, R14
        ]
    };
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile::<TestMem, ARM7TDMI<TestMem>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            let mut cpu = ARM7TDMI::new(mem, HashMap::new(), None);
            cpu.write_reg(0, 234);
            cpu.write_reg(1, 1000);
            routine.call(&mut cpu);
            assert_eq!(cpu.read_reg(0), 234);
            assert_eq!(cpu.read_reg(1), 1234);
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

#[test]
fn test_add_shift() {
    let mut mem = TestMem {
        data: vec![
            0xE080_B501,    // ADD R11, R0, (R1 LSL #10)
            0xE08B_2332,    // ADD R2, R11, (R2 LSR R3)
            0xE1A0_F00E,    // MOV R15, R14
        ]
    };
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile::<TestMem, ARM7TDMI<TestMem>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            let mut cpu = ARM7TDMI::new(mem, HashMap::new(), None);
            cpu.write_reg(0, 0x1234);
            cpu.write_reg(1, 0x11);
            cpu.write_reg(2, 0x9999);
            cpu.write_reg(3, 4);
            cpu.write_reg(4, 0x5678);

            routine.call(&mut cpu);

            assert_eq!(cpu.read_reg(0), 0x1234);
            assert_eq!(cpu.read_reg(1), 0x11);
            assert_eq!(cpu.read_reg(3), 4);
            assert_eq!(cpu.read_reg(4), 0x5678);
            assert_eq!(cpu.read_reg(11), 0x5634);
            assert_eq!(cpu.read_reg(2), 0x5FCD);
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_imm_shifts() {
    let mut mem = TestMem {
        data: vec![
            0xE1A0_5240,    // MOV R5, (R0 ASR #4)
            0xE1A0_6220,    // MOV R6, (R0 LSR #4)
            0xE1A0_7200,    // MOV R7, (R0 LSL #4)
            0xE1A0_8461,    // MOV R8, (R1 ROR #8)
            0xE1A0_9042,    // MOV R9, (R2 ASR #32)
            // TODO: test carry
            0xE1A0_A022,    // MOV R10, (R2 LSR #32)
            // TODO: test carry
            0xE1A0_F00E,    // MOV R15, R14
        ]
    };
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile::<TestMem, ARM7TDMI<TestMem>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            let mut cpu = ARM7TDMI::new(mem, HashMap::new(), None);
            cpu.write_reg(0, 0xFFFF_0000);
            cpu.write_reg(1, 0xA0A0_B5B5);
            cpu.write_reg(2, 0xF000_0000);

            routine.call(&mut cpu);

            assert_eq!(cpu.read_reg(5), 0xFFFF_F000);
            assert_eq!(cpu.read_reg(6), 0x0FFF_F000);
            assert_eq!(cpu.read_reg(7), 0xFFF0_0000);
            assert_eq!(cpu.read_reg(8), 0xB5A0_A0B5);
            assert_eq!(cpu.read_reg(9), 0xFFFF_FFFF);
            assert_eq!(cpu.read_reg(10), 0x0000_0000);
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_reg_shifts() {
    let mut mem = TestMem {
        data: vec![
            0xE1A0_5450,    // MOV R5, (R0 ASR R4)
            0xE1A0_6430,    // MOV R6, (R0 LSR R4)
            0xE1A0_7410,    // MOV R7, (R0 LSL R4)
            0xE3A0_4008,    // MOV R4, #8
            0xE1A0_8471,    // MOV R8, (R1 ROR R4)
            0xE3A0_4020,    // MOV R4, #32
            0xE1A0_9452,    // MOV R9, (R2 ASR R4)
            // TODO: test carry
            0xE1A0_A432,    // MOV R10, (R2 LSR R4)
            // TODO: test carry
            0xE1A0_F00E,    // MOV R15, R14
        ]
    };
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile::<TestMem, ARM7TDMI<TestMem>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            let mut cpu = ARM7TDMI::new(mem, HashMap::new(), None);
            cpu.write_reg(0, 0xFFFF_0000);
            cpu.write_reg(1, 0xA0A0_B5B5);
            cpu.write_reg(2, 0xF000_0000);
            cpu.write_reg(4, 4);

            routine.call(&mut cpu);

            assert_eq!(cpu.read_reg(5), 0xFFFF_F000);
            assert_eq!(cpu.read_reg(6), 0x0FFF_F000);
            assert_eq!(cpu.read_reg(7), 0xFFF0_0000);
            assert_eq!(cpu.read_reg(8), 0xB5A0_A0B5);
            assert_eq!(cpu.read_reg(9), 0xFFFF_FFFF);
            assert_eq!(cpu.read_reg(10), 0x0000_0000);
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_sub_rsb() {
    let mut mem = TestMem {
        data: vec![
            0xE040_9001,    // SUB R9, R0, R1
            0xE269_2FFA,    // RSB R2, R9, #1000
            0xE1A0_F00E,    // MOV R15, R14
        ]
    };
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            let mut cpu = ARM7TDMI::new(mem, HashMap::new(), None);
            cpu.write_reg(0, 10);
            cpu.write_reg(1, 25);

            routine.call(&mut cpu);

            assert_eq!(cpu.read_reg(0), 10);
            assert_eq!(cpu.read_reg(1), 25);
            assert_eq!(cpu.read_reg(9), 0xFFFF_FFF1);
            assert_eq!(cpu.read_reg(2), 1015);
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_logic() {
    let mut mem = TestMem {
        data: vec![
            0xE000_2001,    // AND R2, R0, R1
            0xE020_3001,    // EOR R3, R0, R1
            0xE180_4001,    // ORR R4, R0, R1
            //0xE269_2FFA,    // AND R5, R2, R3
            0xE1A0_F00E,    // MOV R15, R14
        ]
    };
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            let mut cpu = ARM7TDMI::new(mem, HashMap::new(), None);
            cpu.write_reg(0, 0x1515_E0E0);
            cpu.write_reg(1, 0x2424_7777);

            routine.call(&mut cpu);

            assert_eq!(cpu.read_reg(0), 0x1515_E0E0);
            assert_eq!(cpu.read_reg(1), 0x2424_7777);
            assert_eq!(cpu.read_reg(2), 0x0404_6060);
            assert_eq!(cpu.read_reg(3), 0x3131_9797);
            assert_eq!(cpu.read_reg(4), 0x3535_F7F7);
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}
