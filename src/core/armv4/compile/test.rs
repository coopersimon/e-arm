use std::collections::HashMap;
use std::convert::TryInto;
use crate::{
    Mem32, MemCycleType, ExternalException, ARM7TDMI, ARMCore
};

#[allow(unused_macros)]
macro_rules! run_test {
    ( $mem:expr, $routine:expr, $([ $reg:expr, $start:expr, $end:expr ]),* ) => {
        {   // Test interpreter
            let mut cpu = ARM7TDMI::new($mem.clone(), HashMap::new(), None);
            $(
                cpu.write_reg($reg, $start);
            )*
            for _ in 0..($mem.instructions.len()) {
                cpu.step();
            }
            $(
                let got = cpu.read_reg($reg);
                assert_eq!(got, $end, "(sim) R{}: expected 0x{:X}, got 0x{:X}", $reg, $end, got);
            )*
            println!("sim cycles: {}", cpu.ref_mem().cycles);
        }
        {   // Test JIT
            let mut cpu = ARM7TDMI::new($mem.clone(), HashMap::new(), None);
            $(
                cpu.write_reg($reg, $start);
            )*
            $routine.call(&mut cpu);
            $(
                let got = cpu.read_reg($reg);
                assert_eq!(got, $end, "(jit) R{}: expected 0x{:X}, got 0x{:X}", $reg, $end, got);
            )*
            println!("jit cycles: {}", cpu.ref_mem().cycles);
        }
    };
}

struct TestMemBuilder {
    instructions: Vec<u32>,
    data: Vec<u8>,
}

impl TestMemBuilder {
    fn instructions(mut self, i: Vec<u32>) -> Self {
        self.instructions = i;
        self
    }

    fn data(mut self, d: Vec<u8>) -> Self {
        self.data = d;
        self
    }

    fn build(self) -> TestMem {
        TestMem {
            instructions: self.instructions,
            data: self.data,
            cycles: 0
        }
    }
}

#[derive(Clone)]
struct TestMem {
    instructions: Vec<u32>,
    data: Vec<u8>,
    cycles: usize
}

impl TestMem {
    fn new() -> TestMemBuilder {
        TestMemBuilder {
            instructions: Vec::new(),
            data: Vec::new(),
        }
    }
}

impl Mem32 for TestMem {
    type Addr = u32;

    fn load_byte(&mut self, _cycle: MemCycleType, addr: Self::Addr) -> (u8, usize) {
        if addr >= 0x1000_0000 {
            let idx = (addr - 0x1000_0000) as usize;
            (self.data[idx], 1)
        } else {
            panic!("Cannot load byte from instruction memory");
        }
    }
    fn store_byte(&mut self, _cycle: MemCycleType, addr: Self::Addr, data: u8) -> usize {
        if addr >= 0x1000_0000 {
            let idx = (addr - 0x1000_0000) as usize;
            self.data[idx] = data;
            1
        } else {
            panic!("Cannot store to instruction memory");
        }
    }

    fn load_halfword(&mut self, _cycle: MemCycleType, addr: Self::Addr) -> (u16, usize) {
        if addr >= 0x1000_0000 {
            let idx = (addr - 0x1000_0000) as usize;
            let data = (self.data[idx..(idx+2)]).try_into().unwrap();
            (u16::from_le_bytes(data), 1)
        } else {
            panic!("unimplemented");
        }
    }
    fn store_halfword(&mut self, _cycle: MemCycleType, addr: Self::Addr, data: u16) -> usize {
        if addr >= 0x1000_0000 {
            let idx = (addr - 0x1000_0000) as usize;
            for (dest, byte) in self.data[idx..(idx+2)].iter_mut().zip(&data.to_le_bytes()) {
                *dest = *byte;
            }
            1
        } else {
            panic!("Cannot store to instruction memory");
        }
    }

    fn load_word(&mut self, _cycle: MemCycleType, addr: Self::Addr) -> (u32, usize) {
        if addr >= 0x1000_0000 {
            let idx = (addr - 0x1000_0000) as usize;
            let data = (self.data[idx..(idx+4)]).try_into().unwrap();
            (u32::from_le_bytes(data), 1)
        } else {
            let idx = addr / 4;
            (self.instructions[idx as usize], 1)
        }
    }
    fn store_word(&mut self, _cycle: MemCycleType, addr: Self::Addr, data: u32) -> usize {
        if addr >= 0x1000_0000 {
            let idx = (addr - 0x1000_0000) as usize;
            for (dest, byte) in self.data[idx..(idx+4)].iter_mut().zip(&data.to_le_bytes()) {
                *dest = *byte;
            }
            1
        } else {
            panic!("Cannot store to instruction memory");
        }
    }

    fn clock(&mut self, cycles: usize) -> Option<ExternalException> {
        self.cycles += cycles;
        None
    }
}

#[test]
fn test_add_imm() {
    let mut mem = TestMem::new().instructions(vec![
        0xE3A0_007B,    // MOV R0, #123
        0xE280_10EA,    // ADD R1, R0, #234
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine, [0, 0, 123], [1, 0, 357]);
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_add_reg_with_imm() {
    let mut mem = TestMem::new().instructions(vec![
        0xE280_10EA,    // ADD R1, R0, #234
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine, [0, 345, 345], [1, 0, 579]);
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_add_reg_with_reg() {
    let mut mem = TestMem::new().instructions(vec![
        0xE080_1001,    // ADD R1, R0, R1
        0xE082_2003,    // ADD R2, R2, R3
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 234, 234],
                [1, 1000, 1234],
                [2, 555, 556],
                [3, 1, 1]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_multi_add() {
    let mut mem = TestMem::new().instructions(vec![
        0xE080_8001,    // ADD R8, R0, R1
        0xE082_9003,    // ADD R9, R2, R3
        0xE084_A005,    // ADD R10, R4, R5
        0xE086_B007,    // ADD R11, R6, R7
        0xE088_C009,    // ADD R12, R8, R9
        0xE08C_C00A,    // ADD R12, R12, R10
        0xE08C_C00B,    // ADD R12, R12, R11
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 0x1000_0000u32, 0x1000_0000u32],
                [1, 0x1, 0x1],
                [2, 0x10, 0x10],
                [3, 0x11, 0x11],
                [4, 0x1234_5678, 0x1234_5678],
                [5, 0x1234, 0x1234],
                [6, 0x6666, 0x6666],
                [7, 0x7777, 0x7777],
                [8, 0, 0x1000_0001],
                [9, 0, 0x21],
                [10, 0, 0x1234_68AC],
                [11, 0, 0xDDDD],
                [12, 0, 0x2235_46AB]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_add_shift() {
    let mut mem = TestMem::new().instructions(vec![
        0xE080_B501,    // ADD R11, R0, (R1 LSL #10)
        0xE08B_2332,    // ADD R2, R11, (R2 LSR R3)
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 0x1234, 0x1234],
                [1, 0x11, 0x11],
                [2, 0x9999, 0x5FCD],
                [3, 0x4, 0x4],
                [4, 0x5678, 0x5678],
                [11, 0, 0x5634]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_imm_shifts() {
    let mut mem = TestMem::new().instructions(vec![
        0xE1B0_5240,    // MOVS R5, (R0 ASR #4)
        0x228B_B001,    // ADDCS R11, R11, #1
        0xE1B0_6220,    // MOVS R6, (R0 LSR #4)
        0x228B_B002,    // ADDCS R11, R11, #2
        0xE1B0_7200,    // MOVS R7, (R0 LSL #4)
        0x228B_B004,    // ADDCS R11, R11, #4
        0xE1B0_8461,    // MOVS R8, (R1 ROR #8)
        0x228B_B008,    // ADDCS R11, R11, #8
        0xE1B0_9042,    // MOVS R9, (R2 ASR #32)
        0x228B_B010,    // ADDCS R11, R11, #16
        0xE1B0_A022,    // MOVS R10, (R2 LSR #32)
        0x228B_B020,    // ADDCS R11, R11, #32
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 0xFFFF_0000, 0xFFFF_0000u32],
                [1, 0xA0A0_B5B5, 0xA0A0_B5B5u32],
                [2, 0xF000_0000, 0xF000_0000u32],
                [5, 0, 0xFFFF_F000u32],
                [6, 0, 0x0FFF_F000u32],
                [7, 0, 0xFFF0_0000u32],
                [8, 0, 0xB5A0_A0B5u32],
                [9, 0, 0xFFFF_FFFFu32],
                [10, 0, 0x0000_0000],
                [11, 0, 0x0000_003C]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_reg_shifts() {
    let mut mem = TestMem::new().instructions(vec![
        0xE1A0_5450,    // MOV R5, (R0 ASR R4)
        0x228B_B001,    // ADDCS R11, R11, #1
        0xE1A0_6430,    // MOV R6, (R0 LSR R4)
        0x228B_B002,    // ADDCS R11, R11, #2
        0xE1A0_7410,    // MOV R7, (R0 LSL R4)
        0x228B_B004,    // ADDCS R11, R11, #4
        0xE3A0_4008,    // MOV R4, #8
        0xE1A0_8471,    // MOV R8, (R1 ROR R4)
        0x228B_B008,    // ADDCS R11, R11, #8
        //0xE3A0_4020,    // MOV R4, #32
        //0xE1A0_9452,    // MOV R9, (R2 ASR R4)
        //0x228B_B010,    // ADDCS R11, R11, #16
        //0xE1A0_A432,    // MOV R10, (R2 LSR R4)
        //0x228B_B020,    // ADDCS R11, R11, #32
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 0xFFFF_0000, 0xFFFF_0000u32],
                [1, 0xA0A0_B5B5, 0xA0A0_B5B5u32],
                [2, 0xF000_0000, 0xF000_0000u32],
                [4, 4, 8],
                [5, 0, 0xFFFF_F000u32],
                [6, 0, 0x0FFF_F000u32],
                [7, 0, 0xFFF0_0000u32],
                [8, 0, 0xB5A0_A0B5u32],
                //[9, 0, 0xFFFF_FFFFu32],
                //[10, 0, 0x0000_0000],
                [11, 0, 0x0000_0000]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_shift_carry_with_logic() {
    let mut mem = TestMem::new().instructions(vec![
        0xE010_5200,    // ANDS R5, R0, (R0 LSL #4)
        0x228A_A00A,    // ADDCS R10, R10, #10
        0x428B_B00B,    // ADDMI R11, R11, #11
        0xE036_6136,    // EORS R6, R6, (R6 LSR R1)
        0x228C_C00C,    // ADDCS R12, R12, #12
        0x428D_D00D,    // ADDMI R13, R13, #13
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 0xFFFF_0000, 0xFFFF_0000u32],
                [1, 3, 3],
                [5, 0, 0xFFF0_0000u32],
                // 1C1C_8787 ^ 0383_90F0
                [6, 0x1C1C_8787, 0x1F9F_1777],
                [10, 0, 10],
                [11, 0, 11],
                [12, 0, 12],
                [13, 0, 0]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_shift_carry_with_add() {
    let mut mem = TestMem::new().instructions(vec![
        0xE091_0202,    // ADDS R0, R1, R2, LSL #4
        0x228A_A00A,    // ADDCS R10, R10, #10
        0x628B_B00B,    // ADDVS R11, R11, #11
        0xE0B1_3222,    // ADCS R3, R1, R2, LSR #4
        0x228C_C00C,    // ADDCS R12, R12, #12
        0x428D_D00D,    // ADDMI R13, R13, #13
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 0, 0x8ACF_1348u32],
                [1, 0x1234_5678, 0x1234_5678],
                [2, 0x1789_ABCD, 0x1789_ABCD],
                [3, 0, 0x13AC_F134],
                [10, 0, 0],
                [11, 0, 11],
                [12, 0, 0],
                [13, 0, 0]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_sub_rsb() {
    let mut mem = TestMem::new().instructions(vec![
        0xE040_9001,    // SUB R9, R0, R1
        0xE269_2FFA,    // RSB R2, R9, #1000
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 10, 10],
                [1, 25, 25],
                [9, 0, 0xFFFF_FFF1u32],
                [2, 0, 1015]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_logic() {
    let mut mem = TestMem::new().instructions(vec![
        0xE000_2001,    // AND R2, R0, R1
        0xE020_3001,    // EOR R3, R0, R1
        0xE180_4001,    // ORR R4, R0, R1
        //0xE269_2FFA,    // AND R5, R2, R3
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 0x1515_E0E0, 0x1515_E0E0],
                [1, 0x2424_7777, 0x2424_7777],
                [2, 0, 0x0404_6060],
                [3, 0, 0x3131_9797],
                [4, 0, 0x3535_F7F7]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_cond_eq() {
    let mut mem = TestMem::new().instructions(vec![
        0xE3B0_0000,    // MOVS R0, #0
        0x03B0_1001,    // MOVSEQ R1, #1
        0x03A0_2002,    // MOVEQ R2, #2
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            // TODO: test with multiple args
            run_test!(mem, routine,
                [0, 0, 0],
                [1, 0, 1],
                [2, 100, 100]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_cond_carry() {
    let mut mem = TestMem::new().instructions(vec![
        0xE090_2001,    // ADDS R2, R0, R1
        0x23A0_A00A,    // MOVCS R10, #10
        0x33A0_B00B,    // MOVCC R11, #11
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            // TODO: test with multiple args
            run_test!(mem, routine,
                [0, 0xFFFF_FFFFu32, 0xFFFF_FFFFu32],
                [1, 2, 2],
                [2, 0, 1],
                [10, 0, 10],
                [11, 0, 0]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_long_add() {
    let mut mem = TestMem::new().instructions(vec![
        0xE090_8004,    // ADDS R8, R0, R4
        0xE0A1_9005,    // ADC R9, R1, R5
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 0x8888_8888u32, 0x8888_8888u32],
                [1, 0x2, 0x2],
                [4, 0x9999_9999u32, 0x9999_9999u32],
                [5, 0x1, 0x1],
                [8, 0, 0x2222_2221],
                [9, 0, 0x4]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_long_sub() {
    let mut mem = TestMem::new().instructions(vec![
        0xE050_8004,    // SUBS R8, R0, R4
        0x23A0_A001,    // MOVCS R10, #1
        0x33A0_B001,    // MOVCC R11, #1
        0xE0D1_9005,    // SBCS R9, R1, R5
        0x23A0_C001,    // MOVCS R12, #1
        0x33A0_D001,    // MOVCC R13, #1
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 0x8888_8888u32, 0x8888_8888u32],
                [1, 0x2, 0x2],
                [4, 0x9999_9999u32, 0x9999_9999u32],
                [5, 0x1, 0x1],
                [8, 0, 0xEEEE_EEEFu32],
                [9, 0, 0x0],
                [10, 0, 0x0],
                [11, 0, 0x1],
                [12, 0, 0x1],
                [13, 0, 0x0]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_bic() {
    let mut mem = TestMem::new().instructions(vec![
        0xE3C0_30F0,    // BIC R3, R0, #F0
        0xE1C3_9008,    // BIC R9, R3, R8
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 0xE1E1, 0xE1E1],
                [8, 0x3333, 0x3333],
                [3, 0, 0xE101],
                [9, 0, 0xC000]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_mvn() {
    let mut mem = TestMem::new().instructions(vec![
        0xE1E0_5000,    // MVN R5, R0
        0xE3E0_6CF1,    // MVN R6, #F100
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 0x1E1E_2525, 0x1E1E_2525],
                [5, 0, 0xE1E1_DADAu32],
                [6, 0, 0xFFFF_0EFFu32]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_tst() {
    let mut mem = TestMem::new().instructions(vec![
        0xE110_0001,    // TST R0, R1
        0x43A0_A00A,    // MOVMI R10, #10
        0x53A0_B00B,    // MOVPL R11, #11
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            // TODO: test with multiple args
            run_test!(mem, routine,
                [0, 0x8000_0008u32, 0x8000_0008u32],
                [1, 0xFFFF_0000u32, 0xFFFF_0000u32],
                [10, 0, 10],
                [11, 0, 0]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_teq() {
    let mut mem = TestMem::new().instructions(vec![
        0xE130_0001,    // TEQ R0, R1
        0x43A0_A00A,    // MOVMI R10, #10
        0x53A0_B00B,    // MOVPL R11, #11
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            // TODO: test with multiple args
            run_test!(mem, routine,
                [0, 0x8000_0008u32, 0x8000_0008u32],
                [1, 0xFFFF_0000u32, 0xFFFF_0000u32],
                [10, 0, 0],
                [11, 0, 11]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_cmp() {
    let mut mem = TestMem::new().instructions(vec![
        0xE150_0001,    // CMP R0, R1
        0xD3A0_A00A,    // MOVLE R10, #10
        0xC3A0_B00B,    // MOVGT R11, #11
        0x23A0_C00C,    // MOVCS R12, #12
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 5, 5],
                [1, 4, 4],
                [10, 0, 0],
                [11, 0, 11],
                [12, 0, 12]
            );
            run_test!(mem, routine,
                [0, 4, 4],
                [1, 4, 4],
                [10, 0, 10],
                [11, 0, 0],
                [12, 0, 12]
            );
            run_test!(mem, routine,
                [0, 0x1000_0000, 0x1000_0000],
                [1, 0x5000_0000, 0x5000_0000],
                [10, 0, 10],
                [11, 0, 0],
                [12, 0, 0]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_cmn() {
    let mut mem = TestMem::new().instructions(vec![
        0xE170_0001,    // CMN R0, R1
        0x23A0_A00A,    // MOVCS R10, #10
        0xA3A0_B00B,    // MOVGE R11, #11
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 5, 5],
                [1, 4, 4],
                [10, 0, 0],
                [11, 0, 11]
            );
            run_test!(mem, routine,
                [0, 4, 4],
                [1, 4, 4],
                [10, 0, 0],
                [11, 0, 11]
            );
            run_test!(mem, routine,
                [0, 0xF000_0000u32, 0xF000_0000u32],
                [1, 0x1000_0000, 0x1000_0000],
                [10, 0, 10],
                [11, 0, 11]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_internal_branch() {
    let mut mem = TestMem::new().instructions(vec![
        0xE1B0_1000,    // MOVS R1, R0
        0x4A00_0001,    // BMI #4
        0xE3A0_2002,    // MOV R2, #2
        0xE1A0_F00E,    // MOV R15, R14
        0xE3A0_3003,    // MOV R3, #3
        0xE1A0_F00E,    // MOV R15, R14
        0x0,
        0x0
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine,
                [0, 0xFFFF_FFFF, 0xFFFF_FFFFu32],
                [1, 0, 0xFFFF_FFFFu32],
                [2, 0, 0],
                [3, 0, 3]
            );
            run_test!(mem, routine,
                [0, 1, 1],
                [1, 0, 1],
                [2, 0, 2],
                [3, 0, 0]
            );
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_multiply() {
    let mut mem = TestMem::new().instructions(vec![
        0xE004_0091,    // MUL R4, R0, R1
        0xE005_0498,    // MUL R5, R4, R8
        0xE026_5293,    // MLA R6, R2, R3, +R5
        0xE1A0_F00E,    // MOV R15, R14
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(0, 0x10);
                cpu.write_reg(1, 0x1F);
                cpu.write_reg(2, 0x33);
                cpu.write_reg(3, 0xFFFF_FFFF);
                cpu.write_reg(8, 0x22);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(4), 0x1F0);
                assert_eq!(cpu.read_reg(5), 0x41E0);
                assert_eq!(cpu.read_reg(6), 0x41AD);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_long_unsigned_multiply() {
    let mut mem = TestMem::new().instructions(vec![
        0xE086_7091,    // UMULL R6, R7, R0, R1
        0xE08A_B293,    // UMULL R10, R11, R2, R3
        0xE0AA_B495,    // UMLAL R10, R11, R4, R5
        0xE1A0_F00E,    // MOV R15, R14
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(0, 0x1234_5678);
                cpu.write_reg(1, 0x2323_4545);
                cpu.write_reg(2, 0x33);
                cpu.write_reg(3, 0xFFFF_FFFF);
                cpu.write_reg(4, 0x555);
                cpu.write_reg(5, 0x666);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x1234_5678);
                assert_eq!(cpu.read_reg(1), 0x2323_4545);
                assert_eq!(cpu.read_reg(2), 0x33);
                assert_eq!(cpu.read_reg(3), 0xFFFF_FFFF);
                assert_eq!(cpu.read_reg(4), 0x555);
                assert_eq!(cpu.read_reg(5), 0x666);

                assert_eq!(cpu.read_reg(6), 0x27F_A9E7);
                assert_eq!(cpu.read_reg(7), 0x3DD1_A658);
                assert_eq!(cpu.read_reg(10), 0x33);
                assert_eq!(cpu.read_reg(11), 0x0022_1DAB);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_long_signed_multiply() {
    let mut mem = TestMem::new().instructions(vec![
        0xE0C6_7091,    // SMULL R6, R7, R0, R1
        0xE0CA_B293,    // SMULL R10, R11, R2, R3
        0xE0EA_B495,    // SMLAL R10, R11, R4, R5
        0xE1A0_F00E,    // MOV R15, R14
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(0, 0x1234_5678);
                cpu.write_reg(1, 0x2323_4545);
                cpu.write_reg(2, 0x33);
                cpu.write_reg(3, 0xFFFF_FFFF);
                cpu.write_reg(4, 0x555);
                cpu.write_reg(5, 0x666);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x1234_5678);
                assert_eq!(cpu.read_reg(1), 0x2323_4545);
                assert_eq!(cpu.read_reg(2), 0x33);
                assert_eq!(cpu.read_reg(3), 0xFFFF_FFFF);
                assert_eq!(cpu.read_reg(4), 0x555);
                assert_eq!(cpu.read_reg(5), 0x666);
                
                assert_eq!(cpu.read_reg(6), 0x27F_A9E7);
                assert_eq!(cpu.read_reg(7), 0x3DD1_A658);
                assert_eq!(cpu.read_reg(10), 0);
                assert_eq!(cpu.read_reg(11), 0x0022_1DAB);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_load_word_imm_offset() {
    let mut mem = TestMem::new().instructions(vec![
        0xE591_0004,    // LDR R0, [R1, +#4]
        0xE413_2004,    // LDR R2, [R3], -#4
        0xE5B5_4004,    // LDR R4, [R5, +#4]!
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0x82, 0x83, 0x84, 0x85,
        0x78, 0x56, 0x34, 0x12
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(1, 0x1000_0000);
                cpu.write_reg(3, 0x1000_0000);
                cpu.write_reg(5, 0x1000_0000);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x1234_5678);
                assert_eq!(cpu.read_reg(1), 0x1000_0000);
                assert_eq!(cpu.read_reg(2), 0x8584_8382);
                assert_eq!(cpu.read_reg(3), 0x0FFF_FFFC);
                assert_eq!(cpu.read_reg(4), 0x1234_5678);
                assert_eq!(cpu.read_reg(5), 0x1000_0004);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_load_word_reg_offset() {
    let mut mem = TestMem::new().instructions(vec![
        0xE791_000A,    // LDR R0, [R1, +R10]
        0xE613_200A,    // LDR R2, [R3], -R10
        0xE7B5_410B,    // LDR R4, [R5, +R11 LSL 2]!
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0x82, 0x83, 0x84, 0x85,
        0x78, 0x56, 0x34, 0x12
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(1, 0x1000_0000);
                cpu.write_reg(3, 0x1000_0000);
                cpu.write_reg(5, 0x1000_0000);
                cpu.write_reg(10, 4);
                cpu.write_reg(11, 1);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x1234_5678);
                assert_eq!(cpu.read_reg(1), 0x1000_0000);
                assert_eq!(cpu.read_reg(2), 0x8584_8382);
                assert_eq!(cpu.read_reg(3), 0x0FFF_FFFC);
                assert_eq!(cpu.read_reg(4), 0x1234_5678);
                assert_eq!(cpu.read_reg(5), 0x1000_0004);
                assert_eq!(cpu.read_reg(10), 4);
                assert_eq!(cpu.read_reg(11), 1);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_store_word_imm_offset() {
    let mut mem = TestMem::new().instructions(vec![
        0xE581_0004,    // STR R0, [R1, +#4]
        0xE403_2004,    // STR R2, [R3], -#4
        0xE5A5_4004,    // STR R4, [R5, +#4]!
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(0, 0x1234_5678);
                cpu.write_reg(1, 0x1000_0000);
                cpu.write_reg(2, 0x2345_6789);
                cpu.write_reg(3, 0x1000_0000);
                cpu.write_reg(4, 0x9988_7766);
                cpu.write_reg(5, 0x1000_0004);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x1234_5678);
                assert_eq!(cpu.read_reg(1), 0x1000_0000);
                assert_eq!(cpu.read_reg(2), 0x2345_6789);
                assert_eq!(cpu.read_reg(3), 0x0FFF_FFFC);
                assert_eq!(cpu.read_reg(4), 0x9988_7766);
                assert_eq!(cpu.read_reg(5), 0x1000_0008);

                assert_eq!(cpu.ref_mem().data[0], 0x89);
                assert_eq!(cpu.ref_mem().data[1], 0x67);
                assert_eq!(cpu.ref_mem().data[2], 0x45);
                assert_eq!(cpu.ref_mem().data[3], 0x23);

                assert_eq!(cpu.ref_mem().data[4], 0x78);
                assert_eq!(cpu.ref_mem().data[5], 0x56);
                assert_eq!(cpu.ref_mem().data[6], 0x34);
                assert_eq!(cpu.ref_mem().data[7], 0x12);

                assert_eq!(cpu.ref_mem().data[8], 0x66);
                assert_eq!(cpu.ref_mem().data[9], 0x77);
                assert_eq!(cpu.ref_mem().data[10], 0x88);
                assert_eq!(cpu.ref_mem().data[11], 0x99);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_load_byte() {
    let mut mem = TestMem::new().instructions(vec![
        0xE5D1_0002,    // LDRB R0, [R1, +#2]
        0xE453_2004,    // LDRB R2, [R3], -#4
        0xE5F5_4005,    // LDRB R4, [R5, +#5]!
        // TODO: reg offset
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0x82, 0x83, 0x84, 0x85,
        0x78, 0x56, 0x34, 0x12
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(1, 0x1000_0002);
                cpu.write_reg(3, 0x1000_0000);
                cpu.write_reg(5, 0x1000_0000);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x78);
                assert_eq!(cpu.read_reg(1), 0x1000_0002);
                assert_eq!(cpu.read_reg(2), 0x82);
                assert_eq!(cpu.read_reg(3), 0x0FFF_FFFC);
                assert_eq!(cpu.read_reg(4), 0x56);
                assert_eq!(cpu.read_reg(5), 0x1000_0005);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_store_byte() {
    let mut mem = TestMem::new().instructions(vec![
        0xE5C1_0002,    // STRB R0, [R1, +#2]
        0xE443_2004,    // STRB R2, [R3], -#4
        0xE5E5_4005,    // STRB R4, [R5, +#5]!
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(0, 0x1234_5678);
                cpu.write_reg(1, 0x1000_0000);
                cpu.write_reg(2, 0x2345_6789);
                cpu.write_reg(3, 0x1000_0000);
                cpu.write_reg(4, 0x9988_7766);
                cpu.write_reg(5, 0x1000_0004);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x1234_5678);
                assert_eq!(cpu.read_reg(1), 0x1000_0000);
                assert_eq!(cpu.read_reg(2), 0x2345_6789);
                assert_eq!(cpu.read_reg(3), 0x0FFF_FFFC);
                assert_eq!(cpu.read_reg(4), 0x9988_7766);
                assert_eq!(cpu.read_reg(5), 0x1000_0009);

                assert_eq!(cpu.ref_mem().data[0], 0x89);
                assert_eq!(cpu.ref_mem().data[1], 0);
                assert_eq!(cpu.ref_mem().data[2], 0x78);
                assert_eq!(cpu.ref_mem().data[3], 0);

                assert_eq!(cpu.ref_mem().data[4], 0);
                assert_eq!(cpu.ref_mem().data[5], 0);
                assert_eq!(cpu.ref_mem().data[6], 0);
                assert_eq!(cpu.ref_mem().data[7], 0);

                assert_eq!(cpu.ref_mem().data[8], 0);
                assert_eq!(cpu.ref_mem().data[9], 0x66);
                assert_eq!(cpu.ref_mem().data[10], 0);
                assert_eq!(cpu.ref_mem().data[11], 0);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_load_halfword() {
    let mut mem = TestMem::new().instructions(vec![
        0xE1D1_00B2,    // LDRH R0, [R1, +#2]
        0xE053_20B4,    // LDRH R2, [R3], -#4
        0xE1F5_40B6,    // LDRH R4, [R5, +#6]!
        // TODO: reg offset
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0x82, 0x83, 0x84, 0x85,
        0x78, 0x56, 0x34, 0x12
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(1, 0x1000_0002);
                cpu.write_reg(3, 0x1000_0000);
                cpu.write_reg(5, 0x1000_0000);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x5678);
                assert_eq!(cpu.read_reg(1), 0x1000_0002);
                assert_eq!(cpu.read_reg(2), 0x8382);
                assert_eq!(cpu.read_reg(3), 0x0FFF_FFFC);
                assert_eq!(cpu.read_reg(4), 0x1234);
                assert_eq!(cpu.read_reg(5), 0x1000_0006);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_load_signed_halfword() {
    let mut mem = TestMem::new().instructions(vec![
        0xE1D1_00F2,    // LDRSH R0, [R1, +#2]
        0xE053_20F4,    // LDRSH R2, [R3], -#4
        0xE1F5_40F6,    // LDRSH R4, [R5, +#6]!
        // TODO: reg offset
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0x82, 0x83, 0x84, 0x85,
        0x78, 0x56, 0x34, 0x12
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(1, 0x1000_0002);
                cpu.write_reg(3, 0x1000_0000);
                cpu.write_reg(5, 0x1000_0000);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x5678);
                assert_eq!(cpu.read_reg(1), 0x1000_0002);
                assert_eq!(cpu.read_reg(2), 0xFFFF_8382);
                assert_eq!(cpu.read_reg(3), 0x0FFF_FFFC);
                assert_eq!(cpu.read_reg(4), 0x1234);
                assert_eq!(cpu.read_reg(5), 0x1000_0006);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_load_signed_byte() {
    let mut mem = TestMem::new().instructions(vec![
        0xE1D1_00D2,    // LDRSB R0, [R1, +#2]
        0xE053_20D4,    // LDRSB R2, [R3], -#4
        0xE1F5_40D5,    // LDRSB R4, [R5, +#5]!
        // TODO: reg offset
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0x82, 0x83, 0x84, 0x85,
        0x78, 0x56, 0x34, 0x12
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(1, 0x1000_0002);
                cpu.write_reg(3, 0x1000_0000);
                cpu.write_reg(5, 0x1000_0000);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x78);
                assert_eq!(cpu.read_reg(1), 0x1000_0002);
                assert_eq!(cpu.read_reg(2), 0xFFFF_FF82);
                assert_eq!(cpu.read_reg(3), 0x0FFF_FFFC);
                assert_eq!(cpu.read_reg(4), 0x56);
                assert_eq!(cpu.read_reg(5), 0x1000_0005);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_store_halfword() {
    let mut mem = TestMem::new().instructions(vec![
        0xE1C1_00B2,    // STRB R0, [R1, +#2]
        0xE043_20B4,    // STRB R2, [R3], -#4
        0xE1E5_40B6,    // STRB R4, [R5, +#6]!
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(0, 0x1234_5678);
                cpu.write_reg(1, 0x1000_0000);
                cpu.write_reg(2, 0x2345_6789);
                cpu.write_reg(3, 0x1000_0000);
                cpu.write_reg(4, 0x9988_7766);
                cpu.write_reg(5, 0x1000_0004);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x1234_5678);
                assert_eq!(cpu.read_reg(1), 0x1000_0000);
                assert_eq!(cpu.read_reg(2), 0x2345_6789);
                assert_eq!(cpu.read_reg(3), 0x0FFF_FFFC);
                assert_eq!(cpu.read_reg(4), 0x9988_7766);
                assert_eq!(cpu.read_reg(5), 0x1000_000A);

                assert_eq!(cpu.ref_mem().data[0], 0x89);
                assert_eq!(cpu.ref_mem().data[1], 0x67);
                assert_eq!(cpu.ref_mem().data[2], 0x78);
                assert_eq!(cpu.ref_mem().data[3], 0x56);

                assert_eq!(cpu.ref_mem().data[4], 0);
                assert_eq!(cpu.ref_mem().data[5], 0);
                assert_eq!(cpu.ref_mem().data[6], 0);
                assert_eq!(cpu.ref_mem().data[7], 0);

                assert_eq!(cpu.ref_mem().data[8], 0);
                assert_eq!(cpu.ref_mem().data[9], 0);
                assert_eq!(cpu.ref_mem().data[10], 0x66);
                assert_eq!(cpu.ref_mem().data[11], 0x77);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_swp_word() {
    let mut mem = TestMem::new().instructions(vec![
        0xE101_8090,    // SWP R8, R0, [R1]
        0xE103_2092,    // SWP R2, R2, [R3]
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0x82, 0x83, 0x84, 0x85,
        0x66, 0x77, 0x88, 0x99
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(0, 0x1234_5678);
                cpu.write_reg(1, 0x1000_0000);
                cpu.write_reg(2, 0x2345_6789);
                cpu.write_reg(3, 0x1000_0004);
                cpu.write_reg(8, 0xFFEE_DDCC);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x1234_5678);
                assert_eq!(cpu.read_reg(1), 0x1000_0000);
                assert_eq!(cpu.read_reg(2), 0x9988_7766);
                assert_eq!(cpu.read_reg(3), 0x1000_0004);
                assert_eq!(cpu.read_reg(8), 0x8584_8382);

                assert_eq!(cpu.ref_mem().data[0], 0x78);
                assert_eq!(cpu.ref_mem().data[1], 0x56);
                assert_eq!(cpu.ref_mem().data[2], 0x34);
                assert_eq!(cpu.ref_mem().data[3], 0x12);

                assert_eq!(cpu.ref_mem().data[4], 0x89);
                assert_eq!(cpu.ref_mem().data[5], 0x67);
                assert_eq!(cpu.ref_mem().data[6], 0x45);
                assert_eq!(cpu.ref_mem().data[7], 0x23);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_swp_byte() {
    let mut mem = TestMem::new().instructions(vec![
        0xE141_8090,    // SWPB R8, R0, [R1]
        0xE143_2092,    // SWPB R2, R2, [R3]
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0x82, 0x83, 0x84, 0x85,
        0x66, 0x77, 0x88, 0x99
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(0, 0x1234_5678);
                cpu.write_reg(1, 0x1000_0000);
                cpu.write_reg(2, 0x2345_6789);
                cpu.write_reg(3, 0x1000_0005);
                cpu.write_reg(8, 0xFFEE_DDCC);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(0), 0x1234_5678);
                assert_eq!(cpu.read_reg(1), 0x1000_0000);
                assert_eq!(cpu.read_reg(2), 0x77);
                assert_eq!(cpu.read_reg(3), 0x1000_0005);
                assert_eq!(cpu.read_reg(8), 0x82);

                assert_eq!(cpu.ref_mem().data[0], 0x78);
                assert_eq!(cpu.ref_mem().data[1], 0x83);
                assert_eq!(cpu.ref_mem().data[2], 0x84);
                assert_eq!(cpu.ref_mem().data[3], 0x85);

                assert_eq!(cpu.ref_mem().data[4], 0x66);
                assert_eq!(cpu.ref_mem().data[5], 0x89);
                assert_eq!(cpu.ref_mem().data[6], 0x88);
                assert_eq!(cpu.ref_mem().data[7], 0x99);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_load_multiple() {
    let mut mem = TestMem::new().instructions(vec![
        0xE8BD_01F8,    // LDMIA R13!, {R3-8}
        0xE91D_0402,    // LDMDB R13, {R1,R10}
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0x82, 0x83, 0x84, 0x85,
        0x78, 0x56, 0x34, 0x12,
        0x11, 0x22, 0x33, 0x44,
        0x55, 0x66, 0x77, 0x88,
        0x98, 0x76, 0x54, 0x32,
        0x01, 0x02, 0x03, 0x04,
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(13, 0x1000_0000);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(1), 0x3254_7698);
                assert_eq!(cpu.read_reg(3), 0x8584_8382);
                assert_eq!(cpu.read_reg(4), 0x1234_5678);
                assert_eq!(cpu.read_reg(5), 0x4433_2211);
                assert_eq!(cpu.read_reg(6), 0x8877_6655);
                assert_eq!(cpu.read_reg(7), 0x3254_7698);
                assert_eq!(cpu.read_reg(8), 0x0403_0201);
                assert_eq!(cpu.read_reg(10), 0x0403_0201);
                assert_eq!(cpu.read_reg(13), 0x1000_0018);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_store_multiple() {
    let mut mem = TestMem::new().instructions(vec![
        0xE8AD_01F8,    // STMIA R13!, {R3-8}
        0xE90C_0402,    // STMDB R12, {R1,R10}
        0xE1A0_F00E,    // MOV R15, R14
    ]).data(vec![
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            {
                let mut cpu = ARM7TDMI::new(mem.clone(), HashMap::new(), None);
                cpu.write_reg(1, 0xFFEE_DDCC);
                cpu.write_reg(3, 0x8584_8382);
                cpu.write_reg(4, 0x1234_5678);
                cpu.write_reg(5, 0x4433_2211);
                cpu.write_reg(6, 0x8877_6655);
                cpu.write_reg(7, 0x3254_7698);
                cpu.write_reg(8, 0x0403_0201);
                cpu.write_reg(10, 0xCDEF_ABBA);
                cpu.write_reg(12, 0x1000_0020);
                cpu.write_reg(13, 0x1000_0000);
    
                routine.call(&mut cpu);
    
                assert_eq!(cpu.read_reg(1), 0xFFEE_DDCC);
                assert_eq!(cpu.read_reg(3), 0x8584_8382);
                assert_eq!(cpu.read_reg(4), 0x1234_5678);
                assert_eq!(cpu.read_reg(5), 0x4433_2211);
                assert_eq!(cpu.read_reg(6), 0x8877_6655);
                assert_eq!(cpu.read_reg(7), 0x3254_7698);
                assert_eq!(cpu.read_reg(8), 0x0403_0201);
                assert_eq!(cpu.read_reg(10), 0xCDEF_ABBA);
                assert_eq!(cpu.read_reg(12), 0x1000_0020);
                assert_eq!(cpu.read_reg(13), 0x1000_0018);

                assert_eq!(cpu.ref_mem().data[0], 0x82);
                assert_eq!(cpu.ref_mem().data[1], 0x83);
                assert_eq!(cpu.ref_mem().data[2], 0x84);
                assert_eq!(cpu.ref_mem().data[3], 0x85);

                assert_eq!(cpu.ref_mem().data[4], 0x78);
                assert_eq!(cpu.ref_mem().data[5], 0x56);
                assert_eq!(cpu.ref_mem().data[6], 0x34);
                assert_eq!(cpu.ref_mem().data[7], 0x12);

                assert_eq!(cpu.ref_mem().data[8], 0x11);
                assert_eq!(cpu.ref_mem().data[9], 0x22);
                assert_eq!(cpu.ref_mem().data[10], 0x33);
                assert_eq!(cpu.ref_mem().data[11], 0x44);

                assert_eq!(cpu.ref_mem().data[12], 0x55);
                assert_eq!(cpu.ref_mem().data[13], 0x66);
                assert_eq!(cpu.ref_mem().data[14], 0x77);
                assert_eq!(cpu.ref_mem().data[15], 0x88);

                assert_eq!(cpu.ref_mem().data[16], 0x98);
                assert_eq!(cpu.ref_mem().data[17], 0x76);
                assert_eq!(cpu.ref_mem().data[18], 0x54);
                assert_eq!(cpu.ref_mem().data[19], 0x32);

                assert_eq!(cpu.ref_mem().data[20], 0x01);
                assert_eq!(cpu.ref_mem().data[21], 0x02);
                assert_eq!(cpu.ref_mem().data[22], 0x03);
                assert_eq!(cpu.ref_mem().data[23], 0x04);

                assert_eq!(cpu.ref_mem().data[24], 0xCC);
                assert_eq!(cpu.ref_mem().data[25], 0xDD);
                assert_eq!(cpu.ref_mem().data[26], 0xEE);
                assert_eq!(cpu.ref_mem().data[27], 0xFF);

                assert_eq!(cpu.ref_mem().data[28], 0xBA);
                assert_eq!(cpu.ref_mem().data[29], 0xAB);
                assert_eq!(cpu.ref_mem().data[30], 0xEF);
                assert_eq!(cpu.ref_mem().data[31], 0xCD);
            }
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}

#[test]
fn test_load_pc_relative() {
    let mut mem = TestMem::new().instructions(vec![
        0xE59F_0000,    // LDR R0, [R15]
        0xE1A0_F00E,    // MOV R15, R14
        0xABCD_EF01,    // some data
        0x0000_0000,    // garbage
    ]).build();
    let mut compiler = super::ARMv4Compiler::new();
    let routine = compiler.compile_arm::<TestMem, ARM7TDMI<_>>(0, &mut mem);
    match routine {
        Ok(routine) => {
            run_test!(mem, routine, [0, 0, 0xABCD_EF01u32]);
        },
        Err(e) => panic!("unexpected err {:?}", e)
    }
}
