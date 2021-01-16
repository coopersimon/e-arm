// TODO: use test core instead of this one.
use crate::core::*;
use crate::memory::*;

/// Construct a word from bytes (high to low).
const fn make_32(bytes: &[u8]) -> u32 {
    ((bytes[3] as u32) << 24) |
    ((bytes[2] as u32) << 16) |
    ((bytes[1] as u32) << 8) |
    (bytes[0] as u32)
}

struct TestARM4Core {
    regs: [u32; 16],

    cpsr: CPSR,
    spsr: CPSR,

    memory: Vec<u32>,
}

impl TestARM4Core {
    fn new() -> Self {
        let data = (0..1024*64).map(|i| (i & 0xFF) as u8)
            .collect::<Vec<_>>()
            .chunks_exact(4)
            .map(make_32)
            .collect::<Vec<_>>();
        Self {
            regs: [0; 16],

            cpsr: Default::default(),
            spsr: Default::default(),

            memory: data,
        }
    }
}

impl Mem32 for TestARM4Core {
    type Addr = u32;

    fn load_byte(&mut self, addr: Self::Addr) -> (u8, usize) {
        let idx = (addr >> 2) as usize;
        let data = self.memory[idx];
        let shift = (addr & 3) * 8;
        let ret = (data >> shift) as u8;
        (ret, 1)
    }
    fn store_byte(&mut self, addr: Self::Addr, data: u8) -> usize {
        let idx = (addr >> 2) as usize;
        let stored = self.memory[idx];
        let shift = (addr & 3) * 8;
        let mask = !(0xFF << shift);
        self.memory[idx] = (stored & mask) | ((data as u32) << shift);
        1
    }

    fn load_halfword(&mut self, addr: Self::Addr) -> (u16, usize) {
        let idx = (addr >> 2) as usize;
        let data = self.memory[idx];
        let shift = (addr & 2) * 8;
        let ret = (data >> shift) as u16;
        (ret, 1)
    }
    fn store_halfword(&mut self, addr: Self::Addr, data: u16) -> usize {
        let idx = (addr >> 2) as usize;
        let stored = self.memory[idx];
        let shift = (addr & 2) * 8;
        let mask = !(0xFFFF << shift);
        self.memory[idx] = (stored & mask) | ((data as u32) << shift);
        1
    }

    fn load_word(&mut self, addr: Self::Addr) -> (u32, usize) {
        let idx = (addr >> 2) as usize;
        (self.memory[idx], 1)
    }
    fn store_word(&mut self, addr: Self::Addr, data: u32) -> usize {
        let idx = (addr >> 2) as usize;
        self.memory[idx] = data;
        1
    }
}

impl ARMCore for TestARM4Core {
    fn read_reg(&self, n: usize) -> u32 {
        self.regs[n]
    }
    fn write_reg(&mut self, n: usize, data: u32) {
        self.regs[n] = data;
    }

    fn read_usr_reg(&self, n: usize) -> u32 {
        self.regs[n]
    }
    fn write_usr_reg(&mut self, n: usize, data: u32) {
        self.regs[n] = data;
    }

    fn read_cpsr(&self) -> CPSR {
        self.cpsr
    }
    fn write_cpsr(&mut self, data: CPSR) {
        self.cpsr = data;
    }

    fn read_spsr(&self) -> CPSR {
        self.spsr
    }
    fn write_spsr(&mut self, data: CPSR) {
        self.spsr = data;
    }

    fn trigger_exception(&mut self, exception: Exception) {
        // TODO...
    }
    fn return_from_exception(&mut self) {
        // TODO...
    }

    fn ref_coproc<'a>(&'a mut self, coproc: usize) -> Option<&'a mut Box<dyn Coprocessor>> {
        None
    }
}

impl ARMv4 for TestARM4Core {}

// Use to setup in state
#[derive(Default)]
struct TestIn {
    regs: Vec<u32>,
    cpsr: Option<CPSR>,
    instr: u32,
}

impl TestIn {
    fn run_test(&self, test_num: usize, out: &TestOut) {
        let mut cpu = TestARM4Core::new();
        for (i, val) in self.regs.iter().enumerate() {
            cpu.regs[i] = *val;
        }
        if let Some(init_flags) = self.cpsr {
            cpu.cpsr = init_flags;
        }
        
        let cycles = cpu.execute_instruction(self.instr);

        for (i, val) in out.regs.iter().enumerate() {
            if let Some(assert_reg) = val {
                if *assert_reg != cpu.regs[i] {
                    println!("{}: Got r{}: {:X} expected: {:X}", test_num, i, cpu.regs[i], *assert_reg);
                    assert!(false);
                }
            }
        }
        if out.cpsr != cpu.cpsr {
            println!("{}: Got flags: {:X} expected: {:X}", test_num, cpu.cpsr.bits(), out.cpsr.bits());
            assert!(false);
        }

        if out.cycles != cycles {
            println!("{}: Got {} cycles, expected: {}", test_num, cycles, out.cycles);
            assert!(false);
        }
    }
}

// Use to assert output state
#[derive(Default)]
struct TestOut {
    regs: Vec<Option<u32>>,
    cpsr: CPSR,
    cycles: usize,
}


#[test]
fn test_and() {
    let data = vec![
        (
            // AND R2, R0, R1: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=2, Sh=0, Rm=1
            TestIn {
                regs: vec![0x15151515, 0x84848484, 1],
                cpsr: None,
                instr: 0xE0002001
            },
            TestOut {
                regs: vec![None, None, Some(0x04040404)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // AND R1, R0, #0x1F0000: Cond=AL, I=1, Instr=0, S=0, Rn=0, Rd=1, Rot=16, Imm=1F
            TestIn {
                regs: vec![0x95959595, 1],
                cpsr: None,
                instr: 0xE200181F
            },
            TestOut {
                regs: vec![None, Some(0x150000)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // ANDS R1, R0, #0: Cond=AL, I=1, Instr=0, S=1, Rn=0, Rd=1, Rot=0, Imm=0
            TestIn {
                regs: vec![0x95959595, 1],
                cpsr: None,
                instr: 0xE2101000
            },
            TestOut {
                regs: vec![None, Some(0)],
                cpsr: CPSR::Z,
                cycles: 0,
            }
        ),
        (
            // ANDS R1, R0, R1 << #0: Cond=AL, I=0, Instr=0, S=1, Rn=0, Rd=1, Sh=(Imm=0, ASL), Rm=1, 
            TestIn {
                regs: vec![0x95959595, 0x8181FFFF],
                cpsr: Some(CPSR::C),
                instr: 0xE0101001
            },
            TestOut {
                regs: vec![None, Some(0x81819595)],
                cpsr: CPSR::N | CPSR::C,
                cycles: 0,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_eor() {
    let data = vec![
        (
            // EOR R2, R0, R1: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=2, Sh=0, Rm=1
            TestIn {
                regs: vec![0x15151515, 0x84848484, 1],
                cpsr: None,
                instr: 0xE0202001
            },
            TestOut {
                regs: vec![None, None, Some(0x91919191)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // EOR R1, R0, #0x1F0000: Cond=AL, I=1, Instr=0, S=0, Rn=0, Rd=1, Rot=16, Imm=1F
            TestIn {
                regs: vec![0x95959595, 1],
                cpsr: None,
                instr: 0xE220181F
            },
            TestOut {
                regs: vec![None, Some(0x958A9595)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // EORS R1, R0, #0: Cond=AL, I=1, Instr=0, S=1, Rn=0, Rd=1, Rot=0, Imm=0
            TestIn {
                regs: vec![0x95959595, 1],
                cpsr: None,
                instr: 0xE2301000
            },
            TestOut {
                regs: vec![None, Some(0x95959595)],
                cpsr: CPSR::N,
                cycles: 0,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_orr() {
    let data = vec![
        (
            // ORR R2, R0, R1: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=2, Sh=0, Rm=1
            TestIn {
                regs: vec![0x15151515, 0x84848484, 1],
                cpsr: None,
                instr: 0xE1802001
            },
            TestOut {
                regs: vec![None, None, Some(0x95959595)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // ORR R1, R0, #0x1F0000: Cond=AL, I=1, Instr=0, S=0, Rn=0, Rd=1, Rot=16, Imm=1F
            TestIn {
                regs: vec![0x95959595, 1],
                cpsr: Some(CPSR::Z),
                instr: 0xE380181F
            },
            TestOut {
                regs: vec![None, Some(0x959F9595)],
                cpsr: CPSR::Z,
                cycles: 0,
            }
        ),
        (
            // ORRS R1, R0, #0: Cond=AL, I=1, Instr=0, S=1, Rn=0, Rd=1, Rot=0, Imm=0
            TestIn {
                regs: vec![0x95959595, 1],
                cpsr: None,
                instr: 0xE3901000
            },
            TestOut {
                regs: vec![None, Some(0x95959595)],
                cpsr: CPSR::N,
                cycles: 0,
            }
        ),
        (
            // ORRS R1, R0, R1 >> #0: Cond=AL, I=0, Instr=0, S=1, Rn=0, Rd=1, Sh=(Imm=0, LSR), Rm=1, 
            TestIn {
                regs: vec![0x95959595, 0x81810000],
                cpsr: Some(CPSR::C),
                instr: 0xE1901041
            },
            TestOut {
                regs: vec![None, Some(0xFFFFFFFF)],
                cpsr: CPSR::N | CPSR::C,
                cycles: 0,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_bic() {
    let data = vec![
        (
            // BIC R2, R0, R1: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=2, Sh=0, Rm=1
            TestIn {
                regs: vec![0x15151515, 0x84848484, 1],
                cpsr: None,
                instr: 0xE1C02001
            },
            TestOut {
                regs: vec![None, None, Some(0x11111111)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // BIC R1, R0, #0x1F0000: Cond=AL, I=1, Instr=0, S=0, Rn=0, Rd=1, Rot=16, Imm=1F
            TestIn {
                regs: vec![0x95959595, 1],
                cpsr: None,
                instr: 0xE3C0181F
            },
            TestOut {
                regs: vec![None, Some(0x95809595)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // BICS R1, R0, #0: Cond=AL, I=1, Instr=0, S=0, Rn=0, Rd=1, Rot=0, Imm=0
            TestIn {
                regs: vec![0x35353535, 1],
                cpsr: None,
                instr: 0xE3D01000
            },
            TestOut {
                regs: vec![None, Some(0x35353535)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

// Test arithmetic

#[test]
fn test_add() {
    let data = vec![
        (
            // ADD R2, R0, R1: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=2, Sh=0, Rm=1
            TestIn {
                regs: vec![0x123, 0x456, 0x1],
                cpsr: None,
                instr: 0xE0802001
            },
            TestOut {
                regs: vec![None, None, Some(0x579)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // ADDS R1, R0, #0xF0000001: Cond=AL, I=1, Instr=0, S=1, Rn=0, Rd=1, Rot=4, Imm=1F
            TestIn {
                regs: vec![0xFFFFFFF],
                cpsr: None,
                instr: 0xE290121F
            },
            TestOut {
                regs: vec![None, Some(0)],
                cpsr: CPSR::C | CPSR::Z,
                cycles: 0,
            }
        ),
        (
            // ADDS R0, R0, R1 << #6: Cond=AL, I=0, Instr=0, S=1, Rn=0, Rd=0, Sh=(Imm=6, ASL), Rm=1
            TestIn {
                regs: vec![0xFFFF, 0x3],
                cpsr: Some(CPSR::C),
                instr: 0xE0900301
            },
            TestOut {
                regs: vec![Some(0x100BF)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // ADD R0, R0, R1 >> R2: Cond=AL, I=0, Instr=0, S=1, Rn=0, Rd=0, Sh=(R=2, LSR), Rm=1
            TestIn {
                regs: vec![0xFFFF, 0xFFFF, 0x10003],
                cpsr: Some(CPSR::C),
                instr: 0xE0800231
            },
            TestOut {
                regs: vec![Some(0x11FFE)],
                cpsr: CPSR::C,
                cycles: 1,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_sub() {
    let data = vec![
        (
            // SUB R2, R0, R1: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=2, Sh=0, Rm=1
            TestIn {
                regs: vec![0x123, 0x456, 0x1],
                cpsr: None,
                instr: 0xE0402001
            },
            TestOut {
                regs: vec![None, None, Some(0xFFFFFCCD)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // SUBS R1, R0, #0xF0000001: Cond=AL, I=1, Instr=0, S=1, Rn=0, Rd=1, Rot=4, Imm=1F
            TestIn {
                regs: vec![0xFFFFFFF],
                cpsr: None,
                instr: 0xE250121F
            },
            TestOut {
                regs: vec![None, Some(0x1FFFFFFE)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // SUBS R0, R0, R1 << #6: Cond=AL, I=0, Instr=0, S=1, Rn=0, Rd=0, Sh=(Imm=6, ASL), Rm=1
            TestIn {
                regs: vec![0xFFFF, 0x3],
                cpsr: Some(CPSR::V),
                instr: 0xE0500301
            },
            TestOut {
                regs: vec![Some(0xFF3F)],
                cpsr: CPSR::C,
                cycles: 0,
            }
        ),
        (
            // SUB R0, R0, R1 >> R2: Cond=AL, I=0, Instr=0, S=1, Rn=0, Rd=0, Sh=(R=2, LSR), Rm=1
            TestIn {
                regs: vec![0xFFFF, 0xFFFF, 0x10003],
                cpsr: Some(CPSR::C),
                instr: 0xE0400231
            },
            TestOut {
                regs: vec![Some(0xE000)],
                cpsr: CPSR::C,
                cycles: 1,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_rsb() {
    let data = vec![
        (
            // RSB R2, R0, R1: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=2, Sh=0, Rm=1
            TestIn {
                regs: vec![0x123, 0x456, 0x1],
                cpsr: Some(CPSR::C),
                instr: 0xE0602001
            },
            TestOut {
                regs: vec![None, None, Some(0x333)],
                cpsr: CPSR::C,
                cycles: 0,
            }
        ),
        (
            // RSBS R1, R0, #0x1F00: Cond=AL, I=1, Instr=0, S=1, Rn=0, Rd=1, Rot=24, Imm=1F
            TestIn {
                regs: vec![0x1F01],
                cpsr: Some(CPSR::C),
                instr: 0xE2701C1F
            },
            TestOut {
                regs: vec![None, Some(0xFFFFFFFF)],
                cpsr: CPSR::N,
                cycles: 0,
            }
        ),
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_adc() {
    let data = vec![
        (
            // ADC R2, R0, R1: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=2, Sh=0, Rm=1
            TestIn {
                regs: vec![0x123, 0x456, 0x1],
                cpsr: Some(CPSR::C | CPSR::Z),
                instr: 0xE0A02001
            },
            TestOut {
                regs: vec![None, None, Some(0x57A)],
                cpsr: CPSR::C | CPSR::Z,
                cycles: 0,
            }
        ),
        (
            // ADCS R1, R0, #1F: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=1, Rot=0, Imm=1F
            TestIn {
                regs: vec![0xFFFFFFE0],
                cpsr: Some(CPSR::C),
                instr: 0xE2B0101F
            },
            TestOut {
                regs: vec![None, Some(0x0)],
                cpsr: CPSR::C | CPSR::Z,
                cycles: 0,
            }
        ),
        (
            // ADCS R1, R0, R1 >> R2: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=1, Sh=(R=2, ASR), Rm=1
            TestIn {
                regs: vec![0x12, 0xFFFFFF00, 0x8],
                cpsr: Some(CPSR::V),
                instr: 0xE0B01251
            },
            TestOut {
                regs: vec![None, Some(0x11)],
                cpsr: CPSR::C,
                cycles: 1,
            }
        ),
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_sbc() {
    let data = vec![
        (
            // SBC R2, R0, R1: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=2, Sh=0, Rm=1
            TestIn {
                regs: vec![0x123, 0x456, 0x1],
                cpsr: Some(CPSR::C | CPSR::Z),
                instr: 0xE0C02001
            },
            TestOut {
                regs: vec![None, None, Some(0xFFFFFCCD)],
                cpsr: CPSR::C | CPSR::Z,
                cycles: 0,
            }
        ),
        (
            // SBCS R1, R0, #1F: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=1, Rot=0, Imm=1F
            TestIn {
                regs: vec![0xFFFFFFE0],
                cpsr: Some(CPSR::C),
                instr: 0xE2D0101F
            },
            TestOut {
                regs: vec![None, Some(0xFFFFFFC1)],
                cpsr: CPSR::N | CPSR::C,
                cycles: 0,
            }
        ),
        (
            // SBCS R1, R0, R1 >> R2: Cond=AL, I=0, Instr=0, S=0, Rn=0, Rd=1, Sh=(R=2, ASR), Rm=1
            TestIn {
                regs: vec![0x12, 0xFFFFFF00, 0x8],
                cpsr: Some(CPSR::V),
                instr: 0xE0D01251
            },
            TestOut {
                regs: vec![None, Some(0x12)],
                cpsr: CPSR::default(),
                cycles: 1,
            }
        ),
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_mul() {
    let data = vec![
        (
            // MULS R2, R0, R1: Cond=AL, S=1, Rd=2, Rn=0, Rs=1, Rm=0
            TestIn {
                regs: vec![0x123, 0x456, 0x1],
                cpsr: Some(CPSR::C | CPSR::Z),
                instr: 0xE0120190
            },
            TestOut {
                regs: vec![None, None, Some(0x4EDC2)],
                cpsr: CPSR::default(),
                cycles: 2,
            }
        ),
        (
            // MULS R0, R0, R1: Cond=AL, S=1, Rd=0, Rn=0, Rs=1, Rm=0
            TestIn {
                regs: vec![0x123, 0x0, 0x1],
                cpsr: Some(CPSR::C | CPSR::Z),
                instr: 0xE0100190
            },
            TestOut {
                regs: vec![Some(0), None, None],
                cpsr: CPSR::Z,
                cycles: 1,
            }
        ),
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_mla() {
    let data = vec![
        (
            // MLA R2, R0, R1 + R3: Cond=AL, S=1, Rd=2, Rn=3, Rs=1, Rm=0
            TestIn {
                regs: vec![0x123, 0x456, 0x1, 0x12345],
                cpsr: Some(CPSR::C | CPSR::Z),
                instr: 0xE0223190
            },
            TestOut {
                regs: vec![None, None, Some(0x61107)],
                cpsr: CPSR::C | CPSR::Z,
                cycles: 3,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}


// Test comparison

// Test branch


// Test move

#[test]
fn test_mrs() {
    let data = vec![
        (
            // MRS R0, CPSR
            TestIn {
                regs: vec![0x123456],
                cpsr: Some(CPSR::C | CPSR::V | CPSR::USR),
                instr: 0xE10F_0000
            },
            TestOut {
                regs: vec![Some(0x3000_0010)],
                cpsr: CPSR::C | CPSR::V | CPSR::USR,
                cycles: 0,
            }
        ),
        (
            // MRS R0, SPSR
            TestIn {
                regs: vec![0x123456],
                cpsr: None,
                instr: 0xE14F_0000
            },
            TestOut {
                regs: vec![Some(0)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_msr() {
    let data = vec![
        (
            // MSR CPSR_flg, R1
            TestIn {
                regs: vec![0x3000_0011, 0x9000_0011],
                cpsr: Some(CPSR::C | CPSR::V | CPSR::USR),
                instr: 0xE128_F001
            },
            TestOut {
                regs: Vec::new(),
                cpsr: CPSR::N | CPSR::V | CPSR::USR,
                cycles: 0,
            }
        ),
        (
            // MSR CPSR_fc, R0
            TestIn {
                regs: vec![0x3000_0010, 0x9000_0010],
                cpsr: Some(CPSR::SVC),
                instr: 0xE129_F000
            },
            TestOut {
                regs: Vec::new(),
                cpsr: CPSR::C | CPSR::V | CPSR::USR,
                cycles: 0,
            }
        ),
        (
            // MSR CPSR_flg, 0xF000_0000
            TestIn {
                regs: vec![0x3000_0010, 0x9000_0010],
                cpsr: Some(CPSR::USR),
                instr: 0xE328_F4F0
            },
            TestOut {
                regs: Vec::new(),
                cpsr: CPSR::N | CPSR::Z | CPSR::C | CPSR::V | CPSR::USR,
                cycles: 0,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_ldr() {
    let data = vec![
        (
            // LDR R0, [R1]: Cond=AL, I=0, P=0, U=0, B=0, T=0, L=1, Rn=1, Rd=0, Imm=0
            TestIn {
                regs: vec![0x14, 0x44],
                cpsr: None,
                instr: 0xE411_0000
            },
            TestOut {
                regs: vec![Some(0x47_46_45_44), Some(0x44)],
                cpsr: CPSR::default(),
                cycles: 2,
            }
        ),
        (
            // LDR R0, [R1, #8]!: Cond=AL, I=0, P=1, U=1, B=0, W=1, L=1, Rn=1, Rd=0, Imm=8
            TestIn {
                regs: vec![0x14, 0x44],
                cpsr: None,
                instr: 0xE5B1_0008
            },
            TestOut {
                regs: vec![Some(0x4F_4E_4D_4C), Some(0x4C)],
                cpsr: CPSR::default(),
                cycles: 2,
            }
        ),
        (
            // LDR R1, [R0] #-8: Cond=AL, I=0, P=0, U=0, B=0, T=0, L=1, Rn=0, Rd=1, Imm=8
            TestIn {
                regs: vec![0x14, 0x44],
                cpsr: None,
                instr: 0xE410_1008
            },
            TestOut {
                regs: vec![Some(0xC), Some(0x17_16_15_14)],
                cpsr: CPSR::default(),
                cycles: 2,
            }
        ),
        (
            // LDR R1, [R0, {R2, LSL #1}]: Cond=AL, I=1, P=1, U=1, B=0, W=0, L=1, Rn=0, Rd=1, Is=1, Type=LSL, Rm=2
            TestIn {
                regs: vec![0x14, 0x44, 0x10],
                cpsr: None,
                instr: 0xE790_1082
            },
            TestOut {
                regs: vec![Some(0x14), Some(0x37_36_35_34), Some(0x10)],
                cpsr: CPSR::default(),
                cycles: 2,
            }
        ),
        (
            // LDRB R1, [R0, {R2, LSR #1}]!: Cond=AL, I=1, P=1, U=1, B=1, W=1, L=1, Rn=0, Rd=1, Is=1, Type=LSR, Rm=2
            TestIn {
                regs: vec![0x12, 0x44, 0x10],
                cpsr: None,
                instr: 0xE7F0_10A2
            },
            TestOut {
                regs: vec![Some(0x1A), Some(0x1A), Some(0x10)],
                cpsr: CPSR::default(),
                cycles: 2,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

// TODO: test str

#[test]
fn test_ldrh() {
    let data = vec![
        (
            // LDRH R0, [R1], #-8: Cond=AL, P=0, U=0, I=1, L=1, Rn=1, Rd=0, Imm=0 + 8
            TestIn {
                regs: vec![0x12, 0x44],
                cpsr: None,
                instr: 0xE05100B8
            },
            TestOut {
                regs: vec![Some(0x45_44), Some(0x3C)],
                cpsr: CPSR::default(),
                cycles: 2,
            }
        ),
        (
            // LDRH R0, [R1, #36]!: Cond=AL, P=1, U=1, I=1, W=1, L=1, Rn=1, Rd=0, Imm=32 + 4
            TestIn {
                regs: vec![0x33, 0x66],
                cpsr: None,
                instr: 0xE1F102B4
            },
            TestOut {
                regs: vec![Some(0x8B_8A), Some(0x8A)],
                cpsr: CPSR::default(),
                cycles: 2,
            }
        ),
        (
            // LDRH R1, [R0, R2]: Cond=AL, P=1, U=1, I=0, W=0, L=1, Rn=0, Rd=1, Rm=2
            TestIn {
                regs: vec![0x10, 0x11, 0x16],
                cpsr: None,
                instr: 0xE19010B2
            },
            TestOut {
                regs: vec![Some(0x10), Some(0x27_26), Some(0x16)],
                cpsr: CPSR::default(),
                cycles: 2,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_ldm() {
    let data = vec![
        (
            // LDMIB R10!, {R0-R9}: Cond=AL, P=1, U=1, S=0, W=1, L=1, Rn=10, Rlist=0x03FF
            TestIn {
                regs: vec![0x10, 0x12, 0x14, 0x16, 0x18, 0x1A, 0x1C, 0x1E, 0x20, 0x22, 0x24, 0x26, 0x28, 0x2A, 0x2C],
                cpsr: None,
                instr: 0xE9BA03FF
            },
            TestOut {
                regs: vec![Some(0x2B_2A_29_28), Some(0x2F_2E_2D_2C), Some(0x33_32_31_30), Some(0x37_36_35_34), Some(0x3B_3A_39_38),
                    Some(0x3F_3E_3D_3C), Some(0x43_42_41_40), Some(0x47_46_45_44), Some(0x4B_4A_49_48), Some(0x4F_4E_4D_4C),
                    Some(0x4C), Some(0x26), Some(0x28), Some(0x2A), Some(0x2C)],
                cpsr: CPSR::default(),
                cycles: 11,
            }
        ),
        (
            // LDMIA R4, {R0-R3}: Cond=AL, P=0, U=1, S=0, W=0, L=1, Rn=4, Rlist=0x000F
            TestIn {
                regs: vec![0x10, 0x12, 0x14, 0x16, 0x18, 0x1A, 0x1C, 0x1E, 0x20, 0x22, 0x24, 0x26, 0x28, 0x2A, 0x2C],
                cpsr: None,
                instr: 0xE894000F
            },
            TestOut {
                regs: vec![Some(0x1B_1A_19_18), Some(0x1F_1E_1D_1C), Some(0x23_22_21_20), Some(0x27_26_25_24), Some(0x18),
                    Some(0x1A), Some(0x1C), Some(0x1E), Some(0x20), Some(0x22),
                    Some(0x24), Some(0x26), Some(0x28), Some(0x2A), Some(0x2C)],
                cpsr: CPSR::default(),
                cycles: 5,
            }
        ),
        (
            // LDMDB R6, {R2-R4}: Cond=AL, P=1, U=0, S=0, W=0, L=1, Rn=5, Rlist=0x001C
            TestIn {
                regs: vec![0x10, 0x12, 0x14, 0x16, 0x18, 0x1A, 0x1C, 0x1E, 0x20, 0x22, 0x24, 0x26, 0x28, 0x2A, 0x2C],
                cpsr: None,
                instr: 0xE916001C
            },
            TestOut {
                regs: vec![Some(0x10), Some(0x12), Some(0x13_12_11_10), Some(0x17_16_15_14), Some(0x1B_1A_19_18),
                    Some(0x1A), Some(0x1C), Some(0x1E), Some(0x20), Some(0x22),
                    Some(0x24), Some(0x26), Some(0x28), Some(0x2A), Some(0x2C)],
                cpsr: CPSR::default(),
                cycles: 4,
            }
        ),
        (
            // LDMDA R12!, {R0-R3}: Cond=AL, P=0, U=0, S=0, W=1, L=1, Rn=12, Rlist=0x000F 28
            TestIn {
                regs: vec![0x10, 0x12, 0x14, 0x16, 0x18, 0x1A, 0x1C, 0x1E, 0x20, 0x22, 0x24, 0x26, 0x28, 0x2A, 0x2C],
                cpsr: None,
                instr: 0xE83C000F
            },
            TestOut {
                regs: vec![Some(0x1F_1E_1D_1C), Some(0x23_22_21_20), Some(0x27_26_25_24), Some(0x2B_2A_29_28), Some(0x18),
                    Some(0x1A), Some(0x1C), Some(0x1E), Some(0x20), Some(0x22),
                    Some(0x24), Some(0x26), Some(0x18), Some(0x2A), Some(0x2C)],
                cpsr: CPSR::default(),
                cycles: 5,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}