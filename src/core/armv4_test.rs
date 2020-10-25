// TODO: use test core instead of this one.
use crate::core::*;

struct TestARM4Core {
    regs: [u32; 16],

    cpsr: CPSR,

    cycles: usize,
}


impl TestARM4Core {
    pub fn new() -> Self {
        Self {
            regs: [0; 16],

            cpsr: Default::default(),

            cycles: 0,
        }
    }
}

impl ARMCore for TestARM4Core {
    fn read_reg(&self, n: usize) -> u32 {
        self.regs[n]
    }
    fn write_reg(&mut self, n: usize, data: u32) {
        self.regs[n] = data;
    }

    fn read_cpsr(&self) -> CPSR {
        self.cpsr
    }
    fn write_cpsr(&mut self, data: CPSR) {
        self.cpsr = data;
    }

    fn set_mode(&mut self, mode: Mode) {
        // TODO...
    }

    fn add_cycles(&mut self, cycles: usize) {
        self.cycles += cycles;
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
    fn run_test(&self, out: &TestOut) {
        let mut cpu = TestARM4Core::new();
        for (i, val) in self.regs.iter().enumerate() {
            cpu.regs[i] = *val;
        }
        if let Some(init_flags) = self.cpsr {
            cpu.cpsr = init_flags;
        }
        
        cpu.execute_instruction(self.instr);

        for (i, val) in out.regs.iter().enumerate() {
            if let Some(assert_reg) = val {
                if *assert_reg != cpu.regs[i] {
                    println!("Got r{}: {:X} expected: {:X}", i, cpu.regs[i], *assert_reg);
                    assert!(false);
                }
            }
        }
        if out.cpsr != cpu.cpsr {
            println!("Got flags: {:X} expected: {:X}", cpu.cpsr.bits(), out.cpsr.bits());
            assert!(false);
        }
        if let Some(assert_cycles) = out.cycles {
            assert_eq!(assert_cycles, cpu.cycles);
        }
    }
}

// Use to assert output state
#[derive(Default)]
struct TestOut {
    regs: Vec<Option<u32>>,
    cpsr: CPSR,
    cycles: Option<usize>,
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
                cycles: None,
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
                cycles: None,
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
                cycles: None,
            }
        )
    ];

    for (in_data, out_data) in data.iter() {
        in_data.run_test(out_data);
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
                cycles: None,
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
                cycles: None,
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
                cycles: None,
            }
        )
    ];

    for (in_data, out_data) in data.iter() {
        in_data.run_test(out_data);
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
                cycles: None,
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
                cycles: None,
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
                cycles: None,
            }
        )
    ];

    for (in_data, out_data) in data.iter() {
        in_data.run_test(out_data);
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
                cycles: None,
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
                cycles: None,
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
                cycles: None,
            }
        )
    ];

    for (in_data, out_data) in data.iter() {
        in_data.run_test(out_data);
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
                cycles: None,
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
                cycles: None,
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
                cycles: None,
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
                cycles: None,
            }
        )
    ];

    for (in_data, out_data) in data.iter() {
        in_data.run_test(out_data);
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
                cycles: None,
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
                cpsr: CPSR::C,
                cycles: None,
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
                cpsr: CPSR::default(),
                cycles: None,
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
                cycles: None,
            }
        )
    ];

    for (in_data, out_data) in data.iter() {
        in_data.run_test(out_data);
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
                cycles: None,
            }
        ),
        (
            // RSBS R1, R0, #0x1F00: Cond=AL, I=1, Instr=0, S=1, Rn=0, Rd=1, Rot=24, Imm=1F
            TestIn {
                regs: vec![0x1F01],
                cpsr: None,
                instr: 0xE2701C1F
            },
            TestOut {
                regs: vec![None, Some(0xFFFFFFFF)],
                cpsr: CPSR::C | CPSR::N,
                cycles: None,
            }
        ),
    ];

    for (in_data, out_data) in data.iter() {
        in_data.run_test(out_data);
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
                cycles: None,
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
                cycles: None,
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
                cycles: None,
            }
        ),
    ];

    for (in_data, out_data) in data.iter() {
        in_data.run_test(out_data);
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
                cycles: None,
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
                cycles: None,
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
                cycles: None,
            }
        ),
    ];

    for (in_data, out_data) in data.iter() {
        in_data.run_test(out_data);
    }
}



// Test comparison
// Test move