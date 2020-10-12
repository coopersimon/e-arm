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
        if let Some(assert_flags) = out.cpsr {
            if assert_flags != cpu.cpsr {
                println!("Got flags: {:X} expected: {:X}", cpu.cpsr.bits(), assert_flags.bits());
                assert!(false);
            }
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
    cpsr: Option<CPSR>,
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
                cpsr: Some(CPSR::default()),
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
                cpsr: Some(CPSR::default()),
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
                cpsr: Some(CPSR::Z),
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
                cpsr: Some(CPSR::default()),
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
                cpsr: Some(CPSR::default()),
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
                cpsr: Some(CPSR::N),
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
                cpsr: Some(CPSR::default()),
                cycles: None,
            }
        ),
        (
            // ORR R1, R0, #0x1F0000: Cond=AL, I=1, Instr=0, S=0, Rn=0, Rd=1, Rot=16, Imm=1F
            TestIn {
                regs: vec![0x95959595, 1],
                cpsr: None,
                instr: 0xE380181F
            },
            TestOut {
                regs: vec![None, Some(0x959F9595)],
                cpsr: Some(CPSR::default()),
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
                cpsr: Some(CPSR::N),
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
                cpsr: Some(CPSR::default()),
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
                cpsr: Some(CPSR::default()),
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
                cpsr: Some(CPSR::default()),
                cycles: None,
            }
        )
    ];

    for (in_data, out_data) in data.iter() {
        in_data.run_test(out_data);
    }
}