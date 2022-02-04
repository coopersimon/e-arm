// TODO: use test core instead of this one.
use crate::core::*;
use crate::core::armv5::*;
use crate::core::armv4::ARMv4;
use super::super::test_utils::*;

struct TestARM5Core {
    regs: [u32; 16],

    cpsr: CPSR,
    spsr: CPSR,

    memory: TestMem,
}

impl TestARM5Core {
    fn new() -> Self {
        Self {
            regs: [0; 16],

            cpsr: Default::default(),
            spsr: Default::default(),

            memory: TestMem::new(1024*64),
        }
    }
}

impl ARMCore<TestMem> for TestARM5Core {
    fn read_reg(&self, n: usize) -> u32 {
        self.regs[n]
    }
    fn write_reg(&mut self, n: usize, data: u32) {
        self.regs[n] = data;
    }

    fn do_branch(&mut self, dest: u32) {
        self.regs[15] = dest;
    }
    fn call_subroutine(&mut self, _dest: u32) {
        // TODO
    }

    fn clock(&mut self, _cycles: usize) {

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
    fn write_flags(&mut self, flags: CPSR) {
        self.cpsr = flags;
    }

    fn read_spsr(&self) -> CPSR {
        self.spsr
    }
    fn write_spsr(&mut self, data: CPSR) {
        self.spsr = data;
    }

    fn reset(&mut self) {

    }
    fn interrupt(&mut self) {

    }
    fn fast_interrupt(&mut self) {

    }
    fn software_exception(&mut self) {

    }
    fn undefined_exception(&mut self) {

    }
    fn return_from_exception(&mut self) {
        // TODO...
    }

    fn try_swi_hook(&mut self, _comment: u32) -> Option<usize> {
        None
    }

    fn next_fetch_non_seq(&mut self) {
        
    }

    fn ref_mem<'a>(&'a self) -> &'a TestMem {
        &self.memory
    }
    fn mut_mem<'a>(&'a mut self) -> &'a mut TestMem {
        &mut self.memory
    }

    fn mut_coproc<'a>(&'a mut self, _coproc: usize) -> Option<&'a mut dyn CoprocV4> {
        None
    }
}

impl ARMCoreV5 for TestARM5Core {
    fn mut_coproc_v5<'a>(&'a mut self, _coproc: usize) -> Option<&'a mut dyn CoprocV5> {
        None
    }
}

//impl ARMv4Decode<TestMem> for TestARM4Core {}
impl ARMv4<TestMem> for TestARM5Core {}
impl ARMv5<TestMem> for TestARM5Core {}

// Use to setup in state
#[derive(Default)]
struct TestIn {
    regs: Vec<u32>,
    cpsr: Option<CPSR>,
    instr: u32,
}

// Use to assert output state
#[derive(Default)]
struct TestOut {
    regs: Vec<Option<u32>>,
    cpsr: CPSR,
    cycles: usize,
}

impl TestIn {
    fn run_test(&self, test_num: usize, out: &TestOut) {
        let mut cpu = TestARM5Core::new();
        for (i, val) in self.regs.iter().enumerate() {
            cpu.regs[i] = *val;
        }
        if let Some(init_flags) = self.cpsr {
            cpu.cpsr = init_flags;
        }
        
        let instr = decode_arm(self.instr);
        let cycles = instr.execute(&mut cpu);

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

#[test]
fn test_clz() {
    let data = vec![
        (
            // CLZ R1, R0: Cond=AL, Rm=0, Rd=1
            TestIn {
                regs: vec![0x0000FFFF, 0],
                cpsr: None,
                instr: 0xE16F1F10
            },
            TestOut {
                regs: vec![Some(0x0000FFFF), Some(16)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // CLZ R1, R0: Cond=AL, Rm=0, Rd=1
            TestIn {
                regs: vec![0x12345678, 3],
                cpsr: None,
                instr: 0xE16F1F10
            },
            TestOut {
                regs: vec![Some(0x12345678), Some(3)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // CLZ R10, R0: Cond=AL, Rm=0, Rd=1
            TestIn {
                regs: vec![0x00010000],
                cpsr: None,
                instr: 0xE16FAF10
            },
            TestOut {
                regs: vec![Some(0x00010000), None, None, None, None, None, None, None, None, None, Some(15)],
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
fn test_qadd() {
    let data = vec![
        (
            // QADD R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0, 0x12345678, 0x12345678],
                cpsr: None,
                instr: 0xE1020051
            },
            TestOut {
                regs: vec![Some(0x2468ACF0), Some(0x12345678), Some(0x12345678)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // QADD R0, R0, R1: Cond=AL, Rn=1, Rd=0, Rm=0
            TestIn {
                regs: vec![0x7FFFFFF0, 0x12345678],
                cpsr: None,
                instr: 0xE1010050
            },
            TestOut {
                regs: vec![Some(0x7FFFFFFF), Some(0x12345678)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        ),
        (
            // QADD R0, R0, R0: Cond=AL, Rn=0, Rd=0, Rm=0
            TestIn {
                regs: vec![0x80001234, 0x12345678],
                cpsr: None,
                instr: 0xE1000050
            },
            TestOut {
                regs: vec![Some(0x80000000), Some(0x12345678)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        ),
        (
            // QADD R0, R0, R1: Cond=AL, Rn=1, Rd=0, Rm=0
            TestIn {
                regs: vec![0x7FFF0000, 0xFFFF],
                cpsr: Some(CPSR::Q),
                instr: 0xE1010050
            },
            TestOut {
                regs: vec![Some(0x7FFFFFFF), Some(0xFFFF)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        ),
        (
            // QADD R0, R0, R1: Cond=AL, Rn=1, Rd=0, Rm=0
            TestIn {
                regs: vec![0x7FFF0000, 0xFFFE],
                cpsr: Some(CPSR::Q),
                instr: 0xE1010050
            },
            TestOut {
                regs: vec![Some(0x7FFFFFFE), Some(0xFFFE)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        ),
        (
            // QADD R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0, 0x7FFFFFFF, 0],
                cpsr: None,
                instr: 0xE1020051
            },
            TestOut {
                regs: vec![Some(0x7FFFFFFF), Some(0x7FFFFFFF), Some(0)],
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
fn test_qsub() {
    let data = vec![
        (
            // QSUB R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0x1, 0x12345678, 0x12345678],
                cpsr: None,
                instr: 0xE1220051
            },
            TestOut {
                regs: vec![Some(0), Some(0x12345678), Some(0x12345678)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // QSUB R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0x1, 0x80000000, 0x1234],
                cpsr: None,
                instr: 0xE1220051
            },
            TestOut {
                regs: vec![Some(0x80000000), Some(0x80000000), Some(0x1234)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        ),
        (
            // QSUB R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0x1, 0x80000000, 0],
                cpsr: None,
                instr: 0xE1220051
            },
            TestOut {
                regs: vec![Some(0x80000000), Some(0x80000000), Some(0)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // QSUB R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0x1, 0x7FFFFFFF, 0xFFFFFFFF],
                cpsr: None,
                instr: 0xE1220051
            },
            TestOut {
                regs: vec![Some(0x7FFFFFFF), Some(0x7FFFFFFF), Some(0xFFFFFFFF)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        ),
        (
            // QSUB R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0x1, 0x7FFFFFFF, 0x7FFFFFF0],
                cpsr: Some(CPSR::Q),
                instr: 0xE1220051
            },
            TestOut {
                regs: vec![Some(0xF), Some(0x7FFFFFFF), Some(0x7FFFFFF0)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}

#[test]
fn test_qdadd() {
    let data = vec![
        (
            // QDADD R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0, 0x12345678, 0x12345678],
                cpsr: None,
                instr: 0xE1420051
            },
            TestOut {
                regs: vec![Some(0x369D0368), Some(0x12345678), Some(0x12345678)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // QDADD R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0, 0x12345678, 0xB0001234],
                cpsr: None,
                instr: 0xE1420051
            },
            TestOut {
                regs: vec![Some(0x92345678), Some(0x12345678), Some(0xB0001234)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        ),
        (
            // QDADD R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0, 0x12345678, 0x40000000],
                cpsr: None,
                instr: 0xE1420051
            },
            TestOut {
                regs: vec![Some(0x7FFFFFFF), Some(0x12345678), Some(0x40000000)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        ),
        (
            // QDADD R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0, 0xC000AAAA, 0xB0001234],
                cpsr: None,
                instr: 0xE1420051
            },
            TestOut {
                regs: vec![Some(0x80000000), Some(0xC000AAAA), Some(0xB0001234)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        ),
        (
            // QDADD R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0, 0xFFFFFFFF, 0xFFFFFFFF],
                cpsr: None,
                instr: 0xE1420051
            },
            TestOut {
                regs: vec![Some(0xFFFFFFFD), Some(0xFFFFFFFF), Some(0xFFFFFFFF)],
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
fn test_qdsub() {
    let data = vec![
        (
            // QDSUB R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0x1, 0x12345678, 0x12345678],
                cpsr: None,
                instr: 0xE1620051
            },
            TestOut {
                regs: vec![Some(0xEDCBA988), Some(0x12345678), Some(0x12345678)],
                cpsr: CPSR::default(),
                cycles: 0,
            }
        ),
        (
            // QDSUB R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0x1, 0x7FFFFFFF, 0x70000000],
                cpsr: None,
                instr: 0xE1620051
            },
            TestOut {
                regs: vec![Some(0), Some(0x7FFFFFFF), Some(0x70000000)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        ),
        (
            // QDSUB R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0x1, 0x7FFFFFFF, 0xC0000000],
                cpsr: None,
                instr: 0xE1620051
            },
            TestOut {
                regs: vec![Some(0x7FFFFFFF), Some(0x7FFFFFFF), Some(0xC0000000)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        ),
        (
            // QDSUB R0, R1, R2: Cond=AL, Rn=2, Rd=0, Rm=1
            TestIn {
                regs: vec![0x1, 0x7FFFFFFF, 0x60000000],
                cpsr: None,
                instr: 0xE1620051
            },
            TestOut {
                regs: vec![Some(0), Some(0x7FFFFFFF), Some(0x60000000)],
                cpsr: CPSR::Q,
                cycles: 0,
            }
        )
    ];

    for (i, (in_data, out_data)) in data.iter().enumerate() {
        in_data.run_test(i, out_data);
    }
}
