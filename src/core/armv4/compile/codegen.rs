
use dynasmrt::{
    dynasm, DynasmApi, DynasmLabelApi, Assembler, DynamicLabel,
    x64::{
        X64Relocation, Rq
    }
};
use std::rc::Rc;
use std::marker::PhantomData;
use crate::{
    Mem32, ARMCore, ARMv4Instruction,
    core::{
        armv4::instructions::{
            ARMv4InstructionType,
            ALUOperand,
            ShiftOperand
        },
        JITObject
    }
};

/// An instruction which is decoded, with meta-data, ready for code generation.
pub struct DecodedInstruction {
    /// The instruction itself.
    pub instruction: ARMv4Instruction,
    /// Number of cycles needed to fetch the instruction.
    pub fetch_cycles: usize,
    /// If this instruction needs a label emitted before it.
    pub label: Option<usize>,
    /// If this instruction branches somewhere.
    pub branch_to: Option<usize>,
    /// If this instruction should be treated as a return.
    pub ret: bool,
}

pub struct CodeGeneratorX64 {
    assembler: Assembler<X64Relocation>,
    label_table: std::collections::BTreeMap<usize, DynamicLabel>,
}

impl CodeGeneratorX64 {
    pub fn new() -> Self {
        Self {
            assembler: Assembler::new().unwrap(),
            label_table: std::collections::BTreeMap::new(),
        }
    }

    pub fn prelude<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self) {
        let mut_regs = wrap_mut_regs::<M, T> as i64;

        dynasm!(self.assembler
            ; .arch x64

            // Entry point.
            // rdi = CPU
            ; push rbp
            ; mov rbp, rsp
            ; push rbx
            ; push r12
            ; push r13
            ; push r14
            ; push r15

            // Write regs
            ; push rdi
            ; mov rbx, QWORD mut_regs
            ; call rbx
            ; pop rdi

            ; mov r8d, [rax]
            ; mov r9d, [rax+4]
            ; mov r10d, [rax+8]
            ; mov r11d, [rax+12]
            ; mov r12d, [rax+16]
            ; mov r13d, [rax+20]
            ; mov r14d, [rax+24]
            ; mov r15d, [rax+52]
        );
    }

    pub fn finish(self) -> Rc<JITObject> {
        let buf = self.assembler.finalize().unwrap();
        Rc::new(JITObject::new(buf))
    }

    pub fn codegen<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, instruction: &DecodedInstruction) {
        if let Some(label_id) = instruction.label {
            let label = self.get_label(label_id);
            dynasm!(self.assembler
                ; .arch x64
                ; =>label
            );
        }

        // TODO: conditional...

        if instruction.ret {
            self.ret::<M, T>();
        } else {
            match &instruction.instruction.instr {
                ARMv4InstructionType::MOV{ rd, op2, set_flags } => self.mov::<M, T>(*rd, op2, *set_flags),
                ARMv4InstructionType::ADD{ rd, rn, op2, set_flags } => self.add::<M, T>(*rd, *rn, op2, *set_flags),
                _ => panic!("not supported"),
            }
        }

        // TODO: conditional end
    }
}

pub unsafe extern "Rust" fn hello_world<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T) {
    println!("Hello world!");
    //cpu.as_mut().unwrap().write_reg(reg, data);
}


enum DataOperand {
    Imm(i32),
    Reg(u8)
}

impl DataOperand {
    // If operand is in RAX, move into RDX and return true.
    fn mov_to_rdx(&mut self) -> bool {
        match self {
            DataOperand::Reg(0) => {
                *self = DataOperand::Reg(2);
                true
            },
            _ => false
        }
    }
}

// Helpers
impl CodeGeneratorX64 {
    /// Dynamic label lookup
    fn get_label(&mut self, id: usize) -> DynamicLabel {
        if let Some(label) = self.label_table.get(&id) {
            *label
        } else {
            let label = self.assembler.new_dynamic_label();
            self.label_table.insert(id, label);
            label
        }
    }

    /// Register mapping.
    /// If None is returned the register has to be fetched from memory.
    fn get_register(&self, reg: usize) -> Option<u8> {
        match reg {
            0 => Some(8),
            1 => Some(9),
            2 => Some(10),
            3 => Some(11),
            4 => Some(12),
            5 => Some(13),
            6 => Some(14),
            13 => Some(15),
            _ => None
        }
    }

    // Code gen for first operand of ALU instructions (always a register).
    fn alu_operand_1<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, rn: usize, op2: &mut DataOperand) -> u8 {
        let op1 = self.get_register(rn);
        op1.unwrap_or_else(|| {
            // If op2 is in RAX, we need to move it.
            if op2.mov_to_rdx() {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov edx, eax
                );
            }
            let read_reg = wrap_read_reg::<M, T> as i64;
            let reg = rn as i32;
            dynasm!(self.assembler
                ; .arch x64
                ; mov rbx, QWORD read_reg
                ; mov rsi, reg
                //; push r8
                //; push r9
                //; push r10
                //; push r11
                ; call rbx
                //; pop r11
                //; pop r10
                //; pop r9
                //; pop r8
            );
            0   // 0 = RAX
        })
    }

    // Code-gen for an ALU operand.
    // Returns the type of operand for the operation.
    fn alu_operand_2<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, op: &ALUOperand) -> DataOperand {
        match op {
            ALUOperand::Normal(op) => match op {
                ShiftOperand::Immediate(i) => DataOperand::Imm(*i as i32),
                ShiftOperand::Register(r) => match self.get_register(*r) {
                    Some(reg) => DataOperand::Reg(reg),
                    None => {
                        let read_reg = wrap_read_reg::<M, T> as i64;
                        let reg = *r as i32;
                        dynasm!(self.assembler
                            ; .arch x64
                            ; mov rbx, QWORD read_reg
                            ; mov rsi, reg
                            //; push r8
                            //; push r9
                            //; push r10
                            //; push r11
                            ; call rbx
                            //; pop r11
                            //; pop r10
                            //; pop r9
                            //; pop r8
                        );
                        // RAX
                        DataOperand::Reg(0)
                    }
                },
                _ => panic!("{} not supported yet", op),
            },
            ALUOperand::RegShift{op, shift_reg, reg} => panic!("{} not supported yet", op),
        }
    }

    // If the dest register is unmapped, this should be called.
    fn writeback_dest<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, rd: usize) {
        let write_reg = wrap_write_reg::<M, T> as i64;
        let reg = rd as i32;
        dynasm!(self.assembler
            ; .arch x64
            ; mov rbx, QWORD write_reg
            ; mov rsi, reg
            //; push r8
            //; push r9
            //; push r10
            //; push r11
            ; call rbx
            //; pop r11
            //; pop r10
            //; pop r9
            //; pop r8
        );
    }

    fn ret<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self) {
        println!("Gen return!");
        let write_reg = wrap_write_reg::<M, T> as i64;

        dynasm!(self.assembler
            ; .arch x64
            // Write back regs
            ; mov rbx, QWORD write_reg

            ; push r11
            ; push r10
            ; push r9

            ; mov rsi, 0
            ; mov edx, r8d
            ; call rbx

            ; mov rsi, 1
            ; pop rdx
            ; call rbx

            ; mov rsi, 2
            ; pop rdx
            ; call rbx

            ; mov rsi, 3
            ; pop rdx
            ; call rbx

            ; mov rsi, 4
            ; mov edx, r12d
            ; call rbx

            ; mov rsi, 5
            ; mov edx, r13d
            ; call rbx

            ; mov rsi, 6
            ; mov edx, r14d
            ; call rbx

            ; mov rsi, 13
            ; mov edx, r15d
            ; call rbx

            // Restore
            ; pop r15
            ; pop r14
            ; pop r13
            ; pop r12
            ; mov rsp, rbp
            ; pop rbp

            ; ret
        );
    }
}

// Instructions
impl CodeGeneratorX64 {
    fn mov<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, rd: usize, op2: &ALUOperand, set_flags: bool) {
        let dest = self.get_register(rd);
        let dest_reg = dest.unwrap_or(2); // 2 = RDX
        let op = self.alu_operand_2::<M, T>(op2);
        match op {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov Rq(dest_reg), WORD i
                );
            },
            DataOperand::Reg(r) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov Rq(dest_reg), Rq(r)
                );
            }
        }
        // Unmapped dest: we need to write back.
        if dest.is_none() {
            self.writeback_dest::<M, T>(rd);
        }
    }

    fn add<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        let dest = self.get_register(rd);
        let dest_reg = dest.unwrap_or(2); // 2 = RDX

        let mut op2 = self.alu_operand_2::<M, T>(op2);
        let op1_reg = self.alu_operand_1::<M, T>(rn, &mut op2);

        if dest_reg != op1_reg {
            dynasm!(self.assembler
                ; .arch x64
                ; mov Rd(dest_reg), Rd(op1_reg)
            );
        }
        match op2 {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; add Rd(dest_reg), WORD i
                );
            },
            DataOperand::Reg(r) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; add Rd(dest_reg), Rd(r)
                );
            }
        }

        // Unmapped dest: we need to write back.
        if dest.is_none() {
            self.writeback_dest::<M, T>(rd);
        }
    }
}

// CPU wrappers
pub unsafe extern "Rust" fn wrap_call_subroutine<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, dest: u32) {
    cpu.as_mut().unwrap().call_subroutine(dest);
}

pub unsafe extern "Rust" fn wrap_read_reg<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, reg: usize) -> u32 {
    //println!("Read reg {}", reg);
    cpu.as_mut().unwrap().read_reg(reg)
}

pub unsafe extern "Rust" fn wrap_write_reg<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, reg: usize, data: u32) {
    //println!("Write {:X} to reg {}", data, reg);
    cpu.as_mut().unwrap().write_reg(reg, data);
}

pub unsafe extern "Rust" fn wrap_mut_regs<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T) -> *mut u32 {
    cpu.as_mut().unwrap().mut_regs().as_mut_ptr()
}

pub unsafe extern "Rust" fn wrap_clock<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, cycles: usize) {
    // TODO: wrap this in a func...
    cpu.as_mut().unwrap().ref_mem_mut().clock(cycles);
}
