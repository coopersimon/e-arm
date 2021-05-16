
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
        let read_reg = wrap_read_reg::<M, T> as i64;

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
            // TODO: do this in a single call
            ; mov rbx, QWORD read_reg

            ; mov rsi, 0
            ; call rbx
            ; push rax

            ; mov rsi, 1
            ; call rbx
            ; push rax

            ; mov rsi, 2
            ; call rbx
            ; push rax

            ; mov rsi, 3
            ; call rbx
            ; push rax

            ; mov rsi, 4
            ; call rbx
            ; mov r12, rax

            ; mov rsi, 5
            ; call rbx
            ; mov r13, rax

            ; mov rsi, 6
            ; call rbx
            ; mov r14, rax

            ; mov rsi, 13
            ; call rbx
            ; mov r15, rax

            ; pop r11
            ; pop r10
            ; pop r9
            ; pop r8
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

    // Code-gen for an ALU operand.
    // Returns the type of operand for the final operation.
    fn alu_operand<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, op: &ALUOperand) -> DataOperand {
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
                            ; push r8
                            ; push r9
                            ; push r10
                            ; push r11
                            ; call rbx
                            ; pop r11
                            ; pop r10
                            ; pop r9
                            ; pop r8
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
            ; mov rdx, r8
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
            ; mov rdx, r12
            ; call rbx

            ; mov rsi, 5
            ; mov rdx, r13
            ; call rbx

            ; mov rsi, 6
            ; mov rdx, r14
            ; call rbx

            ; mov rsi, 13
            ; mov rdx, r15
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
        let op = self.alu_operand::<M, T>(op2);
        match op {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov Rq(dest_reg), DWORD i
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
            let write_reg = wrap_write_reg::<M, T> as i64;
            let reg = rd as i32;
            dynasm!(self.assembler
                ; .arch x64
                ; mov rbx, QWORD write_reg
                ; mov rsi, reg
                ; push r8
                ; push r9
                ; push r10
                ; push r11
                ; call rbx
                ; pop r11
                ; pop r10
                ; pop r9
                ; pop r8
            );
        }
    }

    fn add<M: Mem32<Addr = u32>, T: ARMCore<M>>(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        let dest = self.get_register(rd);
        let dest_reg = dest.unwrap_or(2); // 2 = RDX

        let mut op2 = self.alu_operand::<M, T>(op2);

        let op1 = self.get_register(rn);
        let op1_reg = op1.unwrap_or_else(|| {
            // If op2 is in RAX, we need to move it.
            if op2.mov_to_rdx() {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov rdx, rax
                );
            }
            let read_reg = wrap_read_reg::<M, T> as i64;
            let reg = rn as i32;
            dynasm!(self.assembler
                ; .arch x64
                ; mov rbx, QWORD read_reg
                ; mov rsi, reg
                ; push r8
                ; push r9
                ; push r10
                ; push r11
                ; call rbx
                ; pop r11
                ; pop r10
                ; pop r9
                ; pop r8
            );
            0   // 0 = RAX
        });

        if dest_reg != op1_reg {
            dynasm!(self.assembler
                ; .arch x64
                ; mov Rq(dest_reg), Rq(op1_reg)
            );
        }
        match op2 {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; add Rq(dest_reg), DWORD i
                );
            },
            DataOperand::Reg(r) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; add Rq(dest_reg), Rq(r)
                );
            }
        }

        // Unmapped dest: we need to write back.
        if dest.is_none() {
            let write_reg = wrap_write_reg::<M, T> as i64;
            let reg = rd as i32;
            dynasm!(self.assembler
                ; .arch x64
                ; mov rbx, QWORD write_reg
                ; mov rsi, reg
                ; push r8
                ; push r9
                ; push r10
                ; push r11
                ; call rbx
                ; pop r11
                ; pop r10
                ; pop r9
                ; pop r8
            );
        }
    }
}

// CPU wrappers
pub unsafe extern "Rust" fn wrap_call_subroutine<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, dest: u32) {
    cpu.as_mut().unwrap().call_subroutine(dest);
}

pub unsafe extern "Rust" fn wrap_read_reg<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, reg: usize) -> u32 {
    println!("Read reg {}", reg);
    cpu.as_mut().unwrap().read_reg(reg)
}

pub unsafe extern "Rust" fn wrap_write_reg<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, reg: usize, data: u32) {
    println!("Write {:X} to reg {}", data, reg);
    cpu.as_mut().unwrap().write_reg(reg, data);
}

pub unsafe extern "Rust" fn wrap_clock<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, cycles: usize) {
    // TODO: wrap this in a func...
    cpu.as_mut().unwrap().ref_mem_mut().clock(cycles);
}
