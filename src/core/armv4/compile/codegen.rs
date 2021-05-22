
use dynasmrt::{
    dynasm, DynasmApi, DynasmLabelApi, Assembler, DynamicLabel,
    x64::{
        X64Relocation
    }
};
use std::rc::Rc;
use std::marker::PhantomData;
use crate::{
    Mem32, ARMCore,
    core::{
        armv4::instructions::{
            ARMv4InstructionType,
            ALUOperand,
            ShiftOperand,
            RegShiftOperand
        },
        ARMCondition,
        JITObject
    }
};
use super::DecodedInstruction;

pub struct CodeGeneratorX64<M: Mem32<Addr = u32>, T: ARMCore<M>> {
    assembler: Assembler<X64Relocation>,
    label_table: std::collections::BTreeMap<usize, DynamicLabel>,

    _unused_m: PhantomData<M>,
    _unused_t: PhantomData<T>
}

impl<M: Mem32<Addr = u32>, T: ARMCore<M>> CodeGeneratorX64<M, T> {
    pub fn new() -> Self {
        Self {
            assembler: Assembler::new().unwrap(),
            label_table: std::collections::BTreeMap::new(),

            _unused_m: PhantomData,
            _unused_t: PhantomData
        }
    }

    pub fn prelude(&mut self) {
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
            ; mov rax, QWORD mut_regs
            ; call rax
            ; pop rdi

            ; mov r8d, [rax]
            ; mov r9d, [rax+4]
            ; mov r10d, [rax+8]
            ; mov r11d, [rax+12]
            ; mov r12d, [rax+16]
            ; mov r13d, [rax+20]
            ; mov r14d, [rax+24]
            ; mov r15d, [rax+52]

            // TODO: flags?
        );
    }

    pub fn finish(self) -> Rc<JITObject<T>> {
        let buf = self.assembler.finalize().unwrap();
        Rc::new(JITObject::new(buf))
    }

    pub fn codegen(&mut self, instruction: &DecodedInstruction) {
        if let Some(label_id) = instruction.label {
            let label = self.get_label(label_id);
            dynasm!(self.assembler
                ; .arch x64
                ; =>label
            );
        }

        let skip_label = self.codegen_cond(instruction.instruction.cond);

        if instruction.ret {
            self.ret();
        } else {
            match &instruction.instruction.instr {
                ARMv4InstructionType::MOV{ rd, op2, set_flags } => self.mov(*rd, op2, *set_flags),
                ARMv4InstructionType::MVN{ rd, op2, set_flags } => self.mvn(*rd, op2, *set_flags),
                ARMv4InstructionType::AND{ rd, rn, op2, set_flags } => self.and(*rd, *rn, op2, *set_flags),
                ARMv4InstructionType::EOR{ rd, rn, op2, set_flags } => self.eor(*rd, *rn, op2, *set_flags),
                ARMv4InstructionType::ORR{ rd, rn, op2, set_flags } => self.orr(*rd, *rn, op2, *set_flags),
                ARMv4InstructionType::BIC{ rd, rn, op2, set_flags } => self.bic(*rd, *rn, op2, *set_flags),

                ARMv4InstructionType::ADD{ rd, rn, op2, set_flags } => self.add(*rd, *rn, op2, *set_flags),
                ARMv4InstructionType::SUB{ rd, rn, op2, set_flags } => self.sub(*rd, *rn, op2, *set_flags),
                ARMv4InstructionType::RSB{ rd, rn, op2, set_flags } => self.rsb(*rd, *rn, op2, *set_flags),
                ARMv4InstructionType::ADC{ rd, rn, op2, set_flags } => self.adc(*rd, *rn, op2, *set_flags),
                ARMv4InstructionType::SBC{ rd, rn, op2, set_flags } => self.sbc(*rd, *rn, op2, *set_flags),
                ARMv4InstructionType::RSC{ rd, rn, op2, set_flags } => self.rsc(*rd, *rn, op2, *set_flags),

                ARMv4InstructionType::TST{ rn, op2 } => self.tst(*rn, op2),
                ARMv4InstructionType::TEQ{ rn, op2 } => self.teq(*rn, op2),
                ARMv4InstructionType::CMP{ rn, op2 } => self.cmp(*rn, op2),
                ARMv4InstructionType::CMN{ rn, op2 } => self.cmn(*rn, op2),

                _ => panic!("not supported"),
            }
        }

        if let Some(label) = skip_label {
            dynasm!(self.assembler
                ; .arch x64
                ; =>label
            );
        }
    }
}

enum DataOperand {
    Imm(i32),
    Reg(u8)
}

// Helpers
impl<M: Mem32<Addr = u32>, T: ARMCore<M>> CodeGeneratorX64<M, T> {
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

    /// Store the flags
    fn push_flags(&mut self) {
        dynasm!(self.assembler
            ; .arch x64
            ; pushf
        );
    }

    /// Restore the flags
    fn pop_flags(&mut self) {
        dynasm!(self.assembler
            ; .arch x64
            ; popf
        );
    }

    /// Generate condition branch.
    fn codegen_cond(&mut self, condition: ARMCondition) -> Option<DynamicLabel> {
        match condition {
            ARMCondition::AL => None,
            other => {
                let skip_label = self.assembler.new_dynamic_label();
                // Match to the inverse of the condition.
                // Then if the condition isn't met, we skip over it.
                match other {
                    ARMCondition::EQ => dynasm!(self.assembler
                        ; .arch x64
                        ; jne =>skip_label
                    ),
                    ARMCondition::NE => dynasm!(self.assembler
                        ; .arch x64
                        ; je =>skip_label
                    ),
                    ARMCondition::CS => dynasm!(self.assembler
                        ; .arch x64
                        ; jnc =>skip_label
                    ),
                    ARMCondition::CC => dynasm!(self.assembler
                        ; .arch x64
                        ; jc =>skip_label
                    ),
                    ARMCondition::MI => dynasm!(self.assembler
                        ; .arch x64
                        ; jns =>skip_label
                    ),
                    ARMCondition::PL => dynasm!(self.assembler
                        ; .arch x64
                        ; js =>skip_label
                    ),
                    ARMCondition::VS => dynasm!(self.assembler
                        ; .arch x64
                        ; jno =>skip_label
                    ),
                    ARMCondition::VC => dynasm!(self.assembler
                        ; .arch x64
                        ; jo =>skip_label
                    ),
                    // Not sure about this one...
                    ARMCondition::HI => dynasm!(self.assembler
                        ; .arch x64
                        ; jna =>skip_label
                    ),
                    // ...or this one
                    ARMCondition::LS => dynasm!(self.assembler
                        ; .arch x64
                        ; ja =>skip_label
                    ),
                    ARMCondition::GE => dynasm!(self.assembler
                        ; .arch x64
                        ; jl =>skip_label
                    ),
                    ARMCondition::LT => dynasm!(self.assembler
                        ; .arch x64
                        ; jge =>skip_label
                    ),
                    ARMCondition::GT => dynasm!(self.assembler
                        ; .arch x64
                        ; jle =>skip_label
                    ),
                    ARMCondition::LE => dynasm!(self.assembler
                        ; .arch x64
                        ; jg =>skip_label
                    ),
                    ARMCondition::AL => unreachable!(),
                }
                Some(skip_label)
            }
        }
    }

    /// Register mapping.
    /// If None is returned the register has to be fetched from memory.
    fn get_mapped_register(&self, reg: usize) -> Option<u8> {
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

    /// Get the register number to use.
    /// If it is mapped, it will return the mapped value.
    /// Otherwise it will generate code to load the register into EAX and return that.
    fn get_register(&mut self, reg: usize) -> u8 {
        let op1 = self.get_mapped_register(reg);
        op1.unwrap_or_else(|| {
            let read_reg = wrap_read_reg::<M, T> as i64;
            let reg_num = reg as i32;
            dynasm!(self.assembler
                ; .arch x64
                ; mov rax, QWORD read_reg
                ; mov rsi, reg_num
                //; push r8
                //; push r9
                //; push r10
                //; push r11
                ; pushf
                ; call rax
                ; popf
                //; pop r11
                //; pop r10
                //; pop r9
                //; pop r8
            );
            0   // 0 = EAX
        })
    }

    // Code-gen for an ALU operand.
    // Returns the type of operand for the operation.
    fn alu_operand_2(&mut self, op: &ALUOperand) -> DataOperand {
        match op {
            ALUOperand::Normal(op) => match op {
                ShiftOperand::Immediate(i) => DataOperand::Imm(*i as i32),
                ShiftOperand::Register(r) => DataOperand::Reg(self.get_register(*r)),
                ShiftOperand::LSL{shift_amount, reg} => {
                    let reg = self.get_register(*reg);
                    let shift_val = *shift_amount as i8;
                    if reg != 0 {
                        dynasm!(self.assembler
                            ; .arch x64
                            ; mov eax, Rd(reg)
                        );
                    }
                    dynasm!(self.assembler
                        ; .arch x64
                        ; shl eax, shift_val
                    );
                    DataOperand::Reg(0) // EAX
                },
                ShiftOperand::LSR{shift_amount, reg} => {
                    let reg = self.get_register(*reg);
                    let shift_val = *shift_amount as i8;
                    if reg != 0 {
                        dynasm!(self.assembler
                            ; .arch x64
                            ; mov eax, Rd(reg)
                        );
                    }
                    dynasm!(self.assembler
                        ; .arch x64
                        ; shr eax, shift_val
                    );
                    DataOperand::Reg(0) // EAX
                },
                ShiftOperand::ASR{shift_amount, reg} => {
                    let reg = self.get_register(*reg);
                    let shift_val = *shift_amount as i8;
                    if reg != 0 {
                        dynasm!(self.assembler
                            ; .arch x64
                            ; mov eax, Rd(reg)
                        );
                    }
                    dynasm!(self.assembler
                        ; .arch x64
                        ; sar eax, shift_val
                    );
                    DataOperand::Reg(0) // EAX
                },
                ShiftOperand::ROR{shift_amount, reg} => {
                    let reg = self.get_register(*reg);
                    let shift_val = *shift_amount as i8;
                    if reg != 0 {
                        dynasm!(self.assembler
                            ; .arch x64
                            ; mov eax, Rd(reg)
                        );
                    }
                    dynasm!(self.assembler
                        ; .arch x64
                        ; ror eax, shift_val
                    );
                    DataOperand::Reg(0) // EAX
                },
                ShiftOperand::LSR32{reg} => {
                    let reg = self.get_register(*reg);
                    if reg != 0 {
                        dynasm!(self.assembler
                            ; .arch x64
                            ; mov eax, Rd(reg)
                        );
                    }
                    dynasm!(self.assembler
                        ; .arch x64
                        ; shl eax, 1    // Fill carry
                        ; mov eax, 0
                    );
                    DataOperand::Reg(0) // EAX
                },
                ShiftOperand::ASR32{reg} => {
                    let reg = self.get_register(*reg);
                    if reg != 0 {
                        dynasm!(self.assembler
                            ; .arch x64
                            ; mov eax, Rd(reg)
                        );
                    }
                    dynasm!(self.assembler
                        ; .arch x64
                        ; sar eax, 31
                    );
                    DataOperand::Reg(0) // EAX
                },
                ShiftOperand::RRX{reg} => {
                    let reg = self.get_register(*reg);
                    if reg != 0 {
                        dynasm!(self.assembler
                            ; .arch x64
                            ; mov eax, Rd(reg)
                        );
                    }
                    dynasm!(self.assembler
                        ; .arch x64
                        ; rcr eax, 1
                    );
                    DataOperand::Reg(0) // EAX
                },
            },
            ALUOperand::RegShift{op, shift_reg, reg} => {
                let shift_reg = self.get_register(*shift_reg);
                if shift_reg == 0 {
                    // We need to use register A for the data register.
                    dynasm!(self.assembler
                        ; .arch x64
                        ; push rax
                    );
                }
                let data_reg = self.get_register(*reg);
                if data_reg != 0 {
                    dynasm!(self.assembler
                        ; .arch x64
                        ; mov eax, Rd(data_reg)
                    );
                }
                if shift_reg == 0 {
                    dynasm!(self.assembler
                        ; .arch x64
                        ; pop rcx
                    );
                } else {
                    dynasm!(self.assembler
                        ; .arch x64
                        ; mov ecx, Rd(shift_reg)
                    );
                }
                match op {
                    RegShiftOperand::LSL => {
                        dynasm!(self.assembler
                            ; .arch x64
                            ; shl eax, cl
                        );
                    },
                    RegShiftOperand::LSR => {
                        dynasm!(self.assembler
                            ; .arch x64
                            ; shr eax, cl
                        );
                    },
                    RegShiftOperand::ASR => {
                        dynasm!(self.assembler
                            ; .arch x64
                            ; sar eax, cl
                        );
                    },
                    RegShiftOperand::ROR => {
                        dynasm!(self.assembler
                            ; .arch x64
                            ; ror eax, cl
                        );
                    },
                }
                DataOperand::Reg(0) // EAX
            }
        }
    }

    // Write the value back to the dest register.
    // The value should be in EBX.
    fn writeback_dest(&mut self, rd: usize) {
        match self.get_mapped_register(rd) {
            Some(reg) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov Rd(reg), ebx
                );
            },
            None => self.writeback_unmapped_dest(rd),
        }
    }

    fn writeback_unmapped_dest(&mut self, rd: usize) {
        let write_reg = wrap_write_reg::<M, T> as i64;
        let reg = rd as i32;
        dynasm!(self.assembler
            ; .arch x64
            ; mov rax, QWORD write_reg
            ; mov rsi, reg
            ; mov edx, ebx
            //; push r8
            //; push r9
            //; push r10
            //; push r11
            ; pushf
            ; call rax
            ; popf
            //; pop r11
            //; pop r10
            //; pop r9
            //; pop r8
        );
    }

    // ALU operation.
    #[inline]
    fn alu_instr<F>(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool, op: F)
        where F: FnOnce(&mut Self, u8, DataOperand)
    {
        if !set_flags {
            self.push_flags();
        }
        let op1_reg = self.get_register(rn);
        // TODO: if rd == rn & op1_reg is mapped, we can use it directly instead of ebx
        // (This will help with thumb code)
        dynasm!(self.assembler
            ; .arch x64
            ; mov ebx, Rd(op1_reg)
        );

        let op2 = self.alu_operand_2(op2);

        op(self, 3, op2);

        self.writeback_dest(rd);
        if !set_flags {
            self.pop_flags();
        }
    }

    fn ret(&mut self) {
        println!("Gen return!");
        let mut_regs = wrap_mut_regs::<M, T> as i64;

        dynasm!(self.assembler
            ; .arch x64
            // TODO: write back flags?

            // Write back regs
            ; mov rax, QWORD mut_regs
            ; call rax

            ; mov [rax], r8d
            ; mov [rax+4], r9d
            ; mov [rax+8], r10d
            ; mov [rax+12], r11d
            ; mov [rax+16], r12d
            ; mov [rax+20], r13d
            ; mov [rax+24], r14d
            ; mov [rax+52], r15d

            // Restore
            ; pop r15
            ; pop r14
            ; pop r13
            ; pop r12
            ; pop rbx
            ; mov rsp, rbp
            ; pop rbp

            ; ret
        );
    }
}

// Instructions
impl<M: Mem32<Addr = u32>, T: ARMCore<M>> CodeGeneratorX64<M, T> {
    fn mov(&mut self, rd: usize, op2: &ALUOperand, set_flags: bool) {
        let dest = self.get_mapped_register(rd);
        let dest_reg = dest.unwrap_or(3); // 3 = EBX
        let op = self.alu_operand_2(op2);
        match op {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov Rd(dest_reg), WORD i
                );
            },
            DataOperand::Reg(r) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov Rd(dest_reg), Rd(r)
                );
            }
        }
        // x86 MOV doesn't set flags by itself
        if set_flags {
            dynasm!(self.assembler
                ; .arch x64
                ; test Rd(dest_reg), WORD -1
            );
        }
        // Unmapped dest: we need to write back.
        if dest.is_none() {
            self.writeback_unmapped_dest(rd);
        }
    }

    fn mvn(&mut self, rd: usize, op2: &ALUOperand, set_flags: bool) {
        let dest = self.get_mapped_register(rd);
        let dest_reg = dest.unwrap_or(3); // 3 = EBX
        let op = self.alu_operand_2(op2);
        match op {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov Rd(dest_reg), WORD i
                    ; not Rd(dest_reg)
                );
            },
            DataOperand::Reg(r) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov Rd(dest_reg), Rd(r)
                    ; not Rd(dest_reg)
                );
            }
        }
        // x86 MOV + NOT doesn't set flags by itself
        if set_flags {
            dynasm!(self.assembler
                ; .arch x64
                ; test Rd(dest_reg), WORD -1
            );
        }
        // Unmapped dest: we need to write back.
        if dest.is_none() {
            self.writeback_unmapped_dest(rd);
        }
    }

    fn and(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; and Rd(op1), WORD i
                    );
                },
                DataOperand::Reg(r) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; and Rd(op1), Rd(r)
                    );
                }
            }
        })
    }

    fn eor(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; xor Rd(op1), WORD i
                    );
                },
                DataOperand::Reg(r) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; xor Rd(op1), Rd(r)
                    );
                }
            }
        })
    }

    fn orr(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; or Rd(op1), WORD i
                    );
                },
                DataOperand::Reg(r) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; or Rd(op1), Rd(r)
                    );
                }
            }
        })
    }

    fn bic(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        if !set_flags {
            self.push_flags();
        }
        let dest = self.get_mapped_register(rd);
        let dest_reg = dest.unwrap_or(3); // 3 = EBX
        let op1_reg = self.get_register(rn);

        let op2 = self.alu_operand_2(op2);

        match op2 {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov eax, WORD i
                    ; andn Rd(dest_reg), eax, Rd(op1_reg)
                );
            },
            DataOperand::Reg(r) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; andn Rd(dest_reg), Rd(r), Rd(op1_reg)
                );
            }
        }

        // Unmapped dest: we need to write back.
        if dest.is_none() {
            self.writeback_unmapped_dest(rd);
        }
        if !set_flags {
            self.pop_flags();
        }
    }

    fn add(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; add Rd(op1), WORD i
                    );
                },
                DataOperand::Reg(r) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; add Rd(op1), Rd(r)
                    );
                }
            }
        })
    }

    fn adc(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; adc Rd(op1), WORD i
                    );
                },
                DataOperand::Reg(r) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; adc Rd(op1), Rd(r)
                    );
                }
            }
        })
    }

    fn sub(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; sub Rd(op1), WORD i
                    );
                },
                DataOperand::Reg(r) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; sub Rd(op1), Rd(r)
                    );
                }
            }
        })
    }

    fn sbc(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; sbb Rd(op1), WORD i
                    );
                },
                DataOperand::Reg(r) => {
                    dynasm!(c.assembler
                        ; .arch x64
                        ; sbb Rd(op1), Rd(r)
                    );
                }
            }
        })
    }

    fn rsb(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        if !set_flags {
            self.push_flags();
        }
        let op2 = self.alu_operand_2(op2);
        match op2 {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov ebx, WORD i
                );
            },
            DataOperand::Reg(r) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov ebx, Rd(r)
                );
            }
        }

        let op1_reg = self.get_register(rn);

        dynasm!(self.assembler
            ; .arch x64
            ; sub ebx, Rd(op1_reg)
        );

        self.writeback_dest(rd);
        if !set_flags {
            self.pop_flags();
        }
    }

    fn rsc(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        if !set_flags {
            self.push_flags();
        }
        let op2 = self.alu_operand_2(op2);
        match op2 {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov ebx, WORD i
                );
            },
            DataOperand::Reg(r) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov ebx, Rd(r)
                );
            }
        }

        let op1_reg = self.get_register(rn);

        dynasm!(self.assembler
            ; .arch x64
            ; sbb ebx, Rd(op1_reg)
        );

        self.writeback_dest(rd);
        if !set_flags {
            self.pop_flags();
        }
    }

    fn tst(&mut self, rn: usize, op2: &ALUOperand) {
        let op1_reg = self.get_register(rn);
        let op2 = self.alu_operand_2(op2);
        match op2 {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; test Rd(op1_reg), WORD i
                );
            },
            DataOperand::Reg(r) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; test Rd(op1_reg), Rd(r)
                );
            }
        }
    }

    fn teq(&mut self, rn: usize, op2: &ALUOperand) {
        let op1_reg = self.get_register(rn);
        dynasm!(self.assembler
            ; .arch x64
            ; mov ebx, Rd(op1_reg)
        );
        let op2 = self.alu_operand_2(op2);
        match op2 {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; xor ebx, WORD i
                );
            },
            DataOperand::Reg(r) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; xor ebx, Rd(r)
                );
            }
        }
    }

    fn cmp(&mut self, rn: usize, op2: &ALUOperand) {
        let op1_reg = self.get_register(rn);
        let op2 = self.alu_operand_2(op2);
        match op2 {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; cmp Rd(op1_reg), WORD i
                );
            },
            DataOperand::Reg(r) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; cmp Rd(op1_reg), Rd(r)
                );
            }
        }
    }

    fn cmn(&mut self, rn: usize, op2: &ALUOperand) {
        let op1_reg = self.get_register(rn);
        dynasm!(self.assembler
            ; .arch x64
            ; mov ebx, Rd(op1_reg)
        );
        let op2 = self.alu_operand_2(op2);
        match op2 {
            DataOperand::Imm(i) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; add ebx, WORD i
                );
            },
            DataOperand::Reg(r) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; add ebx, Rd(r)
                );
            }
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
