
use dynasmrt::{
    dynasm, DynasmApi, DynasmLabelApi, Assembler, DynamicLabel,
    x64::{
        X64Relocation
    }
};
use std::rc::Rc;
use std::marker::PhantomData;
use crate::{
    Mem32, ARMCore, MemCycleType,
    core::{
        armv4::instructions::{
            ARMv4InstructionType,
            ALUOperand,
            ShiftOperand,
            RegShiftOperand,
            TransferParams,
            OpData
        },
        ARMCondition,
        JITObject
    },
    common::u32
};
use super::DecodedInstruction;

const EAX: u8 = 0;
const ECX: u8 = 1;
const EDX: u8 = 2;
const EBX: u8 = 3;

pub struct CodeGeneratorX64<M: Mem32<Addr = u32>, T: ARMCore<M>> {
    assembler: Assembler<X64Relocation>,
    label_table: std::collections::BTreeMap<usize, DynamicLabel>,

    current_pc: u32,
    current_cycles: usize,

    _unused_m: PhantomData<M>,
    _unused_t: PhantomData<T>
}

impl<M: Mem32<Addr = u32>, T: ARMCore<M>> CodeGeneratorX64<M, T> {
    pub fn new() -> Self {
        Self {
            assembler: Assembler::new().unwrap(),
            label_table: std::collections::BTreeMap::new(),

            current_pc: 0,
            current_cycles: 0,

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
            ; sub rsp, 16    // Space for regs, cycles
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

            ; mov [rbp-8], rax          // Put reg ptr on stack
            ; mov QWORD [rbp-16], 0     // Cycles = 0

            // TODO: flags?
        );
    }

    pub fn finish(self) -> Rc<JITObject<T>> {
        let buf = self.assembler.finalize().unwrap();
        Rc::new(JITObject::new(buf))
    }

    pub fn codegen(&mut self, instruction: &DecodedInstruction, pc_val: u32) {
        self.current_pc = pc_val;

        if instruction.clock {
            let clock = wrap_clock::<M, T> as i64;
            dynasm!(self.assembler
                ; .arch x64
                ; mov rdi, [rbp-16]
            );
            self.call(clock);
            self.reset_cycles();
        }
        self.current_cycles += instruction.cycles;

        if let Some(label_id) = instruction.label {
            let label = self.get_label(label_id);
            dynasm!(self.assembler
                ; .arch x64
                ; =>label
            );
        }

        // Handle internal branch.
        if let Some(label_id) = instruction.branch_to {
            let label = self.get_label(label_id);
            self.b(instruction.instruction.cond, label);
            return;
        }

        let skip_label = self.codegen_cond(instruction.instruction.cond);

        match &instruction.instruction.instr {
            ARMv4InstructionType::SWI{comment} => unimplemented!("SWI not implemented yet"),
            ARMv4InstructionType::UND => unimplemented!("UND not permitted for JITting"),

            ARMv4InstructionType::MOV{rd, op2, set_flags} => self.mov(*rd, op2, *set_flags),
            ARMv4InstructionType::MVN{rd, op2, set_flags} => self.mvn(*rd, op2, *set_flags),
            ARMv4InstructionType::AND{rd, rn, op2, set_flags} => self.and(*rd, *rn, op2, *set_flags),
            ARMv4InstructionType::EOR{rd, rn, op2, set_flags} => self.eor(*rd, *rn, op2, *set_flags),
            ARMv4InstructionType::ORR{rd, rn, op2, set_flags} => self.orr(*rd, *rn, op2, *set_flags),
            ARMv4InstructionType::BIC{rd, rn, op2, set_flags} => self.bic(*rd, *rn, op2, *set_flags),

            ARMv4InstructionType::ADD{rd, rn, op2, set_flags} => self.add(*rd, *rn, op2, *set_flags),
            ARMv4InstructionType::SUB{rd, rn, op2, set_flags} => self.sub(*rd, *rn, op2, *set_flags),
            ARMv4InstructionType::RSB{rd, rn, op2, set_flags} => self.rsb(*rd, *rn, op2, *set_flags),
            ARMv4InstructionType::ADC{rd, rn, op2, set_flags} => self.adc(*rd, *rn, op2, *set_flags),
            ARMv4InstructionType::SBC{rd, rn, op2, set_flags} => self.sbc(*rd, *rn, op2, *set_flags),
            ARMv4InstructionType::RSC{rd, rn, op2, set_flags} => self.rsc(*rd, *rn, op2, *set_flags),

            ARMv4InstructionType::TST{rn, op2} => self.tst(*rn, op2),
            ARMv4InstructionType::TEQ{rn, op2} => self.teq(*rn, op2),
            ARMv4InstructionType::CMP{rn, op2} => self.cmp(*rn, op2),
            ARMv4InstructionType::CMN{rn, op2} => self.cmn(*rn, op2),

            ARMv4InstructionType::TADDPC{rd, op2} => self.taddpc(*rd, *op2),

            ARMv4InstructionType::B{..} => unreachable!("this should have been generated earlier..."),
            ARMv4InstructionType::TB{..} => unreachable!("this should have been generated earlier..."),
            ARMv4InstructionType::BX{..} => unreachable!("this should only occur as a return..."),
            ARMv4InstructionType::BL{offset} => self.bl(*offset),
            ARMv4InstructionType::TBLLO{offset} => panic!("TODO: tbllo..."),
            ARMv4InstructionType::TBLHI{offset} => self.bl(*offset),

            ARMv4InstructionType::MUL{set_flags, rd, rs, rm} => self.mul(*rd, *rs, *rm, *set_flags),
            ARMv4InstructionType::MLA{set_flags, rd, rn, rs, rm} => self.mla(*rd, *rn, *rs, *rm, *set_flags),
            ARMv4InstructionType::UMULL{set_flags, rd_hi, rd_lo, rs, rm} => self.umull(*rd_hi, *rd_lo, *rs, *rm, *set_flags),
            ARMv4InstructionType::UMLAL{set_flags, rd_hi, rd_lo, rs, rm} => self.umlal(*rd_hi, *rd_lo, *rs, *rm, *set_flags),
            ARMv4InstructionType::SMULL{set_flags, rd_hi, rd_lo, rs, rm} => self.smull(*rd_hi, *rd_lo, *rs, *rm, *set_flags),
            ARMv4InstructionType::SMLAL{set_flags, rd_hi, rd_lo, rs, rm} => self.smlal(*rd_hi, *rd_lo, *rs, *rm, *set_flags),

            ARMv4InstructionType::SWP{rn, rd, rm} => self.swp(*rn, *rd, *rm, false),
            ARMv4InstructionType::SWPB{rn, rd, rm} => self.swp(*rn, *rd, *rm, true),

            ARMv4InstructionType::TLDRPC{data_reg, offset} => self.tldrpc(*data_reg, *offset),
            ARMv4InstructionType::LDR{transfer_params, data_reg, offset} => self.ldr(transfer_params, *data_reg, offset, false),
            ARMv4InstructionType::STR{transfer_params, data_reg, offset} => self.str(transfer_params, *data_reg, offset, false),
            ARMv4InstructionType::LDRB{transfer_params, data_reg, offset} => self.ldr(transfer_params, *data_reg, offset, true),
            ARMv4InstructionType::STRB{transfer_params, data_reg, offset} => self.str(transfer_params, *data_reg, offset, true),
            ARMv4InstructionType::LDRH{transfer_params, data_reg, offset} => self.ldrh(transfer_params, *data_reg, offset),
            ARMv4InstructionType::LDRSB{transfer_params, data_reg, offset} => self.ldrsb(transfer_params, *data_reg, offset),
            ARMv4InstructionType::LDRSH{transfer_params, data_reg, offset} => self.ldrsh(transfer_params, *data_reg, offset),
            ARMv4InstructionType::STRH{transfer_params, data_reg, offset} => self.strh(transfer_params, *data_reg, offset),

            ARMv4InstructionType::LDM{transfer_params, reg_list, ..} => self.ldm(transfer_params, *reg_list),
            ARMv4InstructionType::STM{transfer_params, reg_list, ..} => self.stm(transfer_params, *reg_list),

            ARMv4InstructionType::MRC{..} |
            ARMv4InstructionType::MCR{..} |
            ARMv4InstructionType::CDP{..} |
            ARMv4InstructionType::LDC{..} |
            ARMv4InstructionType::STC{..} => unimplemented!("coprocessor commands cannot be compiled"),

            ARMv4InstructionType::MSR{..} |
            ARMv4InstructionType::MRS{..} => unreachable!("MSR and MRS are not permitted for JITting"),
        }
        if instruction.ret {
            self.ret();
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

    /// Call a subroutine
    /// WARNING: Will destroy the value in RCX!
    /// Arguments in via rdi, rsi, rdx
    fn call(&mut self, subroutine: i64) {
        dynasm!(self.assembler
            ; .arch x64
            ; mov rcx, QWORD subroutine

            ; push rdi
            ; push r8
            ; push r9
            ; push r10
            ; push r11

            ; call rcx

            ; pop r11
            ; pop r10
            ; pop r9
            ; pop r8
            ; pop rdi
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
    /// Otherwise it will generate code to load the register into the specified register and return that.
    /// 
    /// WARNING: this will destroy the value in RCX!
    fn get_register(&mut self, reg: usize, unmapped: u8) -> u8 {
        let op1 = self.get_mapped_register(reg);
        op1.unwrap_or_else(|| {
            let reg_offset = (reg * 4) as i32;
            dynasm!(self.assembler
                ; .arch x64
                ; mov rcx, [rbp-8]
                ; mov Rd(unmapped), [rcx+reg_offset]
            );
            unmapped
        })
    }

    /// Get the first register for an ALU operation.
    /// If the reg is the PC, it will generate code to write the immediate value into
    /// the specified register.
    fn alu_operand_1(&mut self, reg: usize, unmapped: u8) -> u8 {
        if reg == 15 {
            let pc_const = self.current_pc as i32;
            dynasm!(self.assembler
                ; .arch x64
                ; mov Rd(unmapped), DWORD pc_const
            );
            unmapped
        } else {
            self.get_register(reg, unmapped)
        }
    }

    /// Code-gen for an ALU operand.
    /// Returns the type of operand for the operation.
    fn alu_operand_2(&mut self, op: &ALUOperand) -> DataOperand {
        match op {
            ALUOperand::Normal(op) => self.shift_op(op),
            ALUOperand::RegShift{op, shift_reg, reg} => {
                let data_reg = self.get_register(*reg, EAX);
                if data_reg != EAX {
                    dynasm!(self.assembler
                        ; .arch x64
                        ; mov eax, Rd(data_reg)
                    );
                }
                let shift_reg = self.get_register(*shift_reg, ECX);
                // The shift needs to be in register C.
                if shift_reg != ECX {
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
                DataOperand::Reg(EAX)
            }
        }
    }

    /// Code gen for a shift operand.
    /// Used for 2nd ALU ops and memory offsets.
    fn shift_op(&mut self, op: &ShiftOperand) -> DataOperand {
        match op {
            ShiftOperand::Immediate(i) => DataOperand::Imm(*i as i32),
            ShiftOperand::Register(15) => DataOperand::Imm(self.current_pc as i32),
            ShiftOperand::Register(r) => DataOperand::Reg(self.get_register(*r, EAX)),
            ShiftOperand::LSL{shift_amount, reg} => {
                let reg = self.get_register(*reg, EAX);
                let shift_val = *shift_amount as i8;
                if reg != EAX {
                    dynasm!(self.assembler
                        ; .arch x64
                        ; mov eax, Rd(reg)
                    );
                }
                dynasm!(self.assembler
                    ; .arch x64
                    ; shl eax, shift_val
                );
                DataOperand::Reg(EAX)
            },
            ShiftOperand::LSR{shift_amount, reg} => {
                let reg = self.get_register(*reg, EAX);
                let shift_val = *shift_amount as i8;
                if reg != EAX {
                    dynasm!(self.assembler
                        ; .arch x64
                        ; mov eax, Rd(reg)
                    );
                }
                dynasm!(self.assembler
                    ; .arch x64
                    ; shr eax, shift_val
                );
                DataOperand::Reg(EAX)
            },
            ShiftOperand::ASR{shift_amount, reg} => {
                let reg = self.get_register(*reg, EAX);
                let shift_val = *shift_amount as i8;
                if reg != EAX {
                    dynasm!(self.assembler
                        ; .arch x64
                        ; mov eax, Rd(reg)
                    );
                }
                dynasm!(self.assembler
                    ; .arch x64
                    ; sar eax, shift_val
                );
                DataOperand::Reg(EAX)
            },
            ShiftOperand::ROR{shift_amount, reg} => {
                let reg = self.get_register(*reg, EAX);
                let shift_val = *shift_amount as i8;
                if reg != EAX {
                    dynasm!(self.assembler
                        ; .arch x64
                        ; mov eax, Rd(reg)
                    );
                }
                dynasm!(self.assembler
                    ; .arch x64
                    ; ror eax, shift_val
                );
                DataOperand::Reg(EAX)
            },
            ShiftOperand::LSR32{reg} => {
                let reg = self.get_register(*reg, EAX);
                if reg != EAX {
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
                DataOperand::Reg(EAX)
            },
            ShiftOperand::ASR32{reg} => {
                let reg = self.get_register(*reg, EAX);
                if reg != EAX {
                    dynasm!(self.assembler
                        ; .arch x64
                        ; mov eax, Rd(reg)
                    );
                }
                dynasm!(self.assembler
                    ; .arch x64
                    ; sar eax, 31
                );
                DataOperand::Reg(EAX)
            },
            ShiftOperand::RRX{reg} => {
                let reg = self.get_register(*reg, EAX);
                if reg != EAX {
                    dynasm!(self.assembler
                        ; .arch x64
                        ; mov eax, Rd(reg)
                    );
                }
                dynasm!(self.assembler
                    ; .arch x64
                    ; rcr eax, 1
                );
                DataOperand::Reg(EAX)
            },
        }
    }

    /// Convert OpData to DataOperand.
    fn data_op(&mut self, op: &OpData) -> DataOperand {
        match op {
            OpData::Immediate(i) => DataOperand::Imm(*i as i32),
            OpData::Register(r) => DataOperand::Reg(self.get_register(*r, EAX)),
        }
    }

    /// Get the base reg for a load/store.
    /// 
    /// Puts it into "into" so that it can be offset.
    fn base_reg(&mut self, base_reg: usize, into: u8) {
        if base_reg == 15 {
            let pc_const = self.current_pc as i32;
            dynasm!(self.assembler
                ; .arch x64
                ; mov Rd(into), DWORD pc_const
            );
        } else {
            let real_reg = self.get_register(base_reg, EBX);
            if real_reg != into {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov Rd(into), Rd(real_reg)
                );
            }
        }
    }

    /// Calculate the address +/- offset for a load or store (word or byte).
    fn addr_offset(&mut self, base_reg: u8, inc: bool, offset: DataOperand) {
        if inc {
            match offset {
                DataOperand::Imm(0) => {},
                DataOperand::Imm(i) => dynasm!(self.assembler
                    ; .arch x64
                    ; add Rd(base_reg), DWORD i
                ),
                DataOperand::Reg(r) => dynasm!(self.assembler
                    ; .arch x64
                    ; add Rd(base_reg), Rd(r)
                ),
            }
        } else {
            match offset {
                DataOperand::Imm(0) => {},
                DataOperand::Imm(i) => dynasm!(self.assembler
                    ; .arch x64
                    ; sub Rd(base_reg), DWORD i
                ),
                DataOperand::Reg(r) => dynasm!(self.assembler
                    ; .arch x64
                    ; sub Rd(base_reg), Rd(r)
                ),
            }
        }
    }

    /// Write the value back to the dest register.
    /// Rd = Virtual ARM register
    /// From = x86 register
    fn writeback_dest(&mut self, rd: usize, from: u8) {
        match self.get_mapped_register(rd) {
            Some(reg) => {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov Rd(reg), Rd(from)
                );
            },
            None => self.writeback_unmapped_dest(rd, from),
        }
    }

    /// Writeback an unmapped dest register to memory.
    /// 
    /// WARNING: this will destroy the value in RCX!
    fn writeback_unmapped_dest(&mut self, rd: usize, from: u8) {
        let reg_offset = (rd * 4) as i32;
        dynasm!(self.assembler
            ; .arch x64
            ; mov rcx, [rbp-8]
            ; mov [rcx+reg_offset], Rd(from)
        );
    }

    /// Do an ALU operation.
    #[inline]
    fn alu_instr<F>(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool, op: F)
        where F: FnOnce(&mut Self, u8, DataOperand)
    {
        if !set_flags {
            self.push_flags();
        }
        let op1_reg = self.alu_operand_1(rn, EBX);
        let dest_reg = if rd == rn || op1_reg == EBX {
            // We can use the register value directly.
            op1_reg
        } else {
            dynasm!(self.assembler
                ; .arch x64
                ; mov ebx, Rd(op1_reg)
            );
            EBX
        };

        let op2 = self.alu_operand_2(op2);

        op(self, dest_reg, op2);

        if dest_reg == EBX {
            self.writeback_dest(rd, EBX);
        }
        if !set_flags {
            self.pop_flags();
        }
    }

    /// Do a load halfword, or load signed byte / halfword.
    #[inline]
    fn ext_ldr<F>(&mut self, transfer_params: &TransferParams, data_reg: usize, offset: &OpData, load_method: i64, extend: F)
        where F: FnOnce(&mut Self)
    {
        self.push_flags();

        self.base_reg(transfer_params.base_reg, EBX);

        if transfer_params.pre_index {
            let offset = self.data_op(offset);
            self.addr_offset(EBX, transfer_params.inc, offset);
        }

        dynasm!(self.assembler
            ; .arch x64
            ; mov esi, ebx
        );
        self.call(load_method);

        extend(self);

        self.writeback_dest(data_reg, EAX);
        self.add_dyn_cycles(EDX);
        
        if !transfer_params.pre_index {
            let offset = self.data_op(offset);
            self.addr_offset(EBX, transfer_params.inc, offset);
            self.writeback_dest(transfer_params.base_reg, EBX);
        } else if transfer_params.writeback {
            self.writeback_dest(transfer_params.base_reg, EBX);
        }

        self.pop_flags();
    }

    /// Generate code to add to cycle count.
    fn add_const_cycles(&mut self, cycles: usize) {
        dynasm!(self.assembler
            ; .arch x64
            ; add QWORD [rbp-16], cycles as i32
        );
    }

    /// Generate code to add to cycle count from register.
    fn add_dyn_cycles(&mut self, reg: u8) {
        dynasm!(self.assembler
            ; .arch x64
            ; add QWORD [rbp-16], Rq(reg)
        );
    }

    /// Generate code to reset cycle count after clocking.
    fn reset_cycles(&mut self) {
        dynasm!(self.assembler
            ; .arch x64
            ; mov QWORD [rbp-16], 0
        );
    }

    /// Generate code for returning from subroutine.
    fn ret(&mut self) {
        println!("Gen return!");
        dynasm!(self.assembler
            ; .arch x64
            // TODO: write back flags?

            // Write back regs
            ; mov rax, [rbp-8]

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
        let dest_reg = dest.unwrap_or(EBX);
        let op = self.alu_operand_2(op2);
        match op {
            DataOperand::Imm(i) => dynasm!(self.assembler
                ; .arch x64
                ; mov Rd(dest_reg), DWORD i
            ),
            DataOperand::Reg(r) => dynasm!(self.assembler
                ; .arch x64
                ; mov Rd(dest_reg), Rd(r)
            )
        }
        // x86 MOV doesn't set flags by itself
        if set_flags {
            dynasm!(self.assembler
                ; .arch x64
                ; test Rd(dest_reg), DWORD -1
            );
        }
        // Unmapped dest: we need to write back.
        if dest.is_none() {
            self.writeback_unmapped_dest(rd, EBX);
        }
    }

    fn mvn(&mut self, rd: usize, op2: &ALUOperand, set_flags: bool) {
        let dest = self.get_mapped_register(rd);
        let dest_reg = dest.unwrap_or(EBX);
        let op = self.alu_operand_2(op2);
        match op {
            DataOperand::Imm(i) => dynasm!(self.assembler
                ; .arch x64
                ; mov Rd(dest_reg), DWORD i
                ; not Rd(dest_reg)
            ),
            DataOperand::Reg(r) => dynasm!(self.assembler
                ; .arch x64
                ; mov Rd(dest_reg), Rd(r)
                ; not Rd(dest_reg)
            )
        }
        // x86 MOV + NOT doesn't set flags by itself
        if set_flags {
            dynasm!(self.assembler
                ; .arch x64
                ; test Rd(dest_reg), DWORD -1
            );
        }
        // Unmapped dest: we need to write back.
        if dest.is_none() {
            self.writeback_unmapped_dest(rd, EBX);
        }
    }

    fn and(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => dynasm!(c.assembler
                    ; .arch x64
                    ; and Rd(op1), DWORD i
                ),
                DataOperand::Reg(r) => dynasm!(c.assembler
                    ; .arch x64
                    ; and Rd(op1), Rd(r)
                ),
            }
        })
    }

    fn eor(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => dynasm!(c.assembler
                    ; .arch x64
                    ; xor Rd(op1), DWORD i
                ),
                DataOperand::Reg(r) => dynasm!(c.assembler
                    ; .arch x64
                    ; xor Rd(op1), Rd(r)
                )
            }
        })
    }

    fn orr(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => dynasm!(c.assembler
                    ; .arch x64
                    ; or Rd(op1), DWORD i
                ),
                DataOperand::Reg(r) => dynasm!(c.assembler
                    ; .arch x64
                    ; or Rd(op1), Rd(r)
                )
            }
        })
    }

    fn bic(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        if !set_flags {
            self.push_flags();
        }
        let dest = self.get_mapped_register(rd);
        let dest_reg = dest.unwrap_or(EBX);
        let op1_reg = self.get_register(rn, EBX);

        let op2 = self.alu_operand_2(op2);

        match op2 {
            DataOperand::Imm(i) => dynasm!(self.assembler
                ; .arch x64
                ; mov eax, DWORD i
                ; andn Rd(dest_reg), eax, Rd(op1_reg)
            ),
            DataOperand::Reg(r) => dynasm!(self.assembler
                ; .arch x64
                ; andn Rd(dest_reg), Rd(r), Rd(op1_reg)
            )
        }

        // Unmapped dest: we need to write back.
        if dest.is_none() {
            self.writeback_unmapped_dest(rd, EBX);
        }
        if !set_flags {
            self.pop_flags();
        }
    }

    fn add(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => dynasm!(c.assembler
                    ; .arch x64
                    ; add Rd(op1), DWORD i
                ),
                DataOperand::Reg(r) => dynasm!(c.assembler
                    ; .arch x64
                    ; add Rd(op1), Rd(r)
                )
            }
        })
    }

    fn adc(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => dynasm!(c.assembler
                    ; .arch x64
                    ; adc Rd(op1), DWORD i
                ),
                DataOperand::Reg(r) => dynasm!(c.assembler
                    ; .arch x64
                    ; adc Rd(op1), Rd(r)
                )
            }
        })
    }

    fn sub(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => dynasm!(c.assembler
                    ; .arch x64
                    ; sub Rd(op1), DWORD i
                ),
                DataOperand::Reg(r) => dynasm!(c.assembler
                    ; .arch x64
                    ; sub Rd(op1), Rd(r)
                )
            }
        })
    }

    fn sbc(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        self.alu_instr(rd, rn, op2, set_flags, |c, op1, op2| {
            match op2 {
                DataOperand::Imm(i) => dynasm!(c.assembler
                    ; .arch x64
                    ; sbb Rd(op1), DWORD i
                ),
                DataOperand::Reg(r) => dynasm!(c.assembler
                    ; .arch x64
                    ; sbb Rd(op1), Rd(r)
                )
            }
        })
    }

    fn rsb(&mut self, rd: usize, rn: usize, op2: &ALUOperand, set_flags: bool) {
        if !set_flags {
            self.push_flags();
        }
        let op2 = self.alu_operand_2(op2);
        match op2 {
            DataOperand::Imm(i) => dynasm!(self.assembler
                ; .arch x64
                ; mov ebx, DWORD i
            ),
            DataOperand::Reg(r) => dynasm!(self.assembler
                ; .arch x64
                ; mov ebx, Rd(r)
            )
        }

        if rn == 15 {
            let pc_const = self.current_pc as i32;
            dynasm!(self.assembler
                ; .arch x64
                ; sub ebx, DWORD pc_const
            );
        } else {
            let op1_reg = self.get_register(rn, EAX);
            dynasm!(self.assembler
                ; .arch x64
                ; sub ebx, Rd(op1_reg)
            );
        }

        self.writeback_dest(rd, EBX);
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
            DataOperand::Imm(i) => dynasm!(self.assembler
                ; .arch x64
                ; mov ebx, DWORD i
            ),
            DataOperand::Reg(r) => dynasm!(self.assembler
                ; .arch x64
                ; mov ebx, Rd(r)
            )
        }

        if rn == 15 {
            let pc_const = self.current_pc as i32;
            dynasm!(self.assembler
                ; .arch x64
                ; sbb ebx, DWORD pc_const
            );
        } else {
            let op1_reg = self.get_register(rn, EAX);
            dynasm!(self.assembler
                ; .arch x64
                ; sbb ebx, Rd(op1_reg)
            );
        }

        self.writeback_dest(rd, EBX);
        if !set_flags {
            self.pop_flags();
        }
    }

    fn tst(&mut self, rn: usize, op2: &ALUOperand) {
        let op1_reg = self.alu_operand_1(rn, EBX);
        let op2 = self.alu_operand_2(op2);
        match op2 {
            DataOperand::Imm(i) => dynasm!(self.assembler
                ; .arch x64
                ; test Rd(op1_reg), DWORD i
            ),
            DataOperand::Reg(r) => dynasm!(self.assembler
                ; .arch x64
                ; test Rd(op1_reg), Rd(r)
            )
        }
    }

    fn teq(&mut self, rn: usize, op2: &ALUOperand) {
        let op1_reg = self.alu_operand_1(rn, EBX);
        if op1_reg != EBX {
            dynasm!(self.assembler
                ; .arch x64
                ; mov ebx, Rd(op1_reg)
            );
        }
        let op2 = self.alu_operand_2(op2);
        match op2 {
            DataOperand::Imm(i) => dynasm!(self.assembler
                ; .arch x64
                ; xor ebx, DWORD i
            ),
            DataOperand::Reg(r) => dynasm!(self.assembler
                ; .arch x64
                ; xor ebx, Rd(r)
            )
        }
    }

    fn cmp(&mut self, rn: usize, op2: &ALUOperand) {
        let op1_reg = self.alu_operand_1(rn, EBX);
        let op2 = self.alu_operand_2(op2);
        match op2 {
            DataOperand::Imm(i) => dynasm!(self.assembler
                ; .arch x64
                ; cmp Rd(op1_reg), DWORD i
            ),
            DataOperand::Reg(r) => dynasm!(self.assembler
                ; .arch x64
                ; cmp Rd(op1_reg), Rd(r)
            )
        }
    }

    fn cmn(&mut self, rn: usize, op2: &ALUOperand) {
        let op1_reg = self.alu_operand_1(rn, EBX);
        if op1_reg != EBX {
            dynasm!(self.assembler
                ; .arch x64
                ; mov ebx, Rd(op1_reg)
            );
        }
        let op2 = self.alu_operand_2(op2);
        match op2 {
            DataOperand::Imm(i) => dynasm!(self.assembler
                ; .arch x64
                ; add ebx, DWORD i
            ),
            DataOperand::Reg(r) => dynasm!(self.assembler
                ; .arch x64
                ; add ebx, Rd(r)
            )
        }
    }

    fn taddpc(&mut self, rd: usize, op2: u32) {
        let val = self.current_pc.wrapping_add(op2) as i32;
        let dest = self.get_mapped_register(rd);
        let dest_reg = dest.unwrap_or(EBX);
        dynasm!(self.assembler
            ; .arch x64
            ; mov Rd(dest_reg), DWORD val
        );
        if dest.is_none() {
            self.writeback_unmapped_dest(rd, EBX);
        }
    }

    fn mul(&mut self, rd: usize, rs: usize, rm: usize, set_flags: bool) {
        if !set_flags {
            self.push_flags();
        }

        let op2 = self.get_register(rm, EBX);

        let op1 = self.get_register(rs, EAX);
        if op1 != EAX {
            dynasm!(self.assembler
                ; .arch x64
                ; mov eax, Rd(op1)
            );
        }

        dynasm!(self.assembler
            ; .arch x64
            ; mul Rd(op2)
        );
        self.writeback_dest(rd, EAX);
        if !set_flags {
            self.pop_flags();
        }
    }

    fn mla(&mut self, rd: usize, rn: usize, rs: usize, rm: usize, set_flags: bool) {
        if !set_flags {
            self.push_flags();
        }

        let op2 = self.get_register(rm, EBX);

        let op1 = self.get_register(rs, EAX);
        if op1 != EAX {
            dynasm!(self.assembler
                ; .arch x64
                ; mov eax, Rd(op1)
            );
        }

        dynasm!(self.assembler
            ; .arch x64
            ; mul Rd(op2)
        );

        let add_op = self.get_register(rn, EDX);
        dynasm!(self.assembler
            ; .arch x64
            ; add eax, Rd(add_op)
        );

        self.writeback_dest(rd, EAX);
        if !set_flags {
            self.pop_flags();
        }
    }

    fn umull(&mut self, rd_hi: usize, rd_lo: usize, rs: usize, rm: usize, set_flags: bool) {
        if !set_flags {
            self.push_flags();
        }

        let op2 = self.get_register(rm, EBX);

        let op1 = self.get_register(rs, EAX);
        if op1 != EAX {
            dynasm!(self.assembler
                ; .arch x64
                ; mov eax, Rd(op1)
            );
        }

        dynasm!(self.assembler
            ; .arch x64
            ; mul Rd(op2)
        );

        self.writeback_dest(rd_hi, EDX);
        self.writeback_dest(rd_lo, EAX);
        if !set_flags {
            self.pop_flags();
        }
    }

    fn umlal(&mut self, rd_hi: usize, rd_lo: usize, rs: usize, rm: usize, set_flags: bool) {
        if !set_flags {
            self.push_flags();
        }

        let op2 = self.get_register(rm, EBX);

        let op1 = self.get_register(rs, EAX);
        if op1 != EAX {
            dynasm!(self.assembler
                ; .arch x64
                ; mov eax, Rd(op1)
            );
        }

        dynasm!(self.assembler
            ; .arch x64
            ; mul Rd(op2)
        );

        // Accumulate
        let dest_lo = self.get_register(rd_lo, EBX);
        let dest_hi = self.get_register(rd_hi, ECX);
        dynasm!(self.assembler
            ; .arch x64
            ; add eax, Rd(dest_lo)
            ; adc edx, Rd(dest_hi)
        );
        
        self.writeback_dest(rd_hi, EDX);
        self.writeback_dest(rd_lo, EAX);
        if !set_flags {
            self.pop_flags();
        }
    }

    fn smull(&mut self, rd_hi: usize, rd_lo: usize, rs: usize, rm: usize, set_flags: bool) {
        if !set_flags {
            self.push_flags();
        }

        let op2 = self.get_register(rm, EBX);

        let op1 = self.get_register(rs, EAX);
        if op1 != EAX {
            dynasm!(self.assembler
                ; .arch x64
                ; mov eax, Rd(op1)
            );
        }

        dynasm!(self.assembler
            ; .arch x64
            ; imul Rd(op2)
        );

        self.writeback_dest(rd_hi, EDX);
        self.writeback_dest(rd_lo, EAX);
        if !set_flags {
            self.pop_flags();
        }
    }

    fn smlal(&mut self, rd_hi: usize, rd_lo: usize, rs: usize, rm: usize, set_flags: bool) {
        if !set_flags {
            self.push_flags();
        }

        let op2 = self.get_register(rm, EBX);

        let op1 = self.get_register(rs, EAX);
        if op1 != EAX {
            dynasm!(self.assembler
                ; .arch x64
                ; mov eax, Rd(op1)
            );
        }

        dynasm!(self.assembler
            ; .arch x64
            ; imul Rd(op2)
        );

        // Accumulate
        let dest_lo = self.get_register(rd_lo, EBX);
        let dest_hi = self.get_register(rd_hi, ECX);
        dynasm!(self.assembler
            ; .arch x64
            ; add eax, Rd(dest_lo)
            ; adc edx, Rd(dest_hi)
        );
        
        self.writeback_dest(rd_hi, EDX);
        self.writeback_dest(rd_lo, EAX);
        if !set_flags {
            self.pop_flags();
        }
    }

    fn b(&mut self, cond: ARMCondition, label: DynamicLabel) {
        match cond {
            ARMCondition::EQ => dynasm!(self.assembler
                ; .arch x64
                ; je =>label
            ),
            ARMCondition::NE => dynasm!(self.assembler
                ; .arch x64
                ; jne =>label
            ),
            ARMCondition::CS => dynasm!(self.assembler
                ; .arch x64
                ; jc =>label
            ),
            ARMCondition::CC => dynasm!(self.assembler
                ; .arch x64
                ; jnc =>label
            ),
            ARMCondition::MI => dynasm!(self.assembler
                ; .arch x64
                ; js =>label
            ),
            ARMCondition::PL => dynasm!(self.assembler
                ; .arch x64
                ; jns =>label
            ),
            ARMCondition::VS => dynasm!(self.assembler
                ; .arch x64
                ; jo =>label
            ),
            ARMCondition::VC => dynasm!(self.assembler
                ; .arch x64
                ; jno =>label
            ),
            // Not sure about this one...
            ARMCondition::HI => dynasm!(self.assembler
                ; .arch x64
                ; ja =>label
            ),
            // ...or this one
            ARMCondition::LS => dynasm!(self.assembler
                ; .arch x64
                ; jna =>label
            ),
            ARMCondition::GE => dynasm!(self.assembler
                ; .arch x64
                ; jge =>label
            ),
            ARMCondition::LT => dynasm!(self.assembler
                ; .arch x64
                ; jl =>label
            ),
            ARMCondition::GT => dynasm!(self.assembler
                ; .arch x64
                ; jg =>label
            ),
            ARMCondition::LE => dynasm!(self.assembler
                ; .arch x64
                ; jle =>label
            ),
            ARMCondition::AL => dynasm!(self.assembler
                ; .arch x64
                ; jmp =>label
            ),
        }
    }

    fn bl(&mut self, offset: u32) {
        self.push_flags();
        let dest = self.current_pc.wrapping_add(offset) as i32;
        let call_subroutine = wrap_call_subroutine::<M, T> as i64;
        dynasm!(self.assembler
            ; .arch x64
            ; mov esi, DWORD dest
        );
        self.call(call_subroutine);
        self.pop_flags();
    }

    fn swp(&mut self, rn: usize, rd: usize, rm: usize, byte: bool) {
        self.push_flags();

        let addr_reg = self.get_register(rn, EBX);
        if addr_reg != EBX {
            dynasm!(self.assembler
                ; .arch x64
                ; mov ebx, Rd(addr_reg)
            );
        }

        let load_routine = if byte {
            wrap_load_byte::<M, T> as i64
        } else {
            wrap_load_word::<M, T> as i64
        };
        dynasm!(self.assembler
            ; .arch x64
            ; mov esi, ebx
        );
        self.call(load_routine);
        self.add_dyn_cycles(EDX);

        if byte {
            dynasm!(self.assembler
                ; .arch x64
                ; movzx eax, al
            );
        }

        let src_data_reg = self.get_register(rm, EDX);
        if src_data_reg != EDX {
            dynasm!(self.assembler
                ; .arch x64
                ; mov edx, Rd(src_data_reg)
            );
        }

        self.writeback_dest(rd, EAX);

        let store_routine = if byte {
            wrap_store_byte::<M, T> as i64
        } else {
            wrap_store_word_n::<M, T> as i64
        };
        dynasm!(self.assembler
            ; .arch x64
            ; mov esi, ebx
        );
        self.call(store_routine);
        self.add_dyn_cycles(EAX);

        self.pop_flags();
    }

    fn tldrpc(&mut self, data_reg: usize, offset: u32) {
        self.push_flags();

        let addr = self.current_pc.wrapping_add(offset) as i32;
        let load_word = wrap_load_word::<M, T> as i64;
        dynasm!(self.assembler
            ; .arch x64
            ; mov esi, DWORD addr
        );
        self.call(load_word);
        self.add_dyn_cycles(EDX);

        self.writeback_dest(data_reg, EAX);

        self.pop_flags();
    }

    fn ldr(&mut self, transfer_params: &TransferParams, data_reg: usize, offset: &ShiftOperand, byte: bool) {
        self.push_flags();

        self.base_reg(transfer_params.base_reg, EBX);

        if transfer_params.pre_index {
            let offset = self.shift_op(offset);
            self.addr_offset(EBX, transfer_params.inc, offset);
        }

        let load_routine = if byte {
            wrap_load_byte::<M, T> as i64
        } else {
            wrap_load_word::<M, T> as i64
        };
        dynasm!(self.assembler
            ; .arch x64
            ; mov esi, ebx
        );
        self.call(load_routine);

        if byte {
            dynasm!(self.assembler
                ; .arch x64
                ; movzx eax, al
            );
        }

        self.writeback_dest(data_reg, EAX);
        self.add_dyn_cycles(EDX);
        
        if !transfer_params.pre_index {
            let offset = self.shift_op(offset);
            self.addr_offset(EBX, transfer_params.inc, offset);
            self.writeback_dest(transfer_params.base_reg, EBX);
        } else if transfer_params.writeback {
            self.writeback_dest(transfer_params.base_reg, EBX);
        }

        self.pop_flags();
    }

    fn ldrh(&mut self, transfer_params: &TransferParams, data_reg: usize, offset: &OpData) {
        let load_halfword = wrap_load_halfword::<M, T> as i64;
        self.ext_ldr(transfer_params, data_reg, offset, load_halfword, |c| {
            dynasm!(c.assembler
                ; .arch x64
                ; movzx eax, ax
            );
        });
    }

    fn ldrsb(&mut self, transfer_params: &TransferParams, data_reg: usize, offset: &OpData) {
        let load_byte = wrap_load_byte::<M, T> as i64;
        self.ext_ldr(transfer_params, data_reg, offset, load_byte, |c| {
            dynasm!(c.assembler
                ; .arch x64
                ; movsx eax, al
            );
        });
    }

    fn ldrsh(&mut self, transfer_params: &TransferParams, data_reg: usize, offset: &OpData) {
        let load_halfword = wrap_load_halfword::<M, T> as i64;
        self.ext_ldr(transfer_params, data_reg, offset, load_halfword, |c| {
            dynasm!(c.assembler
                ; .arch x64
                ; movsx eax, ax
            );
        });
    }

    fn str(&mut self, transfer_params: &TransferParams, data_reg: usize, offset: &ShiftOperand, byte: bool) {
        self.push_flags();

        self.base_reg(transfer_params.base_reg, EBX);

        if transfer_params.pre_index {
            let offset = self.shift_op(offset);
            self.addr_offset(EBX, transfer_params.inc, offset);
        }

        let source_reg = self.get_register(data_reg, EDX);
        if source_reg != EDX {
            dynasm!(self.assembler
                ; .arch x64
                ; mov edx, Rd(source_reg)
            );
        }

        let store_routine = if byte {
            wrap_store_byte::<M, T> as i64
        } else {
            wrap_store_word_n::<M, T> as i64
        };
        dynasm!(self.assembler
            ; .arch x64
            ; mov esi, ebx
        );
        self.call(store_routine);
        self.add_dyn_cycles(EAX);
        
        if !transfer_params.pre_index {
            let offset = self.shift_op(offset);
            self.addr_offset(EBX, transfer_params.inc, offset);
            self.writeback_dest(transfer_params.base_reg, EBX);
        } else if transfer_params.writeback {
            self.writeback_dest(transfer_params.base_reg, EBX);
        }

        self.pop_flags();
    }

    fn strh(&mut self, transfer_params: &TransferParams, data_reg: usize, offset: &OpData) {
        self.push_flags();

        self.base_reg(transfer_params.base_reg, EBX);

        if transfer_params.pre_index {
            let offset = self.data_op(offset);
            self.addr_offset(EBX, transfer_params.inc, offset);
        }

        let source_reg = self.get_register(data_reg, EDX);
        if source_reg != EDX {
            dynasm!(self.assembler
                ; .arch x64
                ; mov edx, Rd(source_reg)
            );
        }

        let store_halfword = wrap_store_halfword::<M, T> as i64;
        dynasm!(self.assembler
            ; .arch x64
            ; mov esi, ebx
        );
        self.call(store_halfword);
        self.add_dyn_cycles(EAX);
        
        if !transfer_params.pre_index {
            let offset = self.data_op(offset);
            self.addr_offset(EBX, transfer_params.inc, offset);
            self.writeback_dest(transfer_params.base_reg, EBX);
        } else if transfer_params.writeback {
            self.writeback_dest(transfer_params.base_reg, EBX);
        }

        self.pop_flags();
    }

    fn ldm(&mut self, transfer_params: &TransferParams, reg_list: u32) {
        self.push_flags();

        self.base_reg(transfer_params.base_reg, EBX);

        if !transfer_params.inc {
            let offset = (reg_list.count_ones() * 4) as i32;
            dynasm!(self.assembler
                ; .arch x64
                ; sub ebx, WORD offset
            );
            if transfer_params.writeback {
                self.writeback_dest(transfer_params.base_reg, EBX);
            }
        }

        let pre_inc = transfer_params.pre_index == transfer_params.inc;
        let mut load_word = wrap_load_word_force_align_n::<M, T> as i64;
        for reg in (0..16).filter(|reg| u32::test_bit(reg_list, *reg)) {
            if pre_inc {
                dynasm!(self.assembler
                    ; .arch x64
                    ; add ebx, WORD 4
                );
            }
            dynasm!(self.assembler
                ; .arch x64
                ; mov esi, ebx
            );
            self.call(load_word);
            if !pre_inc {
                dynasm!(self.assembler
                    ; .arch x64
                    ; add ebx, WORD 4
                );
            }

            self.writeback_dest(reg, EAX);
            self.add_dyn_cycles(EDX);

            load_word = wrap_load_word_force_align_s::<M, T> as i64;
        }

        if transfer_params.inc && transfer_params.writeback {
            self.writeback_dest(transfer_params.base_reg, EBX);
        }

        self.pop_flags();
    }

    fn stm(&mut self, transfer_params: &TransferParams, reg_list: u32) {
        self.push_flags();

        self.base_reg(transfer_params.base_reg, EBX);

        if !transfer_params.inc {
            let offset = (reg_list.count_ones() * 4) as i32;
            dynasm!(self.assembler
                ; .arch x64
                ; sub ebx, WORD offset
            );
            if transfer_params.writeback {
                self.writeback_dest(transfer_params.base_reg, EBX);
            }
        }

        let pre_inc = transfer_params.pre_index == transfer_params.inc;
        let mut store_word = wrap_store_word_n::<M, T> as i64;
        for reg in (0..16).filter(|reg| u32::test_bit(reg_list, *reg)) {
            if pre_inc {
                dynasm!(self.assembler
                    ; .arch x64
                    ; add ebx, WORD 4
                );
            }

            let source_reg = self.get_register(reg, EDX);
            if source_reg != EDX {
                dynasm!(self.assembler
                    ; .arch x64
                    ; mov edx, Rd(source_reg)
                );
            }

            dynasm!(self.assembler
                ; .arch x64
                ; mov esi, ebx
            );
            self.call(store_word);
            self.add_dyn_cycles(EAX);

            if !pre_inc {
                dynasm!(self.assembler
                    ; .arch x64
                    ; add ebx, WORD 4
                );
            }

            store_word = wrap_store_word_s::<M, T> as i64;
        }
        
        if transfer_params.inc && transfer_params.writeback {
            self.writeback_dest(transfer_params.base_reg, EBX);
        }

        self.pop_flags();
    }
}

// CPU wrappers
pub unsafe extern "Rust" fn wrap_call_subroutine<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, dest: u32) {
    cpu.as_mut().unwrap().call_subroutine(dest);
}

pub unsafe extern "Rust" fn wrap_mut_regs<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T) -> *mut u32 {
    cpu.as_mut().unwrap().mut_regs().as_mut_ptr()
}

pub unsafe extern "Rust" fn wrap_load_word<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, addr: u32) -> (u32, usize) {
    cpu.as_mut().unwrap().load_word(MemCycleType::N, addr)
}

pub unsafe extern "Rust" fn wrap_load_word_force_align_n<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, addr: u32) -> (u32, usize) {
    cpu.as_mut().unwrap().load_word_force_align(MemCycleType::N, addr)
}

pub unsafe extern "Rust" fn wrap_load_word_force_align_s<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, addr: u32) -> (u32, usize) {
    cpu.as_mut().unwrap().load_word_force_align(MemCycleType::S, addr)
}

pub unsafe extern "Rust" fn wrap_load_halfword<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, addr: u32) -> (u16, usize) {
    cpu.as_mut().unwrap().load_halfword(MemCycleType::N, addr)
}

pub unsafe extern "Rust" fn wrap_load_byte<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, addr: u32) -> (u8, usize) {
    cpu.as_mut().unwrap().load_byte(MemCycleType::N, addr)
}

pub unsafe extern "Rust" fn wrap_store_word_n<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, addr: u32, data: u32) -> usize {
    cpu.as_mut().unwrap().store_word(MemCycleType::N, addr, data)
}

pub unsafe extern "Rust" fn wrap_store_word_s<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, addr: u32, data: u32) -> usize {
    cpu.as_mut().unwrap().store_word(MemCycleType::S, addr, data)
}

pub unsafe extern "Rust" fn wrap_store_halfword<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, addr: u32, data: u16) -> usize {
    cpu.as_mut().unwrap().store_halfword(MemCycleType::N, addr, data)
}

pub unsafe extern "Rust" fn wrap_store_byte<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, addr: u32, data: u8) -> usize {
    cpu.as_mut().unwrap().store_byte(MemCycleType::N, addr, data)
}

pub unsafe extern "Rust" fn wrap_clock<M: Mem32<Addr = u32>, T: ARMCore<M>>(cpu: *mut T, cycles: usize) {
    // TODO: wrap this in a func...
    cpu.as_mut().unwrap().ref_mem_mut().clock(cycles);
}
