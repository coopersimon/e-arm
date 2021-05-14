
use crate::{ARMv4Instruction, core::armv4::instructions::ARMv4InstructionType};
use dynasmrt::{dynasm, Assembler, x64::X64Relocation};

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

impl DecodedInstruction {
    pub fn codegen_x64(self, assembler: &mut Assembler<X64Relocation>) {
        // TODO: insert label

        // TODO: conditional...

        if self.ret {
            decode_ret(assembler);
        } else {
            match self.instruction.instr {
                ARMv4InstructionType::MOV{..} => {},
                _ => panic!("not supported"),
            }
        }

        // TODO: conditional end
    }
}

fn decode_ret(assembler: &mut Assembler<X64Relocation>) {
    // TODO: add return routine...
    dynasm!(assembler
        ; .arch x64
        ; ret
    );
}
