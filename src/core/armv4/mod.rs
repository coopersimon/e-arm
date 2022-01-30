/// ARMv4 Instruction Set

#[allow(dead_code)]
mod armv4_test;
pub mod instructions;
pub mod coproc;
pub mod execute;
pub mod compile;

/// Calculate the number of cycles needed for the multiply.
/// This depends on the status of the most significant bytes of the second operand.
/// Assume a full multiply takes 4 cycles.
/// If the most significant byte is all 0 or all 1, it can be reduced by 1 cycle.
/// Continue for the 2nd and 3rd most significant bytes.
const fn mul_cycles(op2: u32) -> usize {
    let leading_zeros = op2.leading_zeros();
    if leading_zeros == 0 {
        let leading_ones = op2.leading_ones();
        if leading_ones >= 24 {
            1
        } else if leading_ones >= 16 {
            2
        } else if leading_ones >= 8 {
            3
        } else {
            4
        }
    } else if leading_zeros >= 24 {
        1
    } else if leading_zeros >= 16 {
        2
    } else if leading_zeros >= 8 {
        3
    } else {
        4
    }
}
