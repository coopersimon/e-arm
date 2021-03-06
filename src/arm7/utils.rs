/// Additional utils

use crate::Coprocessor;

const NUM_COPROCESSORS: usize = 16;

// TODO: use array when const generics are in rust
pub fn to_slice(mut coproc: std::collections::HashMap<usize, Box<dyn Coprocessor>>) -> Box<[Option<Box<dyn Coprocessor>>]> {
    let mut ret = Vec::new();

    for i in 0..NUM_COPROCESSORS {
        ret.push(coproc.remove(&i));
    }

    ret.into_boxed_slice()
}