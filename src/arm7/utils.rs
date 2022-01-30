/// Additional utils

use crate::CoprocV4Impl;

const NUM_COPROCESSORS: usize = 16;

// TODO: use array when const generics are in rust
pub fn to_slice(mut coproc: std::collections::HashMap<usize, CoprocV4Impl>) -> Box<[Option<CoprocV4Impl>]> {
    let mut ret = Vec::new();

    for i in 0..NUM_COPROCESSORS {
        ret.push(coproc.remove(&i));
    }

    ret.into_boxed_slice()
}