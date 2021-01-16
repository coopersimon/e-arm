/// Additional utils

use crate::Coprocessor;

// TODO: use array when const generics are in rust
pub fn to_slice(coproc: std::collections::HashMap<usize, Box<dyn Coprocessor>>) -> Box<[Option<Box<dyn Coprocessor>>]> {
    let mut ret = vec![None; 16];

    for (i, c) in coproc.drain() {
        ret[i] = Some(c);
    }

    ret.into_boxed_slice()
}