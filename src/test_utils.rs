#![cfg(test)]

use std::fmt::Debug;

pub fn debug_print<T>(t: T) -> String where T: Debug {
    format!("{:#?}", t)
}
