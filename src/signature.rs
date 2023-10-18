use std::collections::HashMap;
use crate::term::CTerm;

pub struct Signature {
    pub defs: HashMap<String, (Vec<String>, CTerm)>,
}

impl Signature {}