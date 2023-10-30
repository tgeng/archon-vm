use std::collections::HashMap;
use crate::term::CTerm;

pub struct Signature {
    pub defs: HashMap<String, (Vec<String>, CTerm)>,
}

impl Signature {
    pub fn new() -> Self {
        Self {
            defs: HashMap::new(),
        }
    }

    pub fn into_defs(self) -> HashMap<String, (Vec<String>, CTerm)> {
        self.defs
    }
}