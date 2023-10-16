use std::collections::HashMap;
use crate::term::CTerm;

pub struct Signature {
    pub defs: HashMap<String, CTerm>,
}

impl Signature {
    pub fn lift_lambdas(&mut self) {
        let mut counter = 0;
        let mut new_lambdas: Vec<(String, CTerm)> = Vec::new();
        for (_, t) in self.defs.iter_mut() {
            Self::lift_lambdas_in_term(&mut counter, t, &mut new_lambdas);
        }
        for (name, t) in new_lambdas {
            self.defs.insert(name, t);
        }
    }

    fn lift_lambdas_in_term(counter: &mut i32, t: &mut CTerm, new_lambdas: &mut Vec<(String, CTerm)>) {
        todo!()
    }
}