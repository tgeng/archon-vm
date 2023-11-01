use std::collections::{HashMap, HashSet};
use crate::term::{CTerm, VTerm};
use crate::visitor::Visitor;

pub trait HasFreeVar {
    fn free_vars(&mut self) -> HashSet<String>;
}

impl HasFreeVar for CTerm {
    fn free_vars(&mut self) -> HashSet<String> {
        let mut visitor = FreeVarVisitor { free_vars: HashSet::new(), binding_count: HashMap::new() };
        visitor.visit_c_term(self);
        visitor.free_vars
    }
}

impl HasFreeVar for VTerm {
    fn free_vars(&mut self) -> HashSet<String> {
        let mut visitor = FreeVarVisitor { free_vars: HashSet::new(), binding_count: HashMap::new() };
        visitor.visit_v_term(self);
        visitor.free_vars
    }
}

struct FreeVarVisitor {
    free_vars: HashSet<String>,
    binding_count: HashMap<String, usize>,
}

impl Visitor for FreeVarVisitor {
    fn add_binding(&mut self, name: &str) -> Option<String> {
        self.binding_count.insert(name.to_string(), self.binding_count.get(name).cloned().unwrap_or(0) + 1);
        None
    }

    fn remove_binding(&mut self, name: &str) {
        self.binding_count.insert(name.to_string(), self.binding_count.get(name).cloned().unwrap_or(0) - 1);
    }

    fn visit_var(&mut self, _v_term: &mut VTerm) {
        match _v_term {
            VTerm::Var { name } => {
                if self.binding_count.get(name).cloned().unwrap_or(0) == 0 {
                    self.free_vars.insert(name.clone());
                }
            }
            _ => unreachable!(),
        }
    }
}
