use std::collections::{HashMap, HashSet};
use crate::ast::term::{CTerm, VTerm};
use crate::ast::visitor::Visitor;

pub trait HasFreeVar {
    fn free_vars(&mut self) -> HashSet<usize>;
}

impl HasFreeVar for CTerm {
    fn free_vars(&mut self) -> HashSet<usize> {
        let mut visitor = FreeVarVisitor { free_vars: HashSet::new(), binding_count: HashMap::new() };
        visitor.visit_c_term(self);
        visitor.free_vars
    }
}

impl HasFreeVar for VTerm {
    fn free_vars(&mut self) -> HashSet<usize> {
        let mut visitor = FreeVarVisitor { free_vars: HashSet::new(), binding_count: HashMap::new() };
        visitor.visit_v_term(self);
        visitor.free_vars
    }
}

struct FreeVarVisitor {
    free_vars: HashSet<usize>,
    binding_count: HashMap<usize, usize>,
}

impl Visitor for FreeVarVisitor {
    fn add_binding(&mut self, name: usize) {
        self.binding_count.insert(name, self.binding_count.get(&name).cloned().unwrap_or(0) + 1);
    }

    fn remove_binding(&mut self, name: usize) {
        self.binding_count.insert(name, self.binding_count.get(&name).cloned().unwrap_or(0) - 1);
    }

    fn visit_var(&mut self, _v_term: &VTerm) {
        match _v_term {
            VTerm::Var { index: name } => {
                if self.binding_count.get(name).cloned().unwrap_or(0) == 0 {
                    self.free_vars.insert(*name);
                }
            }
            _ => unreachable!(),
        }
    }
}
