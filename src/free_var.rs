use std::collections::{HashMap, HashSet};
use crate::term::{CTerm, VTerm};
use crate::visitor::Visitor;

pub trait HasFreeVar {
    fn free_vars(&mut self) -> HashSet<String>;
}

impl HasFreeVar for CTerm {
    fn free_vars(&mut self) -> HashSet<String> {
        let mut visitor = FreeVarVisitor { free_vars: HashSet::new() };
        visitor.visit_c_term(&mut HashMap::new(), self);
        visitor.free_vars
    }
}

impl HasFreeVar for VTerm {
    fn free_vars(&mut self) -> HashSet<String> {
        let mut visitor = FreeVarVisitor { free_vars: HashSet::new() };
        visitor.visit_v_term(&mut HashMap::new(), self);
        visitor.free_vars
    }
}

struct FreeVarVisitor {
    free_vars: HashSet<String>,
}

impl Visitor for FreeVarVisitor {
    type Context = HashMap<String, usize>;

    fn with_bindings<F>(ctx: &mut Self::Context, _names: &[&str], action: F) where F: FnOnce(&mut Self::Context) {
        _names.iter().for_each(|s| { ctx.insert(s.to_string(), ctx.get(*s).cloned().unwrap_or(0) + 1); });
        action(ctx);
        _names.iter().for_each(|s| { ctx.insert(s.to_string(), ctx.get(*s).cloned().unwrap_or(0) - 1); });
    }

    fn visit_var(&mut self, _ctx: &mut Self::Context, _v_term: &mut VTerm) {
        match _v_term {
            VTerm::Var { name } => {
                if _ctx.get(name).cloned().unwrap_or(0) == 0 {
                    self.free_vars.insert(name.clone());
                }
            }
            _ => unreachable!(),
        }
    }
}
