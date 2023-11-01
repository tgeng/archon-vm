use crate::term::{CTerm, VTerm};

pub trait Visitor {
    fn add_binding(&mut self, _name: &str) -> Option<String> { None }

    fn remove_binding(&mut self, _name: &str) {}

    fn visit_v_term(&mut self, v_term: &mut VTerm) {
        match v_term {
            VTerm::Var { .. } => self.visit_var(v_term),
            VTerm::Thunk { .. } => self.visit_thunk(v_term),
            VTerm::Int { .. } => self.visit_int(v_term),
            VTerm::Str { .. } => self.visit_str(v_term),
            VTerm::Tuple { .. } => self.visit_tuple(v_term),
        }
    }

    fn visit_var(&mut self, _v_term: &mut VTerm) {}
    fn visit_thunk(&mut self, v_term: &mut VTerm) {
        let VTerm::Thunk { t } = v_term else { unreachable!() };
        self.visit_c_term(t);
    }
    fn visit_int(&mut self, _v_term: &mut VTerm) {}
    fn visit_str(&mut self, _v_term: &mut VTerm) {}
    fn visit_tuple(&mut self, v_term: &mut VTerm) {
        let VTerm::Tuple { values } = v_term else { unreachable!() };
        for v in values {
            self.visit_v_term(v);
        }
    }

    fn visit_c_term(&mut self, c_term: &mut CTerm) {
        match c_term {
            CTerm::Redex { .. } => self.visit_redex(c_term),
            CTerm::Return { .. } => self.visit_return(c_term),
            CTerm::Force { .. } => self.visit_force(c_term),
            CTerm::Let { .. } => self.visit_let(c_term),
            CTerm::Def { .. } => self.visit_def(c_term),
            CTerm::CaseInt { .. } => self.visit_case_int(c_term),
            CTerm::CaseTuple { .. } => self.visit_case_tuple(c_term),
            CTerm::CaseStr { .. } => self.visit_case_str(c_term),
            CTerm::Primitive { .. } => self.visit_primitive(c_term),
        }
    }

    fn visit_redex(&mut self, c_term: &mut CTerm) {
        let CTerm::Redex { function, args } = c_term else { unreachable!() };
        self.visit_c_term(function);
        args.iter_mut().for_each(|arg| self.visit_v_term(arg));
    }

    fn visit_return(&mut self, c_term: &mut CTerm) {
        let CTerm::Return { value } = c_term else { unreachable!() };
        self.visit_v_term(value);
    }

    fn visit_force(&mut self, c_term: &mut CTerm) {
        let CTerm::Force { thunk } = c_term else { unreachable!() };
        self.visit_v_term(thunk);
    }

    fn visit_let(&mut self, c_term: &mut CTerm) {
        let CTerm::Let { t, body, bound_name } = c_term else { unreachable!() };
        self.visit_c_term(t);
        let old_name = bound_name.clone();
        match self.add_binding(bound_name) {
            Some(new_name) => *bound_name = new_name,
            None => {}
        }
        self.visit_c_term(body);
        self.remove_binding(&old_name);
    }

    fn visit_def(&mut self, _c_term: &mut CTerm) {}

    fn visit_case_int(&mut self, c_term: &mut CTerm) {
        let CTerm::CaseInt { t, branches, default_branch } = c_term else { unreachable!() };
        self.visit_v_term(t);
        for (_, branch) in branches.iter_mut() {
            self.visit_c_term(branch);
        }
        if let Some(default_branch) = default_branch {
            self.visit_c_term(default_branch);
        }
    }

    fn visit_case_tuple(&mut self, c_term: &mut CTerm) {
        let CTerm::CaseTuple { t, bound_names, branch } = c_term else { unreachable!() };
        self.visit_v_term(t);
        let old_names = bound_names.clone();
        for name in bound_names.iter_mut() {
            match self.add_binding(name) {
                Some(new_name) => *name = new_name,
                None => {}
            }
        }
        self.visit_c_term(branch);
        for name in old_names.iter() {
            self.remove_binding(name);
        }
    }

    fn visit_case_str(&mut self, c_term: &mut CTerm) {
        let CTerm::CaseStr { t, branches, default_branch } = c_term else { unreachable!() };
        self.visit_v_term(t);
        for (_, branch) in branches.iter_mut() {
            self.visit_c_term(branch);
        }
        if let Some(default_branch) = default_branch {
            self.visit_c_term(default_branch);
        }
    }

    fn visit_primitive(&mut self, _c_term: &mut CTerm) {}
}