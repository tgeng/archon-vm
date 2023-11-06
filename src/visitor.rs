use crate::term::{CTerm, VTerm};

pub trait Visitor {
    fn add_binding(&mut self, _name: usize) {}

    fn remove_binding(&mut self, _name: usize) {}

    fn visit_v_term(&mut self, v_term: &VTerm) {
        match v_term {
            VTerm::Var { .. } => self.visit_var(v_term),
            VTerm::Thunk { .. } => self.visit_thunk(v_term),
            VTerm::Int { .. } => self.visit_int(v_term),
            VTerm::Str { .. } => self.visit_str(v_term),
            VTerm::Array { .. } => self.visit_tuple(v_term),
        }
    }

    fn visit_var(&mut self, _v_term: &VTerm) {}
    fn visit_thunk(&mut self, v_term: &VTerm) {
        let VTerm::Thunk { t } = v_term else { unreachable!() };
        self.visit_c_term(t);
    }
    fn visit_int(&mut self, _v_term: &VTerm) {}
    fn visit_str(&mut self, _v_term: &VTerm) {}
    fn visit_tuple(&mut self, v_term: &VTerm) {
        let VTerm::Array { values } = v_term else { unreachable!() };
        for v in values {
            self.visit_v_term(v);
        }
    }

    fn visit_c_term(&mut self, c_term: &CTerm) {
        match c_term {
            CTerm::Redex { .. } => self.visit_redex(c_term),
            CTerm::Return { .. } => self.visit_return(c_term),
            CTerm::Force { .. } => self.visit_force(c_term),
            CTerm::Let { .. } => self.visit_let(c_term),
            CTerm::Def { .. } => self.visit_def(c_term),
            CTerm::CaseInt { .. } => self.visit_case_int(c_term),
            CTerm::ArrayGet { .. } => self.visit_projection(c_term),
            CTerm::CaseStr { .. } => self.visit_case_str(c_term),
            CTerm::Primitive { .. } => self.visit_primitive(c_term),
        }
    }

    fn visit_redex(&mut self, c_term: &CTerm) {
        let CTerm::Redex { function, args } = c_term else { unreachable!() };
        self.visit_c_term(function);
        args.iter().for_each(|arg| self.visit_v_term(arg));
    }

    fn visit_return(&mut self, c_term: &CTerm) {
        let CTerm::Return { value } = c_term else { unreachable!() };
        self.visit_v_term(value);
    }

    fn visit_force(&mut self, c_term: &CTerm) {
        let CTerm::Force { thunk } = c_term else { unreachable!() };
        self.visit_v_term(thunk);
    }

    fn visit_let(&mut self, c_term: &CTerm) {
        let CTerm::Let { t, body, bound_index: bound_name } = c_term else { unreachable!() };
        self.visit_c_term(t);
        self.add_binding(*bound_name);
        self.visit_c_term(body);
        self.remove_binding(*bound_name);
    }

    fn visit_def(&mut self, _c_term: &CTerm) {}

    fn visit_case_int(&mut self, c_term: &CTerm) {
        let CTerm::CaseInt { t, branches, default_branch } = c_term else { unreachable!() };
        self.visit_v_term(t);
        for (_, branch) in branches.iter() {
            self.visit_c_term(branch);
        }
        if let Some(default_branch) = default_branch {
            self.visit_c_term(default_branch);
        }
    }

    fn visit_projection(&mut self, c_term: &CTerm) {
        let CTerm::ArrayGet { array, index } = c_term else { unreachable!() };
        self.visit_v_term(array);
        self.visit_v_term(index);
    }

    fn visit_case_str(&mut self, c_term: &CTerm) {
        let CTerm::CaseStr { t, branches, default_branch } = c_term else { unreachable!() };
        self.visit_v_term(t);
        for (_, branch) in branches.iter() {
            self.visit_c_term(branch);
        }
        if let Some(default_branch) = default_branch {
            self.visit_c_term(default_branch);
        }
    }

    fn visit_primitive(&mut self, _c_term: &CTerm) {}
}