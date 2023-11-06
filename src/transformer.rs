use crate::term::{CTerm, VTerm};

pub trait Transformer {
    fn add_binding(&mut self, name: usize) -> usize { name }

    fn remove_binding(&mut self, _name: usize) {}

    fn transform_v_term(&mut self, v_term: &mut VTerm) {
        match v_term {
            VTerm::Var { .. } => self.transform_var(v_term),
            VTerm::Thunk { .. } => self.transform_thunk(v_term),
            VTerm::Int { .. } => self.transform_int(v_term),
            VTerm::Str { .. } => self.transform_str(v_term),
            VTerm::Array { .. } => self.transform_tuple(v_term),
        }
    }

    fn transform_var(&mut self, _v_term: &mut VTerm) {}
    fn transform_thunk(&mut self, v_term: &mut VTerm) {
        let VTerm::Thunk { t } = v_term else { unreachable!() };
        self.transform_c_term(t);
    }
    fn transform_int(&mut self, _v_term: &mut VTerm) {}
    fn transform_str(&mut self, _v_term: &mut VTerm) {}
    fn transform_tuple(&mut self, v_term: &mut VTerm) {
        let VTerm::Array { values } = v_term else { unreachable!() };
        for v in values {
            self.transform_v_term(v);
        }
    }

    fn transform_c_term(&mut self, c_term: &mut CTerm) {
        match c_term {
            CTerm::Redex { .. } => self.transform_redex(c_term),
            CTerm::Return { .. } => self.transform_return(c_term),
            CTerm::Force { .. } => self.transform_force(c_term),
            CTerm::Let { .. } => self.transform_let(c_term),
            CTerm::Def { .. } => self.transform_def(c_term),
            CTerm::CaseInt { .. } => self.transform_case_int(c_term),
            CTerm::MemGet { .. } => self.transform_projection(c_term),
            CTerm::CaseStr { .. } => self.transform_case_str(c_term),
            CTerm::Primitive { .. } => self.transform_primitive(c_term),
        }
    }

    fn transform_redex(&mut self, c_term: &mut CTerm) {
        let CTerm::Redex { function, args } = c_term else { unreachable!() };
        self.transform_c_term(function);
        args.iter_mut().for_each(|arg| self.transform_v_term(arg));
    }

    fn transform_return(&mut self, c_term: &mut CTerm) {
        let CTerm::Return { value } = c_term else { unreachable!() };
        self.transform_v_term(value);
    }

    fn transform_force(&mut self, c_term: &mut CTerm) {
        let CTerm::Force { thunk } = c_term else { unreachable!() };
        self.transform_v_term(thunk);
    }

    fn transform_let(&mut self, c_term: &mut CTerm) {
        let CTerm::Let { t, body, bound_index: bound_name } = c_term else { unreachable!() };
        self.transform_c_term(t);
        let old_name = *bound_name;
        *bound_name = self.add_binding(*bound_name);
        self.transform_c_term(body);
        self.remove_binding(old_name);
    }

    fn transform_def(&mut self, _c_term: &mut CTerm) {}

    fn transform_case_int(&mut self, c_term: &mut CTerm) {
        let CTerm::CaseInt { t, branches, default_branch } = c_term else { unreachable!() };
        self.transform_v_term(t);
        for (_, branch) in branches.iter_mut() {
            self.transform_c_term(branch);
        }
        if let Some(default_branch) = default_branch {
            self.transform_c_term(default_branch);
        }
    }

    fn transform_projection(&mut self, c_term: &mut CTerm) {
        let CTerm::MemGet { base: array, offset: index } = c_term else { unreachable!() };
        self.transform_v_term(array);
        self.transform_v_term(index);
    }

    fn transform_case_str(&mut self, c_term: &mut CTerm) {
        let CTerm::CaseStr { t, branches, default_branch } = c_term else { unreachable!() };
        self.transform_v_term(t);
        for (_, branch) in branches.iter_mut() {
            self.transform_c_term(branch);
        }
        if let Some(default_branch) = default_branch {
            self.transform_c_term(default_branch);
        }
    }

    fn transform_primitive(&mut self, _c_term: &mut CTerm) {}
}