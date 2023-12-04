use crate::ast::term::{CTerm, VTerm};

pub trait Transformer {
    fn add_binding(&mut self, name: usize) -> usize { name }

    fn remove_binding(&mut self, _name: usize) {}

    fn transform_v_term(&mut self, v_term: &mut VTerm) {
        match v_term {
            VTerm::Var { .. } => self.transform_var(v_term),
            VTerm::Thunk { .. } => self.transform_thunk(v_term),
            VTerm::Int { .. } => self.transform_int(v_term),
            VTerm::Str { .. } => self.transform_str(v_term),
            VTerm::Struct { .. } => self.transform_tuple(v_term),
        }
    }

    fn transform_var(&mut self, _v_term: &mut VTerm) {}
    fn transform_thunk(&mut self, v_term: &mut VTerm) {
        let VTerm::Thunk { t, .. } = v_term else { unreachable!() };
        self.transform_c_term(t);
    }
    fn transform_int(&mut self, _v_term: &mut VTerm) {}
    fn transform_str(&mut self, _v_term: &mut VTerm) {}
    fn transform_tuple(&mut self, v_term: &mut VTerm) {
        let VTerm::Struct { values } = v_term else { unreachable!() };
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
            CTerm::Lambda { .. } => self.transform_lambda(c_term),
            CTerm::MemGet { .. } => self.transform_mem_get(c_term),
            CTerm::MemSet { .. } => self.transform_mem_set(c_term),
            CTerm::PrimitiveCall { .. } => self.transform_primitive_call(c_term),
            CTerm::OperationCall { .. } => self.transform_operation_call(c_term),
            CTerm::Handler { .. } => self.transform_handler(c_term),
            CTerm::LongJump { .. } => self.transform_long_return(c_term),
        }
    }

    fn transform_redex(&mut self, c_term: &mut CTerm) {
        self.transform_redex_default(c_term);
    }

    fn transform_redex_default(&mut self, c_term: &mut CTerm) {
        let CTerm::Redex { function, args } = c_term else { unreachable!() };
        self.transform_c_term(function);
        args.iter_mut().for_each(|arg| self.transform_v_term(arg));
    }

    fn transform_return(&mut self, c_term: &mut CTerm) {
        let CTerm::Return { value } = c_term else { unreachable!() };
        self.transform_v_term(value);
    }

    fn transform_force(&mut self, c_term: &mut CTerm) {
        self.transform_force_default(c_term);
    }

    fn transform_force_default(&mut self, c_term: &mut CTerm) {
        let CTerm::Force { thunk, .. } = c_term else { unreachable!() };
        self.transform_v_term(thunk);
    }

    fn transform_let(&mut self, c_term: &mut CTerm) {
        let CTerm::Let { t, body, bound_index: bound_name, .. } = c_term else { unreachable!() };
        self.transform_c_term(t);
        let old_name = *bound_name;
        *bound_name = self.add_binding(*bound_name);
        self.transform_c_term(body);
        self.remove_binding(old_name);
    }

    fn transform_def(&mut self, _c_term: &mut CTerm) {}

    fn transform_case_int(&mut self, c_term: &mut CTerm) {
        let CTerm::CaseInt { t, branches, default_branch, .. } = c_term else { unreachable!() };
        self.transform_v_term(t);
        for (_, branch) in branches.iter_mut() {
            self.transform_c_term(branch);
        }
        if let Some(default_branch) = default_branch {
            self.transform_c_term(default_branch);
        }
    }

    fn transform_lambda(&mut self, c_term: &mut CTerm) {
        self.transform_lambda_default(c_term);
    }

    fn transform_lambda_default(&mut self, c_term: &mut CTerm) {
        let CTerm::Lambda { args, body, .. } = c_term else { unreachable!() };
        let old_args = args.clone();
        args.iter_mut().for_each(|(arg, _)| *arg = self.add_binding(*arg));
        self.transform_c_term(body);
        old_args.iter().for_each(|(arg, _)| self.remove_binding(*arg));
    }

    fn transform_mem_get(&mut self, c_term: &mut CTerm) {
        let CTerm::MemGet { base, offset } = c_term else { unreachable!() };
        self.transform_v_term(base);
        self.transform_v_term(offset);
    }

    fn transform_mem_set(&mut self, c_term: &mut CTerm) {
        let CTerm::MemSet { base, offset, value } = c_term else { unreachable!() };
        self.transform_v_term(base);
        self.transform_v_term(offset);
        self.transform_v_term(value);
    }

    fn transform_primitive_call(&mut self, c_term: &mut CTerm) {
        let CTerm::PrimitiveCall { args, .. } = c_term else { unreachable!() };
        args.iter_mut().for_each(|arg| self.transform_v_term(arg));
    }

    fn transform_operation_call(&mut self, c_term: &mut CTerm) {
        let CTerm::OperationCall { eff, args, .. } = c_term else { unreachable!() };
        self.transform_v_term(eff);
        args.iter_mut().for_each(|arg| self.transform_v_term(arg));
    }

    fn transform_handler(&mut self, c_term: &mut CTerm) {
        self.transform_handler_default(c_term);
    }

    // Handling handler is convoluted so we create this helper function to avoid re-implementing
    // this in each transformer.
    fn transform_handler_default(&mut self, c_term: &mut CTerm) {
        let CTerm::Handler {
            parameter,
            parameter_disposer,
            parameter_replicator,
            transform,
            complex_handlers,
            simple_handlers,
            input
        } = c_term else { unreachable!() };
        self.transform_v_term(parameter);
        self.transform_v_term(parameter_disposer);
        self.transform_v_term(parameter_replicator);
        self.transform_v_term(transform);
        for (eff, handler) in complex_handlers.iter_mut() {
            self.transform_v_term(eff);
            self.transform_v_term(handler);
        }
        for (eff, handler) in simple_handlers.iter_mut() {
            self.transform_v_term(eff);
            self.transform_v_term(handler);
        }
        self.transform_v_term(input);
    }

    fn transform_long_return(&mut self, c_term: &mut CTerm) {
        let CTerm::LongJump { value } = c_term else { unreachable!() };
        self.transform_v_term(value);
    }
}