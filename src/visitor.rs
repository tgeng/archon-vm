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
            VTerm::Struct { .. } => self.visit_tuple(v_term),
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
        let VTerm::Struct { values } = v_term else { unreachable!() };
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
            CTerm::MemGet { .. } => self.visit_mem_get(c_term),
            CTerm::MemSet { .. } => self.visit_mem_set(c_term),
            CTerm::PrimitiveCall { .. } => self.visit_primitive_call(c_term),
            CTerm::SpecializedFunctionCall { .. } => self.visit_specialized_function_call(c_term),
            CTerm::OperationCall { .. } => self.visit_operation_call(c_term),
            CTerm::Handler { .. } => self.visit_handler(c_term),
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
        let CTerm::CaseInt { t, branches, default_branch, .. } = c_term else { unreachable!() };
        self.visit_v_term(t);
        for (_, branch) in branches.iter() {
            self.visit_c_term(branch);
        }
        if let Some(default_branch) = default_branch {
            self.visit_c_term(default_branch);
        }
    }

    fn visit_mem_get(&mut self, c_term: &CTerm) {
        let CTerm::MemGet { base, offset } = c_term else { unreachable!() };
        self.visit_v_term(base);
        self.visit_v_term(offset);
    }

    fn visit_mem_set(&mut self, c_term: &CTerm) {
        let CTerm::MemSet { base, offset, value } = c_term else { unreachable!() };
        self.visit_v_term(base);
        self.visit_v_term(offset);
        self.visit_v_term(value);
    }

    fn visit_primitive_call(&mut self, c_term: &CTerm) {
        let CTerm::PrimitiveCall { args, .. } = c_term else { unreachable!() };
        args.iter().for_each(|arg| self.visit_v_term(arg));
    }

    fn visit_specialized_function_call(&mut self, c_term: &CTerm) {
        let CTerm::SpecializedFunctionCall { args, .. } = c_term else { unreachable!() };
        args.iter().for_each(|arg| self.visit_v_term(arg));
    }

    fn visit_operation_call(&mut self, c_term: &CTerm) {
        let CTerm::OperationCall { eff, args } = c_term else { unreachable!() };
        eff.args.iter().for_each(|arg| self.visit_v_term(arg));
        args.iter().for_each(|arg| self.visit_v_term(arg));
    }

    fn visit_handler(&mut self, c_term: &CTerm) {
        let CTerm::Handler {
            parameter,
            box parameter_disposer,
            box parameter_replicator,
            box transform,
            complex_handlers,
            simple_handlers,
            box input
        } = c_term else { unreachable!() };
        self.visit_v_term(parameter);

        let (parameter_disposer_bound_index, parameter_disposer) = parameter_disposer;
        self.add_binding(*parameter_disposer_bound_index);
        self.visit_c_term(parameter_disposer);
        self.remove_binding(*parameter_disposer_bound_index);

        let (parameter_replicator_bound_index, parameter_replicator) = parameter_replicator;
        self.add_binding(*parameter_replicator_bound_index);
        self.visit_c_term(parameter_replicator);
        self.remove_binding(*parameter_replicator_bound_index);

        let (transform_parameter_bound_index, transform_input_bound_index, transform) = transform;
        self.add_binding(*transform_parameter_bound_index);
        self.add_binding(*transform_input_bound_index);
        self.visit_c_term(transform);
        self.remove_binding(*transform_input_bound_index);
        self.remove_binding(*transform_parameter_bound_index);

        for (eff, parameter_bound_index, args_bound_index, continuation_bound_index, handler) in complex_handlers.iter() {
            eff.args.iter().for_each(|arg| self.visit_v_term(arg));
            self.add_binding(*parameter_bound_index);
            args_bound_index.iter().for_each(|arg| self.add_binding(*arg));
            self.add_binding(*continuation_bound_index);
            self.visit_c_term(handler);
            self.remove_binding(*continuation_bound_index);
            args_bound_index.iter().for_each(|arg| self.remove_binding(*arg));
            self.remove_binding(*parameter_bound_index);
        }
        for (eff, parameter_bound_index, args_bound_index, handler) in simple_handlers.iter() {
            eff.args.iter().for_each(|arg| self.visit_v_term(arg));
            self.add_binding(*parameter_bound_index);
            args_bound_index.iter().for_each(|arg| self.add_binding(*arg));
            self.visit_c_term(handler);
            args_bound_index.iter().for_each(|arg| self.remove_binding(*arg));
            self.remove_binding(*parameter_bound_index);
        }
        self.visit_c_term(input);
    }
}