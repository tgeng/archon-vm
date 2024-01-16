use crate::ast::term::{CTerm, VTerm};

pub trait Visitor {
    type Ctx: Copy;

    fn add_binding(&mut self, _name: usize, _ctx: Self::Ctx) {}

    fn remove_binding(&mut self, _name: usize, _ctx: Self::Ctx) {}

    fn visit_v_term(&mut self, v_term: &VTerm, ctx: Self::Ctx) {
        match v_term {
            VTerm::Var { .. } => self.visit_var(v_term, ctx),
            VTerm::Thunk { .. } => self.visit_thunk(v_term, ctx),
            VTerm::Int { .. } => self.visit_int(v_term, ctx),
            VTerm::Str { .. } => self.visit_str(v_term, ctx),
            VTerm::Struct { .. } => self.visit_tuple(v_term, ctx),
        }
    }

    fn visit_var(&mut self, _v_term: &VTerm, _ctx: Self::Ctx) {}
    fn visit_thunk(&mut self, v_term: &VTerm, ctx: Self::Ctx) {
        let VTerm::Thunk { t, .. } = v_term else { unreachable!() };
        self.visit_c_term(t, ctx);
    }
    fn visit_int(&mut self, _v_term: &VTerm, _ctx: Self::Ctx) {}
    fn visit_str(&mut self, _v_term: &VTerm, _ctx: Self::Ctx) {}
    fn visit_tuple(&mut self, v_term: &VTerm, ctx: Self::Ctx) {
        let VTerm::Struct { values } = v_term else { unreachable!() };
        for v in values {
            self.visit_v_term(v, ctx);
        }
    }

    fn visit_c_term(&mut self, c_term: &CTerm, ctx: Self::Ctx) {
        match c_term {
            CTerm::Redex { .. } => self.visit_redex(c_term, ctx),
            CTerm::Return { .. } => self.visit_return(c_term, ctx),
            CTerm::Force { .. } => self.visit_force(c_term, ctx),
            CTerm::Let { .. } => self.visit_let(c_term, ctx),
            CTerm::Def { .. } => self.visit_def(c_term, ctx),
            CTerm::CaseInt { .. } => self.visit_case_int(c_term, ctx),
            CTerm::Lambda { .. } => self.visit_lambda(c_term, ctx),
            CTerm::MemGet { .. } => self.visit_mem_get(c_term, ctx),
            CTerm::MemSet { .. } => self.visit_mem_set(c_term, ctx),
            CTerm::PrimitiveCall { .. } => self.visit_primitive_call(c_term, ctx),
            CTerm::OperationCall { .. } => self.visit_operation_call(c_term, ctx),
            CTerm::Handler { .. } => self.visit_handler(c_term, ctx),
        }
    }

    fn visit_redex(&mut self, c_term: &CTerm, ctx: Self::Ctx) {
        let CTerm::Redex { function, args } = c_term else { unreachable!() };
        self.visit_c_term(function, ctx);
        args.iter().for_each(|arg| self.visit_v_term(arg, ctx));
    }

    fn visit_return(&mut self, c_term: &CTerm, ctx: Self::Ctx) {
        let CTerm::Return { value } = c_term else { unreachable!() };
        self.visit_v_term(value, ctx);
    }

    fn visit_force(&mut self, c_term: &CTerm, ctx: Self::Ctx) {
        let CTerm::Force { thunk, .. } = c_term else { unreachable!() };
        self.visit_v_term(thunk, ctx);
    }

    fn visit_let(&mut self, c_term: &CTerm, ctx: Self::Ctx) {
        let CTerm::Let { t, body, bound_index: bound_name } = c_term else { unreachable!() };
        self.visit_c_term(t, ctx);
        self.add_binding(*bound_name, ctx);
        self.visit_c_term(body, ctx);
        self.remove_binding(*bound_name, ctx);
    }

    fn visit_def(&mut self, _c_term: &CTerm, _ctx: Self::Ctx) {}

    fn visit_case_int(&mut self, c_term: &CTerm, ctx: Self::Ctx) {
        let CTerm::CaseInt { t, branches, default_branch, .. } = c_term else { unreachable!() };
        self.visit_v_term(t, ctx);
        for (_, branch) in branches.iter() {
            self.visit_c_term(branch, ctx);
        }
        if let Some(default_branch) = default_branch {
            self.visit_c_term(default_branch, ctx);
        }
    }

    fn visit_lambda(&mut self, c_term: &CTerm, ctx: Self::Ctx) {
        let CTerm::Lambda { args, body, .. } = c_term else { unreachable!() };
        args.iter().for_each(|(arg, _)| self.add_binding(*arg, ctx));
        self.visit_c_term(body, ctx);
        args.iter().for_each(|(arg, _)| self.remove_binding(*arg, ctx));
    }

    fn visit_mem_get(&mut self, c_term: &CTerm, ctx: Self::Ctx) {
        let CTerm::MemGet { base, offset } = c_term else { unreachable!() };
        self.visit_v_term(base, ctx);
        self.visit_v_term(offset, ctx);
    }

    fn visit_mem_set(&mut self, c_term: &CTerm, ctx: Self::Ctx) {
        let CTerm::MemSet { base, offset, value } = c_term else { unreachable!() };
        self.visit_v_term(base, ctx);
        self.visit_v_term(offset, ctx);
        self.visit_v_term(value, ctx);
    }

    fn visit_primitive_call(&mut self, c_term: &CTerm, ctx: Self::Ctx) {
        let CTerm::PrimitiveCall { args, .. } = c_term else { unreachable!() };
        args.iter().for_each(|arg| self.visit_v_term(arg, ctx));
    }

    fn visit_operation_call(&mut self, c_term: &CTerm, ctx: Self::Ctx) {
        let CTerm::OperationCall { eff, args, .. } = c_term else { unreachable!() };
        self.visit_v_term(eff, ctx);
        args.iter().for_each(|arg| self.visit_v_term(arg, ctx));
    }

    fn visit_handler(&mut self, c_term: &CTerm, ctx: Self::Ctx) {
        let CTerm::Handler {
            parameter,
            parameter_disposer,
            parameter_replicator,
            transform,
            handlers,
            input
        } = c_term else { unreachable!() };
        self.visit_v_term(parameter, ctx);
        self.visit_v_term(parameter_disposer, ctx);
        self.visit_v_term(parameter_replicator, ctx);
        self.visit_v_term(transform, ctx);
        for (eff, handler, ..) in handlers.iter() {
            self.visit_v_term(eff, ctx);
            self.visit_v_term(handler, ctx);
        }
        self.visit_v_term(input, ctx);
    }
}