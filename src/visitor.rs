use crate::term::{CTerm, VTerm};

pub trait Visitor {
    type Context;

    fn with_bindings<F>(ctx: &mut Self::Context, _names: &[&str], action: F) where F: FnOnce(&mut Self::Context) {
        action(ctx)
    }

    fn visit_v_term(&mut self, ctx: &mut Self::Context, v_term: &mut VTerm) {
        match v_term {
            VTerm::Var { .. } => self.visit_var(ctx, v_term),
            VTerm::Thunk { .. } => self.visit_thunk(ctx, v_term),
            VTerm::Int { .. } => self.visit_int(ctx, v_term),
            VTerm::Str { .. } => self.visit_str(ctx, v_term),
            VTerm::Tuple { .. } => self.visit_tuple(ctx, v_term),
        }
    }

    fn visit_var(&mut self, _ctx: &mut Self::Context, _v_term: &mut VTerm) {}
    fn visit_thunk(&mut self, ctx: &mut Self::Context, v_term: &mut VTerm) {
        let VTerm::Thunk { t } = v_term else { unreachable!() };
        self.visit_c_term(ctx, t);
    }
    fn visit_int(&mut self, _ctx: &mut Self::Context, _v_term: &mut VTerm) {}
    fn visit_str(&mut self, _ctx: &mut Self::Context, _v_term: &mut VTerm) {}
    fn visit_tuple(&mut self, ctx: &mut Self::Context, v_term: &mut VTerm) {
        let VTerm::Tuple { values } = v_term else { unreachable!() };
        for v in values {
            self.visit_v_term(ctx, v);
        }
    }

    fn visit_c_term(&mut self, ctx: &mut Self::Context, c_term: &mut CTerm) {
        match c_term {
            CTerm::App { .. } => self.visit_app(ctx, c_term),
            CTerm::Return { .. } => self.visit_return(ctx, c_term),
            CTerm::Force { .. } => self.visit_force(ctx, c_term),
            CTerm::Let { .. } => self.visit_let(ctx, c_term),
            CTerm::Def { .. } => self.visit_def(ctx, c_term),
            CTerm::CaseInt { .. } => self.visit_case_int(ctx, c_term),
            CTerm::CaseTuple { .. } => self.visit_case_tuple(ctx, c_term),
            CTerm::CaseStr { .. } => self.visit_case_str(ctx, c_term),
            CTerm::Primitive { .. } => self.visit_primitive(ctx, c_term),
        }
    }

    fn visit_app(&mut self, ctx: &mut Self::Context, c_term: &mut CTerm) {
        let CTerm::App { function, arg } = c_term else { unreachable!() };
        self.visit_c_term(ctx, function);
        self.visit_v_term(ctx, arg);
    }

    fn visit_return(&mut self, ctx: &mut Self::Context, c_term: &mut CTerm) {
        let CTerm::Return { value } = c_term else { unreachable!() };
        self.visit_v_term(ctx, value);
    }

    fn visit_force(&mut self, ctx: &mut Self::Context, c_term: &mut CTerm) {
        let CTerm::Force { thunk } = c_term else { unreachable!() };
        self.visit_v_term(ctx, thunk);
    }

    fn visit_let(&mut self, ctx: &mut Self::Context, c_term: &mut CTerm) {
        let CTerm::Let { t, body, bound_name } = c_term else { unreachable!() };
        self.visit_c_term(ctx, t);
        Self::with_bindings(ctx, &[bound_name], |ctx| {
            self.visit_c_term(ctx, body);
        });
    }

    fn visit_def(&mut self, _ctx: &mut Self::Context, _c_term: &mut CTerm) {}

    fn visit_case_int(&mut self, ctx: &mut Self::Context, c_term: &mut CTerm) {
        let CTerm::CaseInt { t, branches, default_branch } = c_term else { unreachable!() };
        self.visit_v_term(ctx, t);
        for (_, branch) in branches.iter_mut() {
            self.visit_c_term(ctx, branch);
        }
        if let Some(default_branch) = default_branch {
            self.visit_c_term(ctx, default_branch);
        }
    }

    fn visit_case_tuple(&mut self, ctx: &mut Self::Context, c_term: &mut CTerm) {
        let CTerm::CaseTuple { t, bound_names, branch } = c_term else { unreachable!() };
        self.visit_v_term(ctx, t);
        let bound_names_refs: Vec<&str> = bound_names.iter().map(|s| s.as_str()).collect();
        Self::with_bindings(ctx, &bound_names_refs, |ctx| {
            self.visit_c_term(ctx, branch);
        });
    }

    fn visit_case_str(&mut self, ctx: &mut Self::Context, c_term: &mut CTerm) {
        let CTerm::CaseStr { t, branches, default_branch } = c_term else { unreachable!() };
        self.visit_v_term(ctx, t);
        for (_, branch) in branches.iter_mut() {
            self.visit_c_term(ctx, branch);
        }
        if let Some(default_branch) = default_branch {
            self.visit_c_term(ctx, default_branch);
        }
    }

    fn visit_primitive(&mut self, _ctx: &mut Self::Context, _c_term: &mut CTerm) {}
}