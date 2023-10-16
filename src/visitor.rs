use std::collections::HashMap;
use crate::term::{CTerm, VTerm};

pub trait Visitor {
    type Context;
    type Result;

    fn combine(&self, results: Vec<Self::Result>) -> Self::Result;

    fn with_binding<'a>(&self, ctx: &'a mut Self::Context, _name: &str) -> &'a mut Self::Context {
        ctx
    }

    fn visit_v_term(&self, ctx: &mut Self::Context, v_term: &VTerm) -> Self::Result {
        match v_term {
            VTerm::Var { name } => self.visit_var(ctx, name),
            VTerm::Thunk { t } => self.visit_thunk(ctx, t),
            VTerm::Int { value } => self.visit_int(ctx, value),
            VTerm::Str { value } => self.visit_str(ctx, value),
            VTerm::Tuple { values } => self.visit_tuple(ctx, values),
        }
    }

    fn visit_var(&self, _ctx: &mut Self::Context, _name: &str) -> Self::Result {
        self.combine(vec! {})
    }
    fn visit_thunk(&self, ctx: &mut Self::Context, t: &CTerm) -> Self::Result {
        self.visit_c_term(ctx, t)
    }

    fn visit_int(&self, _ctx: &mut Self::Context, _value: &i32) -> Self::Result {
        self.combine(vec! {})
    }

    fn visit_str(&self, _ctx: &mut Self::Context, _value: &str) -> Self::Result {
        self.combine(vec! {})
    }

    fn visit_tuple(&self, ctx: &mut Self::Context, values: &[VTerm]) -> Self::Result {
        self.combine(values.iter().map(|v| self.visit_v_term(ctx, v)).collect())
    }

    fn visit_c_term(&self, ctx: &mut Self::Context, c_term: &CTerm) -> Self::Result {
        match c_term {
            CTerm::Lambda { arg_name, body } => self.visit_lambda(ctx, arg_name, body),
            CTerm::App { function, arg } => self.visit_app(ctx, function, arg),
            CTerm::Return { value } => self.visit_return(ctx, value),
            CTerm::Force { thunk } => self.visit_force(ctx, thunk),
            CTerm::Let { t, bound_name, body } => self.visit_let(ctx, t, bound_name, body),
            CTerm::Def { name } => self.visit_def(ctx, name),
            CTerm::CaseInt { t, branches, default_branch } => self.visit_case_int(ctx, t, branches, default_branch),
            CTerm::CaseTuple { t, bound_names, branch } => self.visit_case_tuple(ctx, t, bound_names, branch),
            CTerm::CaseStr { t, branches, default_branch } => self.visit_case_str(ctx, t, branches, default_branch),
            CTerm::Primitive { name, arity } => self.visit_primitive(ctx, name, arity),
        }
    }

    fn visit_lambda(&self, ctx: &mut Self::Context, arg_name: &str, body: &CTerm) -> Self::Result {
        self.visit_c_term(self.with_binding(ctx, arg_name), body)
    }

    fn visit_app(&self, ctx: &mut Self::Context, function: &CTerm, arg: &VTerm) -> Self::Result {
        self.combine(vec![
            self.visit_c_term(ctx, function),
            self.visit_v_term(ctx, arg),
        ])
    }

    fn visit_return(&self, ctx: &mut Self::Context, value: &VTerm) -> Self::Result {
        self.visit_v_term(ctx, value)
    }

    fn visit_force(&self, ctx: &mut Self::Context, thunk: &VTerm) -> Self::Result {
        self.visit_v_term(ctx, thunk)
    }
    fn visit_let(&self, ctx: &mut Self::Context, t: &CTerm, bound_name: &str, body: &CTerm) -> Self::Result {
        self.combine(vec![
            self.visit_c_term(ctx, t),
            self.visit_c_term(self.with_binding(ctx, bound_name), body),
        ])
    }
    fn visit_def(&self, _ctx: &mut Self::Context, _name: &str) -> Self::Result {
        self.combine(vec! {})
    }

    fn visit_case_int(&self, ctx: &mut Self::Context, t: &VTerm, branches: &HashMap<i32, CTerm>, default_branch: &Option<Box<CTerm>>) -> Self::Result {
        let mut results = vec![self.visit_v_term(ctx, t)];
        for branch in branches.values() {
            results.push(self.visit_c_term(ctx, branch));
        }
        if let Some(default_branch) = default_branch {
            results.push(self.visit_c_term(ctx, default_branch));
        }
        self.combine(results)
    }

    fn visit_case_tuple(&self, ctx: &mut Self::Context, t: &VTerm, bound_names: &Vec<String>, branch: &CTerm) -> Self::Result {
        let mut results = vec![self.visit_v_term(ctx, t)];
        let mut context = ctx;
        for name in bound_names {
            context = self.with_binding(context, name);
        }
        results.push(self.visit_c_term(context, branch));
        self.combine(results)
    }

    fn visit_case_str(&self, ctx: &mut Self::Context, t: &VTerm, branches: &HashMap<String, CTerm>, default_branch: &Option<Box<CTerm>>) -> Self::Result {
        let mut results = vec![self.visit_v_term(ctx, t)];
        for branch in branches.values() {
            results.push(self.visit_c_term(ctx, branch));
        }
        if let Some(default_branch) = default_branch {
            results.push(self.visit_c_term(ctx, default_branch));
        }
        self.combine(results)
    }

    fn visit_primitive(&self, _ctx: &mut Self::Context, _name: &str, _arity: &i32) -> Self::Result {
        self.combine(vec! {})
    }
}