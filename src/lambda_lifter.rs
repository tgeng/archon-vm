use std::collections::HashSet;
use crate::signature::Signature;
use crate::term::{CTerm, VTerm};
use crate::term::CTerm::Def;
use crate::visitor::Visitor;

pub fn lift_lambdas(signature: &mut Signature) {
    let mut lifter = LambdaLifter::new();
    for (name, t) in signature.defs.iter_mut() {
        lifter.lift_lambdas_in_term(name, t);
    }
    for (name, t) in lifter.new_lambdas {
        signature.defs.insert(name, t);
    }
}

struct LambdaLifter {
    counter: i32,
    new_lambdas: Vec<(String, CTerm)>,
}

impl LambdaLifter {
    pub fn new() -> LambdaLifter {
        LambdaLifter {
            counter: 0,
            new_lambdas: Vec::new(),
        }
    }
    pub fn lift_lambdas_in_term(&mut self, name: &str, term: &mut CTerm) {
        self.visit_c_term(&name.to_string(), term);
    }
}

impl Visitor for LambdaLifter {
    type Context = String;

    fn visit_lambda(&mut self, name: &Self::Context, c_term: &mut CTerm) {
        // Process body first so that nested lambda are lifted first
        let CTerm::Lambda { arg_name, body, } = c_term else { unreachable!() };
        Self::with_bindings(name, &[arg_name], |ctx| {
            self.visit_c_term(ctx, body);
        });

        let mut free_variable_names: Vec<String> = FreeVariableFinder::find_free_variables(c_term).into_iter().collect();
        free_variable_names.sort();
        let new_lambda_name = format!("{}#{}", name, self.counter);
        self.counter += 1;
        let mut new_c_term = free_variable_names.iter().fold(Def {
            name: new_lambda_name,
        }, |f, arg_name| {
            CTerm::App {
                function: Box::new(f),
                arg: VTerm::Var {
                    name: arg_name.clone(),
                },
            }
        });
        // swap the lambda in the original term with an application of the free variables on the
        // newly lifted lambda
        std::mem::swap(c_term, &mut new_c_term);
        free_variable_names.iter().rfold(new_c_term, |body, arg_name| {
            CTerm::Lambda {
                arg_name: arg_name.clone(),
                body: Box::new(body),
            }
        });
    }
}

struct FreeVariableFinder {
    free_variables: HashSet<String>,
}

impl FreeVariableFinder {
    fn find_free_variables(term: &mut CTerm) -> HashSet<String> {
        let mut finder = FreeVariableFinder {
            free_variables: HashSet::new(),
        };
        let names_in_scope = HashSet::new();
        finder.visit_c_term(&names_in_scope, term);
        finder.free_variables
    }
}

impl Visitor for FreeVariableFinder {
    type Context = HashSet<String>;
    fn with_bindings<F>(names_in_scope: &Self::Context, names: &[&str], action: F) where F: FnOnce(&Self::Context) {
        let mut new_ctx = names_in_scope.clone();
        for name in names {
            new_ctx.insert(name.to_string());
        }
        action(&new_ctx);
    }

    fn visit_var(&mut self, names_in_scope: &Self::Context, v_term: &mut VTerm) {
        let VTerm::Var { name } = v_term else { unreachable!() };
        if !names_in_scope.contains(name) {
            self.free_variables.insert(name.clone());
        }
    }
}