use std::collections::HashMap;
use either::Either;
use phf::{phf_map};
use crate::signature::Signature;
use crate::term::{CTerm, VTerm};
use crate::u_term::{Def, UTerm};

static PRIMITIVE_ARITY: phf::Map<&'static str, u8> = phf_map! {
    "+" => 2,
    "-" => 2,
    "*" => 2,
    "/" => 2,
    "%" => 2,
    "max" => 2,
    "min" => 2,
};

struct Transpiler {
    signature: Signature,
    counter: u32,
}

impl Transpiler {
    fn transpile(&mut self, u_term: &UTerm, enclosing_def_name: &str, def_map: &HashMap<&str, &str>) -> CTerm {
        match u_term {
            UTerm::Identifier { name } => match self.transpile_identifier(name, def_map) {
                Either::Left(c) => c,
                Either::Right(v) => CTerm::Return { value: v },
            },
            UTerm::Int { value } => CTerm::Return { value: VTerm::Int { value: *value } },
            UTerm::Str { value } => CTerm::Return { value: VTerm::Str { value: value.to_string() } },
            UTerm::Tuple { values } => {
                let (transpiled_values, transpiled_computations) = self.transpile_values(values, enclosing_def_name, def_map);
                Self::squash_computations(CTerm::Return { value: VTerm::Tuple { values: transpiled_values } }, transpiled_computations)
            }
            UTerm::Lambda { arg_names, body } => {
                let lambda_name = self.new_name(enclosing_def_name);
                self.transpile(&UTerm::Defs {
                    defs: HashMap::from([(lambda_name.to_string(), Def {
                        args: arg_names.clone(),
                        block: body.clone(),
                    })]),
                    body: Some(Box::new(UTerm::Identifier { name: lambda_name })),
                }, enclosing_def_name, def_map)
            }
            UTerm::Thunk { t } => CTerm::Return { value: VTerm::Thunk { t: Box::new(self.transpile(t, enclosing_def_name, def_map)) } },
            UTerm::App { function, args } => {
                let (transpiled_args, transpiled_computations) = self.transpile_values(args, enclosing_def_name, def_map);
                let body = transpiled_args.into_iter().fold(self.transpile(function, enclosing_def_name, def_map), |f, arg| {
                    CTerm::App { function: Box::new(f), arg }
                });
                Self::squash_computations(body, transpiled_computations)
            }
            UTerm::Force { thunk } => self.transpile_value_and_map(thunk, enclosing_def_name, def_map, |t| CTerm::Force { thunk: t }),
            UTerm::CaseInt { t, branches, default_branch } => {
                let mut transpiled_branches = HashMap::new();
                for (value, branch) in branches {
                    transpiled_branches.insert(*value, self.transpile(branch, enclosing_def_name, def_map));
                }
                let transpiled_default_branch = default_branch.as_ref().map(|b| Box::new(self.transpile(b, enclosing_def_name, def_map)));
                self.transpile_value_and_map(t, enclosing_def_name, def_map, |t| {
                    CTerm::CaseInt { t, branches: transpiled_branches, default_branch: transpiled_default_branch }
                })
            }
            UTerm::CaseStr { t, branches, default_branch } => {
                let mut transpiled_branches = HashMap::new();
                for (value, branch) in branches {
                    transpiled_branches.insert(value.clone(), self.transpile(branch, enclosing_def_name, def_map));
                }
                let transpiled_default_branch = default_branch.as_ref().map(|b| Box::new(self.transpile(b, enclosing_def_name, def_map)));
                self.transpile_value_and_map(t, enclosing_def_name, def_map, |t| {
                    CTerm::CaseStr { t, branches: transpiled_branches, default_branch: transpiled_default_branch }
                })
            }
            UTerm::CaseTuple { t, names, branch } => {
                let transpiled_branch = self.transpile(branch, enclosing_def_name, def_map);
                self.transpile_value_and_map(t, enclosing_def_name, def_map, |t| {
                    CTerm::CaseTuple { t, bound_names: names.clone(), branch: Box::new(transpiled_branch) }
                })
            }
            UTerm::Let { name, t, body } => {
                let transpiled_t = self.transpile(t, enclosing_def_name, def_map);
                let transpiled_body = self.transpile(body, enclosing_def_name, def_map);
                CTerm::Let { t: Box::new(transpiled_t), bound_name: name.to_string(), body: Box::new(transpiled_body) }
            }
            UTerm::Defs { .. } => todo!(),
        }
    }

    fn transpile_value(&mut self, u_term: &UTerm, enclosing_def_name: &str, def_map: &HashMap<&str, &str>) -> (VTerm, Option<(String, CTerm)>) {
        match u_term {
            UTerm::Identifier { name } => match self.transpile_identifier(name, def_map) {
                Either::Left(c_term) => self.new_computation(c_term, enclosing_def_name),
                Either::Right(v_term) => (v_term, None),
            }
            UTerm::Int { value } => (VTerm::Int { value: *value }, None),
            UTerm::Str { value } => (VTerm::Str { value: value.clone() }, None),
            UTerm::Tuple { .. } => {
                match self.transpile(u_term, enclosing_def_name, def_map) {
                    CTerm::Return { value } => (value, None),
                    c_term => self.new_computation(c_term, enclosing_def_name),
                }
            }
            UTerm::Lambda { .. } => {
                let c_term = self.transpile(u_term, enclosing_def_name, def_map);
                (VTerm::Thunk { t: Box::new(c_term) }, None)
            }
            UTerm::Thunk { t } => {
                let c_term = self.transpile(t, enclosing_def_name, def_map);
                (VTerm::Thunk { t: Box::new(c_term) }, None)
            }
            UTerm::CaseInt { .. } |
            UTerm::CaseStr { .. } |
            UTerm::CaseTuple { .. } |
            UTerm::App { .. } |
            UTerm::Force { .. } |
            UTerm::Let { .. } |
            UTerm::Defs { .. } => {
                let c_term = self.transpile(u_term, enclosing_def_name, def_map);
                self.new_computation(c_term, enclosing_def_name)
            }
        }
    }

    fn transpile_values(&mut self, u_terms: &[UTerm], enclosing_def_name: &str, def_map: &HashMap<&str, &str>) -> (Vec<VTerm>, Vec<Option<(String, CTerm)>>) {
        u_terms.iter().map(|v| self.transpile_value(v, enclosing_def_name, def_map)).unzip()
    }

    fn transpile_value_and_map<F>(&mut self, u_term: &UTerm, enclosing_def_name: &str, def_map: &HashMap<&str, &str>, f: F) -> CTerm where F: FnOnce(VTerm) -> CTerm {
        let (v_term, computation) = self.transpile_value(u_term, enclosing_def_name, def_map);
        if let Some((name, computation)) = computation {
            CTerm::Let { t: Box::new(computation), bound_name: name, body: Box::new(f(v_term)) }
        } else {
            f(v_term)
        }
    }

    fn squash_computations(body: CTerm, computations: Vec<Option<(String, CTerm)>>) -> CTerm {
        computations.into_iter().fold(body, |c, o| {
            if let Some((name, t)) = o {
                CTerm::Let { t: Box::new(t), bound_name: name, body: Box::new(c) }
            } else {
                c
            }
        })
    }

    fn new_computation(&mut self, c_term: CTerm, enclosing_def_name: &str) -> (VTerm, Option<(String, CTerm)>) {
        let name = self.new_name(enclosing_def_name);
        (VTerm::Var { name: name.clone() }, Some((name, c_term)))
    }

    fn transpile_identifier(&self, name: &str, def_map: &HashMap<&str, &str>) -> Either<CTerm, VTerm> {
        if let Some(tuple) = PRIMITIVE_ARITY.get_entry(name) {
            let (name, arity) = tuple;
            Either::Left(CTerm::Primitive { name, arity: *arity })
        } else if let Some(def_name) = def_map.get(name) {
            Either::Left(CTerm::Def { name: def_name.to_string() })
        } else {
            Either::Right(VTerm::Var { name: name.to_string() })
        }
    }

    fn new_name(&mut self, enclosing_def_name: &str) -> String {
        let name = format!("{}#{}", enclosing_def_name, self.counter);
        self.counter += 1;
        name
    }
}