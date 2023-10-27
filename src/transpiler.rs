use std::collections::{HashMap, HashSet};
use either::Either;
use phf::{phf_map};
use crate::signature::Signature;
use crate::term::{CTerm, VTerm};
use crate::u_term::{Def, UTerm};

static PRIMITIVE_ARITY: phf::Map<&'static str, u8> = phf_map! {
    "_int_pos" => 2,
    "_int_neg" => 2,
    "_int_add" => 2,
    "_int_sub" => 2,
    "_int_mul" => 2,
    "_int_div" => 2,
    "_int_mod" => 2,
};

struct Transpiler {
    signature: Signature,
    counter: u32,
}

struct Context<'a> {
    enclosing_def_name: &'a str,
    identifier_map: &'a HashMap<String, Either<CTerm, VTerm>>,
}

impl Transpiler {
    fn transpile(&mut self, u_term: UTerm, context: &Context) -> CTerm {
        match u_term {
            UTerm::Identifier { name } => match self.transpile_identifier(&name, context) {
                Either::Left(c) => c,
                Either::Right(v) => CTerm::Return { value: v },
            },
            UTerm::Int { value } => CTerm::Return { value: VTerm::Int { value } },
            UTerm::Str { value } => CTerm::Return { value: VTerm::Str { value } },
            UTerm::Tuple { values } => {
                let (transpiled_values, transpiled_computations) = self.transpile_values(values, context);
                Self::squash_computations(CTerm::Return { value: VTerm::Tuple { values: transpiled_values } }, transpiled_computations)
            }
            UTerm::Lambda { arg_names, body } => {
                let lambda_name = self.new_name(context.enclosing_def_name);
                self.transpile(UTerm::Defs {
                    defs: HashMap::from([(lambda_name.to_string(), Def {
                        args: arg_names.clone(),
                        block: body.clone(),
                    })]),
                    body: Some(Box::new(UTerm::Identifier { name: lambda_name })),
                }, context)
            }
            UTerm::App { function, args } => {
                let (transpiled_args, transpiled_computations) = self.transpile_values(args, context);
                let body = transpiled_args.into_iter().fold(self.transpile(*function, context), |f, arg| {
                    CTerm::App { function: Box::new(f), arg }
                });
                Self::squash_computations(body, transpiled_computations)
            }
            UTerm::Force { thunk } => self.transpile_value_and_map(*thunk, context, |t| CTerm::Force { thunk: t }),
            UTerm::CaseInt { t, branches, default_branch } => {
                let mut transpiled_branches = HashMap::new();
                for (value, branch) in branches {
                    transpiled_branches.insert(value, self.transpile(branch, context));
                }
                let transpiled_default_branch = default_branch.map(|b| Box::new(self.transpile(*b, context)));
                self.transpile_value_and_map(*t, context, |t| {
                    CTerm::CaseInt { t, branches: transpiled_branches, default_branch: transpiled_default_branch }
                })
            }
            UTerm::CaseStr { t, branches, default_branch } => {
                let mut transpiled_branches = HashMap::new();
                for (value, branch) in branches {
                    transpiled_branches.insert(value.clone(), self.transpile(branch, context));
                }
                let transpiled_default_branch = default_branch.map(|b| Box::new(self.transpile(*b, context)));
                self.transpile_value_and_map(*t, context, |t| {
                    CTerm::CaseStr { t, branches: transpiled_branches, default_branch: transpiled_default_branch }
                })
            }
            UTerm::CaseTuple { t, bound_names: names, branch } => {
                let mut identifier_map = context.identifier_map.clone();
                names.iter().for_each(|name| {
                    identifier_map.insert(name.to_string(), Either::Right(VTerm::Var { name: name.to_string() }));
                });
                let transpiled_branch = self.transpile(*branch, &Context {
                    enclosing_def_name: context.enclosing_def_name,
                    identifier_map: &identifier_map,
                });
                self.transpile_value_and_map(*t, context, |t| {
                    CTerm::CaseTuple { t, bound_names: names.clone(), branch: Box::new(transpiled_branch) }
                })
            }
            UTerm::Let { name, t, body } => {
                let transpiled_t = self.transpile(*t, context);
                let mut identifier_map = context.identifier_map.clone();
                identifier_map.insert(name.to_string(), Either::Right(VTerm::Var { name: name.clone() }));
                let transpiled_body = self.transpile(*body, &Context {
                    enclosing_def_name: context.enclosing_def_name,
                    identifier_map: &identifier_map,
                });
                CTerm::Let { t: Box::new(transpiled_t), bound_name: name.to_string(), body: Box::new(transpiled_body) }
            }
            UTerm::Defs { defs, body } => {
                let mut identifier_map_with_all_defs = HashMap::new();
                let def_with_names: Vec<(Def, String, Vec<String>)> = defs.into_iter().map(|(name, def)| {
                    let mut free_vars = HashSet::new();
                    let identifier_names: HashSet<&str> = context.identifier_map.keys().map(|s| s.as_str()).collect();
                    Self::get_free_vars(&def.block, &identifier_names, &mut free_vars);
                    let def_name = format!("{}${}", context.enclosing_def_name, name);
                    let mut free_var_vec: Vec<String> = free_vars.into_iter().map(|s| s.to_string()).collect();
                    free_var_vec.sort();
                    identifier_map_with_all_defs.insert(name, Either::Left(free_var_vec.iter().fold(CTerm::Def { name: def_name.clone() }, |body, arg| {
                        CTerm::App {
                            function: Box::new(body),
                            arg: VTerm::Var { name: arg.to_string() },
                        }
                    })));
                    (def, def_name.clone(), free_var_vec)
                }).collect();
                def_with_names.into_iter().for_each(|(def, name, free_vars)| {
                    let mut identifier_map = context.identifier_map.clone();
                    def.args.iter().for_each(|arg| {
                        identifier_map.insert(arg.clone(), Either::Right(VTerm::Var { name: arg.clone() }));
                    });
                    let def_body = self.transpile(*def.block, &Context {
                        enclosing_def_name: &name,
                        identifier_map: &identifier_map,
                    });
                    let mut free_var_strings: Vec<String> = free_vars.iter().map(|s| s.to_string()).collect();
                    free_var_strings.extend(def.args.clone());
                    self.signature.defs.insert(name.clone(), (free_var_strings, def_body));
                });
                match body {
                    None => CTerm::Return { value: VTerm::Tuple { values: Vec::new() } },
                    Some(body) => self.transpile(*body, &Context {
                        enclosing_def_name: context.enclosing_def_name,
                        identifier_map: &identifier_map_with_all_defs,
                    })
                }
            }
        }
    }

    fn transpile_value(&mut self, u_term: UTerm, context: &Context) -> (VTerm, Option<(String, CTerm)>) {
        match u_term {
            UTerm::Identifier { name } => match self.transpile_identifier(&name, context) {
                Either::Left(c_term) => self.new_computation(c_term, context.enclosing_def_name),
                Either::Right(v_term) => (v_term, None),
            }
            UTerm::Int { value } => (VTerm::Int { value }, None),
            UTerm::Str { value } => (VTerm::Str { value }, None),
            UTerm::Tuple { .. } => {
                match self.transpile(u_term, context) {
                    CTerm::Return { value } => (value, None),
                    c_term => self.new_computation(c_term, context.enclosing_def_name),
                }
            }
            UTerm::Lambda { .. } => {
                let c_term = self.transpile(u_term, context);
                (VTerm::Thunk { t: Box::new(c_term) }, None)
            }
            UTerm::CaseInt { .. } |
            UTerm::CaseStr { .. } |
            UTerm::CaseTuple { .. } |
            UTerm::App { .. } |
            UTerm::Force { .. } |
            UTerm::Let { .. } |
            UTerm::Defs { .. } => {
                let c_term = self.transpile(u_term, context);
                self.new_computation(c_term, context.enclosing_def_name)
            }
        }
    }

    fn transpile_values(&mut self, u_terms: Vec<UTerm>, context: &Context) -> (Vec<VTerm>, Vec<Option<(String, CTerm)>>) {
        u_terms.into_iter().map(|v| self.transpile_value(v, context)).unzip()
    }

    fn transpile_value_and_map<F>(&mut self, u_term: UTerm, context: &Context, f: F) -> CTerm where F: FnOnce(VTerm) -> CTerm {
        let (v_term, computation) = self.transpile_value(u_term, context);
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

    fn transpile_identifier(&self, name: &str, context: &Context) -> Either<CTerm, VTerm> {
        if let Some(term) = context.identifier_map.get(name) {
            term.clone()
        } else if let Some(tuple) = PRIMITIVE_ARITY.get_entry(name) {
            let (name, arity) = tuple;
            Either::Left(CTerm::Primitive { name, arity: *arity })
        } else {
            // properly returning a Result is better but very annoying since that requires transposing out of various collection
            panic!("Unknown identifier: {}", name)
        }
    }

    fn new_name(&mut self, enclosing_def_name: &str) -> String {
        let name = format!("{}#{}", enclosing_def_name, self.counter);
        self.counter += 1;
        name
    }

    fn get_free_vars<'a>(u_term: &'a UTerm, bound_names: &HashSet<&'a str>, free_vars: &mut HashSet<&'a str>) {
        match u_term {
            UTerm::Identifier { name } => if !bound_names.contains(name.as_str()) && !PRIMITIVE_ARITY.contains_key(name.as_str()) {
                free_vars.insert(name.as_str());
            }
            UTerm::Int { .. } => {}
            UTerm::Str { .. } => {}
            UTerm::Tuple { values } => values.iter().for_each(|v| Self::get_free_vars(v, bound_names, free_vars)),
            UTerm::Lambda { arg_names, body } => {
                let mut new_bound_names = bound_names.clone();
                new_bound_names.extend(arg_names.iter().map(|s| s.as_str()));
                Self::get_free_vars(body, &new_bound_names, free_vars);
            }
            UTerm::App { function, args } => {
                Self::get_free_vars(function, bound_names, free_vars);
                args.iter().for_each(|v| Self::get_free_vars(v, bound_names, free_vars));
            }
            UTerm::Force { thunk } => Self::get_free_vars(thunk, bound_names, free_vars),
            UTerm::CaseInt { t, branches, default_branch } => {
                Self::get_free_vars(t, bound_names, free_vars);
                branches.values().for_each(|v| Self::get_free_vars(v, bound_names, free_vars));
                if let Some(default_branch) = default_branch {
                    Self::get_free_vars(default_branch, bound_names, free_vars);
                }
            }
            UTerm::CaseStr { t, branches, default_branch } => {
                Self::get_free_vars(t, bound_names, free_vars);
                branches.values().for_each(|v| Self::get_free_vars(v, bound_names, free_vars));
                if let Some(default_branch) = default_branch {
                    Self::get_free_vars(default_branch, bound_names, free_vars);
                }
            }
            UTerm::CaseTuple { t, bound_names: names, branch } => {
                Self::get_free_vars(t, bound_names, free_vars);
                let mut new_bound_names = bound_names.clone();
                new_bound_names.extend(names.iter().map(|s| s.as_str()));
                Self::get_free_vars(branch, &new_bound_names, free_vars);
            }
            UTerm::Let { name, t, body } => {
                Self::get_free_vars(t, bound_names, free_vars);
                let mut new_bound_names = bound_names.clone();
                new_bound_names.insert(name.as_str());
                Self::get_free_vars(body, &new_bound_names, free_vars);
            }
            UTerm::Defs { defs, body } => {
                let mut new_bound_names = bound_names.clone();
                new_bound_names.extend(defs.keys().map(|s| s.as_str()));
                defs.values().for_each(|def| {
                    let mut def_bound_names = new_bound_names.clone();
                    def_bound_names.extend(def.args.iter().map(|s| s.as_str()));
                    Self::get_free_vars(&def.block, &def_bound_names, free_vars)
                });
                if let Some(body) = body {
                    Self::get_free_vars(body, &new_bound_names, free_vars);
                }
            }
        }
    }
}