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

pub struct Transpiler {
    signature: Signature,
    local_counter: u32,
    lambda_counter: u32,
}

struct Context<'a> {
    enclosing_def_name: &'a str,
    identifier_map: &'a HashMap<String, Either<CTerm, VTerm>>,
}

impl Transpiler {
    fn into_signature(self) -> Signature {
        self.signature
    }
    fn transpile(&mut self, u_term: UTerm) {
        let main = self.transpile_impl(u_term, &Context {
            enclosing_def_name: "",
            identifier_map: &HashMap::new(),
        });
        self.signature.insert("main".to_string(), Vec::new(), main);
    }

    fn transpile_impl(&mut self, u_term: UTerm, context: &Context) -> CTerm {
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
                let lambda_name = self.new_lambda_name(context.enclosing_def_name);
                self.transpile_impl(UTerm::Defs {
                    defs: HashMap::from([(lambda_name.to_string(), Def {
                        args: arg_names.clone(),
                        body: body.clone(),
                    })]),
                    body: Some(Box::new(UTerm::Identifier { name: lambda_name })),
                }, context)
            }
            UTerm::Redex { function, args } => {
                let (transpiled_args, transpiled_computations) = self.transpile_values(args, context);
                let body = CTerm::Redex { function: Box::new(self.transpile_impl(*function, context)), args: transpiled_args };
                Self::squash_computations(body, transpiled_computations)
            }
            UTerm::Force { thunk } => self.transpile_value_and_map(*thunk, context, |t| CTerm::Force { thunk: t }),
            UTerm::Thunk { computation } => CTerm::Return { value: VTerm::Thunk { t: Box::new(self.transpile_impl(*computation, context)) } },
            UTerm::CaseInt { t, branches, default_branch } => {
                let mut transpiled_branches = HashMap::new();
                for (value, branch) in branches {
                    transpiled_branches.insert(value, self.transpile_impl(branch, context));
                }
                let transpiled_default_branch = default_branch.map(|b| Box::new(self.transpile_impl(*b, context)));
                self.transpile_value_and_map(*t, context, |t| {
                    CTerm::CaseInt { t, branches: transpiled_branches, default_branch: transpiled_default_branch }
                })
            }
            UTerm::CaseStr { t, branches, default_branch } => {
                let mut transpiled_branches = HashMap::new();
                for (value, branch) in branches {
                    transpiled_branches.insert(value.clone(), self.transpile_impl(branch, context));
                }
                let transpiled_default_branch = default_branch.map(|b| Box::new(self.transpile_impl(*b, context)));
                self.transpile_value_and_map(*t, context, |t| {
                    CTerm::CaseStr { t, branches: transpiled_branches, default_branch: transpiled_default_branch }
                })
            }
            UTerm::CaseTuple { t, bound_names: names, branch } => {
                let mut identifier_map = context.identifier_map.clone();
                names.iter().for_each(|name| {
                    identifier_map.insert(name.to_string(), Either::Right(VTerm::Var { name: name.to_string() }));
                });
                let transpiled_branch = self.transpile_impl(*branch, &Context {
                    enclosing_def_name: context.enclosing_def_name,
                    identifier_map: &identifier_map,
                });
                self.transpile_value_and_map(*t, context, |t| {
                    CTerm::CaseTuple { t, bound_names: names.clone(), branch: Box::new(transpiled_branch) }
                })
            }
            UTerm::Let { name, t, body } => {
                let transpiled_t = self.transpile_impl(*t, context);
                let mut identifier_map = context.identifier_map.clone();
                identifier_map.insert(name.to_string(), Either::Right(VTerm::Var { name: name.clone() }));
                let transpiled_body = self.transpile_impl(*body, &Context {
                    enclosing_def_name: context.enclosing_def_name,
                    identifier_map: &identifier_map,
                });
                CTerm::Let { t: Box::new(transpiled_t), bound_name: name.to_string(), body: Box::new(transpiled_body) }
            }
            UTerm::Defs { defs, body } => {
                let def_names: Vec<String> = defs.keys().map(|s| s.to_string()).collect();
                let mut identifier_map_with_all_defs = context.identifier_map.clone();
                let def_with_names: Vec<(Def, String, Vec<String>)> = defs.into_iter().map(|(name, def)| {
                    let mut free_vars = HashSet::new();
                    let identifier_names: HashSet<&str> = HashSet::new();
                    Self::get_free_vars(&def.body, &identifier_names, &mut free_vars);
                    // remove all names bound inside the current def
                    def.args.iter().for_each(|v| {
                        free_vars.remove(v.as_str());
                    });
                    // remove all names matching defs bound in current scope
                    def_names.iter().for_each(|v| {
                        free_vars.remove(v.as_str());
                    });
                    // remove all names matching defs bound in parent scopes
                    context.identifier_map.iter().for_each(|(name, t)|
                        if let Either::Left(_) = t {
                            free_vars.remove(name.as_str());
                        }
                    );
                    let def_name = if context.enclosing_def_name.is_empty() {
                        name.clone()
                    } else {
                        format!("{}${}", context.enclosing_def_name, name)
                    };
                    let mut free_var_vec: Vec<String> = free_vars.into_iter().map(|s| s.to_string()).collect();
                    free_var_vec.sort();
                    let term = CTerm::Redex {
                        function: Box::new(CTerm::Def { name: def_name.clone() }),
                        args: free_var_vec.iter().map(|name| VTerm::Var { name: name.to_string() }).collect(),
                    };
                    identifier_map_with_all_defs.insert(name, Either::Left(term));
                    (def, def_name.clone(), free_var_vec)
                }).collect();
                def_with_names.into_iter().for_each(|(def, name, free_vars)| {
                    let mut identifier_map = identifier_map_with_all_defs.clone();
                    def.args.iter().for_each(|arg| {
                        identifier_map.insert(arg.clone(), Either::Right(VTerm::Var { name: arg.clone() }));
                    });
                    let def_body = self.transpile_impl(*def.body, &Context {
                        enclosing_def_name: &name,
                        identifier_map: &identifier_map,
                    });
                    let mut free_var_strings: Vec<String> = free_vars.iter().map(|s| s.to_string()).collect();
                    free_var_strings.extend(def.args.clone());
                    self.signature.defs.insert(name.clone(), (free_var_strings, def_body));
                });
                match body {
                    None => CTerm::Return { value: VTerm::Tuple { values: Vec::new() } },
                    Some(body) => self.transpile_impl(*body, &Context {
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
                Either::Left(c_term) => self.new_computation(c_term),
                Either::Right(v_term) => (v_term, None),
            }
            UTerm::Int { value } => (VTerm::Int { value }, None),
            UTerm::Str { value } => (VTerm::Str { value }, None),
            UTerm::Tuple { .. } => {
                match self.transpile_impl(u_term, context) {
                    CTerm::Return { value } => (value, None),
                    c_term => self.new_computation(c_term),
                }
            }
            UTerm::Lambda { .. } => {
                let c_term = self.transpile_impl(u_term, context);
                (VTerm::Thunk { t: Box::new(c_term) }, None)
            }
            UTerm::Thunk { computation } => {
                (VTerm::Thunk { t: Box::new(self.transpile_impl(*computation, context)) }, None)
            }
            UTerm::CaseInt { .. } |
            UTerm::CaseStr { .. } |
            UTerm::CaseTuple { .. } |
            UTerm::Redex { .. } |
            UTerm::Force { .. } |
            UTerm::Let { .. } |
            UTerm::Defs { .. } => {
                let c_term = self.transpile_impl(u_term, context);
                self.new_computation(c_term)
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

    fn new_computation(&mut self, c_term: CTerm) -> (VTerm, Option<(String, CTerm)>) {
        let name = self.new_local_name();
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

    fn new_lambda_name(&mut self, enclosing_def_name: &str) -> String {
        let name = format!("{}__lambda_{}", enclosing_def_name, self.lambda_counter);
        self.lambda_counter += 1;
        name
    }

    fn new_local_name(&mut self) -> String {
        let name = format!("__local_{}", self.local_counter);
        self.local_counter += 1;
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
            UTerm::Redex { function, args } => {
                Self::get_free_vars(function, bound_names, free_vars);
                args.iter().for_each(|v| Self::get_free_vars(v, bound_names, free_vars));
            }
            UTerm::Force { thunk } => Self::get_free_vars(thunk, bound_names, free_vars),
            UTerm::Thunk { computation } => Self::get_free_vars(computation, bound_names, free_vars),
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
                    Self::get_free_vars(&def.body, &def_bound_names, free_vars)
                });
                if let Some(body) = body {
                    Self::get_free_vars(body, &new_bound_names, free_vars);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use crate::parser::parse_u_term;
    use crate::signature::Signature;
    use crate::transpiler::Transpiler;

    fn check(test_input_path: &str, test_output_path: &str) -> Result<(), String> {
        let u_term = parse_u_term(&fs::read_to_string(test_input_path).unwrap())?;
        let mut transpiler = Transpiler {
            signature: Signature::new(),
            lambda_counter: 0,
            local_counter: 0,
        };
        transpiler.transpile(u_term.clone());
        let mut signature = transpiler.into_signature();
        signature.optimize();
        let mut defs = signature.into_defs().into_iter().collect::<Vec<_>>();
        defs.sort_by_key(|(name, _)| name.clone());

        let actual = format!("UTerm\n========\n{:#?}\n\nDefs\n========\n{:#?}", u_term, defs);
        let expected = match fs::read_to_string(test_output_path) {
            Ok(s) => s,
            Err(_) => {
                fs::write(test_output_path, actual).unwrap();
                return Err(format!("Output file {} not found", test_output_path));
            }
        };
        if expected != actual {
            fs::write(test_output_path, actual).unwrap();
            Err(format!("Output mismatch for {}", test_input_path))
        } else {
            Ok(())
        }
    }

    #[test]
    fn run_tests() -> Result<(), String> {
        let mut resource_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        resource_dir.push("resources/transpiler_tests");
        let mut test_input_paths = fs::read_dir(resource_dir)
            .unwrap()
            .map(|r| r.unwrap().path())
            .filter(|p| p.file_name().unwrap().to_str().unwrap().ends_with(".input.txt"))
            .collect::<Vec<_>>();
        test_input_paths.sort();
        let all_results = test_input_paths.into_iter().map(|test_input_path| {
            let test_output_path = test_input_path.with_extension("").with_extension("output.txt");
            check(test_input_path.to_str().unwrap(), test_output_path.to_str().unwrap())
        }).filter(|r| r.is_err()).collect::<Vec<_>>();
        if all_results.is_empty() {
            Ok(())
        } else {
            Err(all_results.into_iter().map(|r| r.unwrap_err()).collect::<Vec<_>>().join("\n"))
        }
    }
}