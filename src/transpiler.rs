use std::collections::{HashMap, HashSet};
use either::{Either, Left, Right};
use phf::{phf_set};
use crate::signature::{FunctionDefinition, Signature};
use crate::term::{CTerm, VTerm};
use crate::u_term::{Def, UTerm};

static PRIMITIVE_ARITY: phf::Set<&'static str> = phf_set! {
    "_int_pos",
    "_int_neg",
    "_int_add",
    "_int_sub",
    "_int_mul",
    "_int_div",
    "_int_mod",
};

pub struct Transpiler {
    signature: Signature,
    local_counter: usize,
    lambda_counter: usize,
}

struct Context<'a> {
    enclosing_def_name: &'a str,
    def_map: &'a HashMap<String, CTerm>,
    var_map: &'a HashMap<String, usize>,
}

impl Transpiler {
    fn into_signature(self) -> Signature {
        self.signature
    }
    fn transpile(&mut self, u_term: UTerm) {
        let main = self.transpile_impl(u_term, &Context {
            enclosing_def_name: "",
            def_map: &HashMap::new(),
            var_map: &HashMap::new(),
        });
        self.signature.insert("main".to_string(), FunctionDefinition {
            args: vec![],
            body: main,
            var_bound: 0,
        });
    }

    fn transpile_impl(&mut self, u_term: UTerm, context: &Context) -> CTerm {
        match u_term {
            UTerm::Identifier { name } => match self.transpile_identifier(&name, context) {
                Left(c) => c,
                Right(v) => CTerm::Return { value: v },
            },
            UTerm::Int { value } => CTerm::Return { value: VTerm::Int { value } },
            UTerm::Str { value } => CTerm::Return { value: VTerm::Str { value } },
            UTerm::Struct { values } => {
                let (transpiled_values, transpiled_computations) = self.transpile_values(values, context);
                Self::squash_computations(CTerm::Return { value: VTerm::Struct { values: transpiled_values } }, transpiled_computations)
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
            UTerm::Force { thunk } => self.transpile_value_and_map(*thunk, context, |(_, t)| CTerm::Force { thunk: t }),
            UTerm::Thunk { computation } => CTerm::Return { value: VTerm::Thunk { t: Box::new(self.transpile_impl(*computation, context)) } },
            UTerm::CaseInt { t, branches, default_branch } => {
                let mut transpiled_branches = HashMap::new();
                for (value, branch) in branches {
                    transpiled_branches.insert(value, self.transpile_impl(branch, context));
                }
                let transpiled_default_branch = default_branch.map(|b| Box::new(self.transpile_impl(*b, context)));
                self.transpile_value_and_map(*t, context, |(_, t)| {
                    CTerm::CaseInt { t, branches: transpiled_branches, default_branch: transpiled_default_branch }
                })
            }
            UTerm::MemGet { box base, box offset } => {
                self.transpile_value_and_map(base, context, |(s, base)| {
                    s.transpile_value_and_map(offset, context, |(_, offset)| {
                        CTerm::MemGet { base, offset }
                    })
                })
            }
            UTerm::MemSet { box base, box offset, box value } => {
                self.transpile_value_and_map(base, context, |(s, base)| {
                    s.transpile_value_and_map(offset, context, |(s, offset)| {
                        s.transpile_value_and_map(value, context, |(_, value)| {
                            CTerm::MemSet { base, offset, value }
                        })
                    })
                })
            }
            UTerm::Let { name, t, body } => {
                let transpiled_t = self.transpile_impl(*t, context);
                let mut var_map = context.var_map.clone();
                let bound_index = self.new_local_index();
                var_map.insert(name.to_string(), bound_index);
                let transpiled_body = self.transpile_impl(*body, &Context {
                    enclosing_def_name: context.enclosing_def_name,
                    def_map: context.def_map,
                    var_map: &var_map,
                });
                CTerm::Let { t: Box::new(transpiled_t), bound_index, body: Box::new(transpiled_body) }
            }
            UTerm::Defs { defs, body } => {
                let def_names: Vec<String> = defs.keys().map(|s| s.to_string()).collect();
                let mut def_map = context.def_map.clone();
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
                    context.def_map.iter().for_each(|(name, _)| { free_vars.remove(name.as_str()); });
                    let def_name = if context.enclosing_def_name.is_empty() {
                        name.clone()
                    } else {
                        format!("{}${}", context.enclosing_def_name, name)
                    };
                    let mut free_var_vec: Vec<String> = free_vars.into_iter().map(|s| s.to_owned()).collect();
                    free_var_vec.sort();
                    let term = CTerm::Redex {
                        function: Box::new(CTerm::Def { name: def_name.clone() }),
                        args: free_var_vec.iter().map(|name| VTerm::Var { index: *context.var_map.get(name).unwrap() }).collect(),
                    };
                    def_map.insert(name, term);
                    (def, def_name.clone(), free_var_vec)
                }).collect();
                def_with_names.into_iter().for_each(|(def, name, free_vars)| {
                    let mut var_map = HashMap::new();
                    let bound_indexes: Vec<_> = free_vars.iter().chain(def.args.iter()).map(|arg| {
                        let index = self.new_local_index();
                        var_map.insert(arg.clone(), index);
                        index
                    }).collect();
                    let def_body = self.transpile_impl(*def.body, &Context {
                        enclosing_def_name: &name,
                        def_map: &def_map,
                        var_map: &var_map,
                    });
                    let mut free_var_strings: Vec<String> = free_vars.iter().map(|s| s.to_string()).collect();
                    free_var_strings.extend(def.args.clone());
                    self.signature.defs.insert(name.clone(), FunctionDefinition { args: bound_indexes, body: def_body, var_bound: self.local_counter });
                });
                match body {
                    None => CTerm::Return { value: VTerm::Struct { values: Vec::new() } },
                    Some(body) => self.transpile_impl(*body, &Context {
                        enclosing_def_name: context.enclosing_def_name,
                        var_map: context.var_map,
                        def_map: &def_map,
                    })
                }
            }
        }
    }

    fn transpile_value(&mut self, u_term: UTerm, context: &Context) -> (VTerm, Option<(usize, CTerm)>) {
        match u_term {
            UTerm::Identifier { name } => match self.transpile_identifier(&name, context) {
                Left(c_term) => self.new_computation(c_term),
                Right(v_term) => (v_term, None),
            }
            UTerm::Int { value } => (VTerm::Int { value }, None),
            UTerm::Str { value } => (VTerm::Str { value }, None),
            UTerm::Struct { .. } => {
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
            UTerm::MemGet { .. } |
            UTerm::MemSet { .. } |
            UTerm::Redex { .. } |
            UTerm::Force { .. } |
            UTerm::Let { .. } |
            UTerm::Defs { .. } => {
                let c_term = self.transpile_impl(u_term, context);
                self.new_computation(c_term)
            }
        }
    }

    fn transpile_values(&mut self, u_terms: Vec<UTerm>, context: &Context) -> (Vec<VTerm>, Vec<Option<(usize, CTerm)>>) {
        u_terms.into_iter().map(|v| self.transpile_value(v, context)).unzip()
    }

    fn transpile_value_and_map<F>(&mut self, u_term: UTerm, context: &Context, f: F) -> CTerm where F: FnOnce((&mut Self, VTerm)) -> CTerm {
        let (v_term, computation) = self.transpile_value(u_term, context);
        if let Some((name, computation)) = computation {
            CTerm::Let { t: Box::new(computation), bound_index: name, body: Box::new(f((self, v_term))) }
        } else {
            f((self, v_term))
        }
    }

    fn squash_computations(body: CTerm, computations: Vec<Option<(usize, CTerm)>>) -> CTerm {
        computations.into_iter().rfold(body, |c, o| {
            if let Some((bound_index, t)) = o {
                CTerm::Let { t: Box::new(t), bound_index, body: Box::new(c) }
            } else {
                c
            }
        })
    }

    fn new_computation(&mut self, c_term: CTerm) -> (VTerm, Option<(usize, CTerm)>) {
        let index = self.new_local_index();
        (VTerm::Var { index }, Some((index, c_term)))
    }

    fn transpile_identifier(&self, name: &str, context: &Context) -> Either<CTerm, VTerm> {
        if let Some(term) = context.def_map.get(name) {
            Left(term.clone())
        } else if let Some(index) = context.var_map.get(name) {
            Right(VTerm::Var { index: *index })
        } else if let Some(name) = PRIMITIVE_ARITY.get_key(name) {
            Left(CTerm::Primitive { name })
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

    fn new_local_index(&mut self) -> usize {
        let new_local_index = self.local_counter;
        self.local_counter += 1;
        new_local_index
    }

    fn get_free_vars<'a>(u_term: &'a UTerm, bound_names: &HashSet<&'a str>, free_vars: &mut HashSet<&'a str>) {
        match u_term {
            UTerm::Identifier { name } => if !bound_names.contains(name.as_str()) && !PRIMITIVE_ARITY.contains(name.as_str()) {
                free_vars.insert(name.as_str());
            }
            UTerm::Int { .. } => {}
            UTerm::Str { .. } => {}
            UTerm::Struct { values } => values.iter().for_each(|v| Self::get_free_vars(v, bound_names, free_vars)),
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
            UTerm::MemGet { base, offset } => {
                Self::get_free_vars(base, bound_names, free_vars);
                Self::get_free_vars(offset, bound_names, free_vars);
            }
            UTerm::MemSet { base, offset, value } => {
                Self::get_free_vars(base, bound_names, free_vars);
                Self::get_free_vars(offset, bound_names, free_vars);
                Self::get_free_vars(value, bound_names, free_vars);
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