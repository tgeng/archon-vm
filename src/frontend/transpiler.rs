use std::collections::{HashMap, HashSet};
use either::{Either, Left, Right};
use crate::ast::signature::{FunctionDefinition, Signature};
use crate::ast::term::{CTerm, CType, SpecializedType, VTerm, VType};
use crate::frontend::f_term::{Def, FTerm};
use crate::ast::primitive_functions::PRIMITIVE_FUNCTIONS;

pub struct Transpiler {
    signature: Signature,
    local_counter: usize,
    lambda_counter: usize,
}

struct Context<'a> {
    enclosing_def_name: &'a str,
    def_map: &'a HashMap<String, (String, Vec<VTerm>)>,
    var_map: &'a HashMap<String, usize>,
}

impl Transpiler {
    fn into_signature(self) -> Signature {
        self.signature
    }
    fn transpile(&mut self, f_term: FTerm) {
        let main = self.transpile_impl(f_term, &Context {
            enclosing_def_name: "",
            def_map: &HashMap::new(),
            var_map: &HashMap::new(),
        });
        self.signature.insert("main".to_string(), FunctionDefinition {
            args: vec![],
            body: main,
            c_type: CType::SpecializedF(VType::Specialized(SpecializedType::Integer)),
            var_bound: 0,
            may_be_simple: false,
            may_be_complex: false,
            may_be_specialized: false,
        });
    }

    fn transpile_impl(&mut self, f_term: FTerm, context: &Context) -> CTerm {
        match f_term {
            FTerm::Identifier { name, may_have_complex_effects } =>
                match self.transpile_identifier(&name, context, may_have_complex_effects) {
                    Left(c) => c,
                    Right(v) => CTerm::Return { value: v },
                },
            FTerm::Int { value } => CTerm::Return { value: VTerm::Int { value } },
            FTerm::Str { value } => CTerm::Return { value: VTerm::Str { value } },
            FTerm::Struct { values } => {
                let (transpiled_values, transpiled_computations) = self.transpile_values(values, context);
                Self::squash_computations(CTerm::Return { value: VTerm::Struct { values: transpiled_values } }, transpiled_computations)
            }
            FTerm::Lambda { arg_names, body, may_have_complex_effects } => {
                let mut var_map = context.var_map.clone();
                let args: Vec<_> = arg_names.iter().map(|(name, v_type)| {
                    let index = self.new_local_index();
                    var_map.insert(name.clone(), index);
                    (index, *v_type)
                }).collect();
                let transpiled_body = self.transpile_impl(*body, &Context {
                    enclosing_def_name: context.enclosing_def_name,
                    def_map: context.def_map,
                    var_map: &var_map,
                });
                CTerm::Lambda { args, body: Box::new(transpiled_body), may_have_complex_effects }
            }
            FTerm::Redex { function, args } => {
                let (transpiled_args, transpiled_computations) = self.transpile_values(args, context);
                let body = CTerm::Redex { function: Box::new(self.transpile_impl(*function, context)), args: transpiled_args };
                Self::squash_computations(body, transpiled_computations)
            }
            FTerm::Force { thunk, may_have_complex_effects } =>
                self.transpile_value_and_map(*thunk, context, |(_, t)| CTerm::Force { thunk: t, may_have_complex_effects }),
            FTerm::Thunk { computation, may_have_complex_effects } =>
                CTerm::Return {
                    value: VTerm::Thunk {
                        t: Box::new(self.transpile_impl(*computation, context)),
                        may_have_complex_effects,
                    }
                },
            FTerm::CaseInt { t, result_type, branches, default_branch } => {
                let mut transpiled_branches = Vec::new();
                for (value, branch) in branches {
                    transpiled_branches.push((value, self.transpile_impl(branch, context)));
                }
                let transpiled_default_branch = default_branch.map(|b| Box::new(self.transpile_impl(*b, context)));
                self.transpile_value_and_map(*t, context, |(_, t)| {
                    CTerm::CaseInt { t, result_type, branches: transpiled_branches, default_branch: transpiled_default_branch }
                })
            }
            FTerm::MemGet { box base, box offset } => {
                self.transpile_value_and_map(base, context, |(s, base)| {
                    s.transpile_value_and_map(offset, context, |(_, offset)| {
                        CTerm::MemGet { base, offset }
                    })
                })
            }
            FTerm::MemSet { box base, box offset, box value } => {
                self.transpile_value_and_map(base, context, |(s, base)| {
                    s.transpile_value_and_map(offset, context, |(s, offset)| {
                        s.transpile_value_and_map(value, context, |(_, value)| {
                            CTerm::MemSet { base, offset, value }
                        })
                    })
                })
            }
            FTerm::Let { name, t, body } => {
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
            FTerm::Defs { defs, body } => {
                let def_names: Vec<String> = defs.iter().map(|(s, _)| s.to_string()).collect();
                let mut def_map = context.def_map.clone();
                let def_with_names: Vec<(Def, String, Vec<String>)> = defs.into_iter().map(|(name, def)| {
                    let mut free_vars = HashSet::new();
                    let identifier_names: HashSet<&str> = HashSet::new();
                    Self::get_free_vars(&def.body, &identifier_names, &mut free_vars);
                    // remove all names bound inside the current def
                    def.args.iter().for_each(|(v, _v_type)| {
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
                    def_map.insert(
                        name,
                        (
                            def_name.clone(),
                            free_var_vec
                                .iter()
                                .map(|name| VTerm::Var { index: *context.var_map.get(name).unwrap() })
                                .collect()));
                    (def, def_name.clone(), free_var_vec)
                }).collect();
                def_with_names.into_iter().for_each(|(def, name, free_vars)| {
                    let mut var_map = HashMap::new();
                    let bound_indexes: Vec<_> = free_vars.iter().map(|v| (v.clone(), VType::Uniform)).chain(def.args.clone()).map(|(arg, arg_type)| {
                        let index = self.new_local_index();
                        var_map.insert(arg.clone(), index);
                        (index, arg_type)
                    }).collect();
                    let def_body = self.transpile_impl(*def.body, &Context {
                        enclosing_def_name: &name,
                        def_map: &def_map,
                        var_map: &var_map,
                    });
                    let mut free_var_strings: Vec<String> = free_vars.iter().map(|s| s.to_string()).collect();
                    free_var_strings.extend(def.args.into_iter().map(|(v, _)| v));
                    self.signature.defs.insert(
                        name.clone(),
                        FunctionDefinition {
                            args: bound_indexes,
                            body: def_body,
                            c_type: def.c_type,
                            var_bound: self.local_counter,
                            may_be_simple: false,
                            may_be_complex: false,
                            may_be_specialized: false,
                        },
                    );
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
            FTerm::OperationCall { box eff, args, complex } => {
                self.transpile_value_and_map(eff, context, |(s, eff)| {
                    let (transpiled_args, transpiled_computations) = s.transpile_values(args, context);
                    let operation_call = CTerm::OperationCall { eff, args: transpiled_args, complex };
                    Self::squash_computations(operation_call, transpiled_computations)
                })
            }
            FTerm::Handler {
                box parameter,
                box parameter_disposer,
                box parameter_replicator,
                box transform,
                simple_handlers,
                complex_handlers,
                box input
            } => {
                self.transpile_value_and_map(parameter, context, |(s, parameter)| {
                    s.transpile_value_and_map(parameter_disposer, context, |(s, parameter_disposer)| {
                        s.transpile_value_and_map(parameter_replicator, context, |(s, parameter_replicator)| {
                            s.transpile_value_and_map(transform, context, |(s, transform)| {
                                let (simple_effs, simple_handlers): (Vec<_>, Vec<_>) = simple_handlers.into_iter().map(|(a, b)| (a, b)).unzip();
                                let (simple_effs_v, simple_effs_c) = s.transpile_values(simple_effs, context);
                                let (simple_handlers_v, simple_handlers_c) = s.transpile_values(simple_handlers, context);
                                let (complex_effs, complex_handlers): (Vec<_>, Vec<_>) = complex_handlers.into_iter().map(|(a, b)| (a, b)).unzip();
                                let (complex_effs_v, complex_effs_c) = s.transpile_values(complex_effs, context);
                                let (complex_handlers_v, complex_handlers_c) = s.transpile_values(complex_handlers, context);
                                s.transpile_value_and_map(input, context, |(s, input)| {
                                    let handler = CTerm::Handler {
                                        parameter,
                                        parameter_disposer,
                                        parameter_replicator,
                                        transform,
                                        simple_handlers: simple_effs_v.into_iter().zip(simple_handlers_v).collect(),
                                        complex_handlers: complex_effs_v.into_iter().zip(complex_handlers_v).collect(),
                                        input,
                                    };
                                    let handler = Self::squash_computations(handler, simple_effs_c);
                                    let handler = Self::squash_computations(handler, simple_handlers_c);
                                    let handler = Self::squash_computations(handler, complex_effs_c);
                                    let handler = Self::squash_computations(handler, complex_handlers_c);
                                    handler
                                })
                            })
                        })
                    })
                })
            }
        }
    }

    fn transpile_value(&mut self, f_term: FTerm, context: &Context) -> (VTerm, Option<(usize, CTerm)>) {
        match f_term {
            FTerm::Identifier { name, may_have_complex_effects } => match self.transpile_identifier(&name, context, may_have_complex_effects) {
                Left(c_term) => self.new_computation(c_term),
                Right(v_term) => (v_term, None),
            }
            FTerm::Int { value } => (VTerm::Int { value }, None),
            FTerm::Str { value } => (VTerm::Str { value }, None),
            FTerm::Struct { .. } => {
                match self.transpile_impl(f_term, context) {
                    CTerm::Return { value } => (value, None),
                    c_term => self.new_computation(c_term),
                }
            }
            FTerm::Lambda { .. } => {
                let c_term = self.transpile_impl(f_term, context);
                (VTerm::Thunk { t: Box::new(c_term), may_have_complex_effects: true }, None)
            }
            FTerm::Thunk { computation, may_have_complex_effects } => {
                (VTerm::Thunk { t: Box::new(self.transpile_impl(*computation, context)), may_have_complex_effects }, None)
            }
            FTerm::CaseInt { .. } |
            FTerm::MemGet { .. } |
            FTerm::MemSet { .. } |
            FTerm::Redex { .. } |
            FTerm::Force { .. } |
            FTerm::Let { .. } |
            FTerm::OperationCall { .. } |
            FTerm::Handler { .. } |
            FTerm::Defs { .. } => {
                let c_term = self.transpile_impl(f_term, context);
                self.new_computation(c_term)
            }
        }
    }

    fn transpile_values(&mut self, f_terms: Vec<FTerm>, context: &Context) -> (Vec<VTerm>, Vec<Option<(usize, CTerm)>>) {
        f_terms.into_iter().map(|v| self.transpile_value(v, context)).unzip()
    }

    fn transpile_value_and_map<F>(&mut self, f_term: FTerm, context: &Context, f: F) -> CTerm where F: FnOnce((&mut Self, VTerm)) -> CTerm {
        let (v_term, computation) = self.transpile_value(f_term, context);
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

    fn transpile_identifier(&self, name: &str, context: &Context, may_have_complex_effects: bool) -> Either<CTerm, VTerm> {
        if let Some(index) = context.var_map.get(name) {
            Right(VTerm::Var { index: *index })
        } else if let Some((name, args)) = context.def_map.get(name) {
            let term = CTerm::Redex {
                function: Box::new(CTerm::Def { name: name.to_owned(), may_have_complex_effects }),
                args: args.to_owned(),
            };
            Left(term.clone())
        } else if let Some(name) = PRIMITIVE_FUNCTIONS.get_key(name) {
            Left(CTerm::Def { name: (*name).to_owned(), may_have_complex_effects })
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

    fn get_free_vars<'a>(f_term: &'a FTerm, bound_names: &HashSet<&'a str>, free_vars: &mut HashSet<&'a str>) {
        match f_term {
            FTerm::Identifier { name, .. } => if !bound_names.contains(name.as_str()) && !PRIMITIVE_FUNCTIONS.contains_key(name.as_str()) {
                free_vars.insert(name.as_str());
            }
            FTerm::Int { .. } => {}
            FTerm::Str { .. } => {}
            FTerm::Struct { values } => values.iter().for_each(|v| Self::get_free_vars(v, bound_names, free_vars)),
            FTerm::Lambda { arg_names, body, .. } => {
                let mut new_bound_names = bound_names.clone();
                new_bound_names.extend(arg_names.iter().map(|(s, _)| s.as_str()));
                Self::get_free_vars(body, &new_bound_names, free_vars);
            }
            FTerm::Redex { function, args } => {
                Self::get_free_vars(function, bound_names, free_vars);
                args.iter().for_each(|v| Self::get_free_vars(v, bound_names, free_vars));
            }
            FTerm::Force { thunk, .. } => Self::get_free_vars(thunk, bound_names, free_vars),
            FTerm::Thunk { computation, .. } => Self::get_free_vars(computation, bound_names, free_vars),
            FTerm::CaseInt { t, branches, default_branch, .. } => {
                Self::get_free_vars(t, bound_names, free_vars);
                branches.iter().for_each(|(_, v)| Self::get_free_vars(v, bound_names, free_vars));
                if let Some(default_branch) = default_branch {
                    Self::get_free_vars(default_branch, bound_names, free_vars);
                }
            }
            FTerm::MemGet { base, offset } => {
                Self::get_free_vars(base, bound_names, free_vars);
                Self::get_free_vars(offset, bound_names, free_vars);
            }
            FTerm::MemSet { base, offset, value } => {
                Self::get_free_vars(base, bound_names, free_vars);
                Self::get_free_vars(offset, bound_names, free_vars);
                Self::get_free_vars(value, bound_names, free_vars);
            }
            FTerm::Let { name, t, body } => {
                Self::get_free_vars(t, bound_names, free_vars);
                let mut new_bound_names = bound_names.clone();
                new_bound_names.insert(name.as_str());
                Self::get_free_vars(body, &new_bound_names, free_vars);
            }
            FTerm::Defs { defs, body } => {
                let mut new_bound_names = bound_names.clone();
                new_bound_names.extend(defs.iter().map(|(s, _)| s.as_str()));
                defs.iter().for_each(|(_, def)| {
                    let mut def_bound_names = new_bound_names.clone();
                    def_bound_names.extend(def.args.iter().map(|(s, _)| s.as_str()));
                    Self::get_free_vars(&def.body, &def_bound_names, free_vars)
                });
                if let Some(body) = body {
                    Self::get_free_vars(body, &new_bound_names, free_vars);
                }
            }
            FTerm::OperationCall { box eff, args, .. } => {
                Self::get_free_vars(eff, bound_names, free_vars);
                args.iter().for_each(|v| Self::get_free_vars(v, bound_names, free_vars));
            }
            FTerm::Handler {
                box parameter,
                box parameter_disposer,
                box parameter_replicator,
                box transform,
                complex_handlers,
                simple_handlers,
                box input
            } => {
                Self::get_free_vars(parameter, bound_names, free_vars);
                Self::get_free_vars(parameter_disposer, bound_names, free_vars);
                Self::get_free_vars(parameter_replicator, bound_names, free_vars);
                Self::get_free_vars(transform, bound_names, free_vars);
                complex_handlers.iter().for_each(|(eff, handler)| {
                    Self::get_free_vars(eff, bound_names, free_vars);
                    Self::get_free_vars(handler, bound_names, free_vars);
                });
                simple_handlers.iter().for_each(|(eff, handler)| {
                    Self::get_free_vars(eff, bound_names, free_vars);
                    Self::get_free_vars(handler, bound_names, free_vars);
                });
                Self::get_free_vars(input, bound_names, free_vars);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use cranelift_jit::JITModule;
    use crate::backend::compiler::Compiler;
    use crate::frontend::parser::parse_f_term;
    use crate::ast::signature::{FunctionEnablement, Signature};
    use crate::frontend::transpiler::Transpiler;

    fn check(test_input_path: &str, test_output_path: &str) -> Result<(), String> {
        println!("checking {}", test_input_path);
        let f_term = parse_f_term(&fs::read_to_string(test_input_path).unwrap())?;
        let mut transpiler = Transpiler {
            signature: Signature::new(),
            lambda_counter: 0,
            local_counter: 0,
        };
        transpiler.transpile(f_term.clone());
        let mut signature = transpiler.into_signature();
        signature.optimize();
        signature.enable("main", FunctionEnablement::MayBeSpecialized);
        let mut defs = signature.into_defs().into_iter().collect::<Vec<_>>();
        defs.sort_by_key(|(name, _)| name.clone());
        let mut compiler: Compiler<JITModule> = Default::default();
        let mut clir = vec![];
        compiler.compile(&defs, &mut Some(&mut clir));
        let main_func = compiler.finalize_and_get_main();

        let expected = match fs::read_to_string(test_output_path) {
            Ok(s) => s,
            Err(_) => "".to_owned(),
        };
        let partial_actual = format!(
            "FTerm\n========\n{:#?}\n\nDefs\n========\n{:#?}\n\nCLIR\n========\n{}",
            f_term,
            defs,
            clir.iter().map(|(name, clir)| format!("[{}]\n{}", name, clir)).collect::<Vec<_>>().join("\n\n"));
        if !expected.starts_with(&partial_actual) {
            // Write partial actual to expected in case executing the compiled function crashes the
            // test.
            fs::write(test_output_path, &partial_actual).unwrap();
        }

        let result = main_func();
        let actual = format!("{}\n\nResult\n========\n{}", partial_actual, result);
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
        resource_dir.push("resources/frontend/transpiler_tests");
        let mut test_input_paths = fs::read_dir(resource_dir)
            .unwrap()
            .map(|r| r.unwrap().path())
            .filter(|p| p.file_name().unwrap().to_str().unwrap().ends_with("handler_complex_3.input.txt"))
            .collect::<Vec<_>>();
        test_input_paths.sort();
        let all_results = test_input_paths.into_iter().map(|test_input_path| {
            let test_output_path = test_input_path.with_extension("").with_extension("output.txt");
            let result = check(test_input_path.to_str().unwrap(), test_output_path.to_str().unwrap());
            (test_input_path, result)
        }).filter(|(_, r)| r.is_err()).collect::<Vec<_>>();
        if all_results.is_empty() {
            Ok(())
        } else {
            Err(all_results.into_iter().map(|(test_input_path, r)| format!("[{}] {}", test_input_path.file_name().unwrap().to_str().unwrap(), r.unwrap_err())).collect::<Vec<_>>().join("\n"))
        }
    }
}