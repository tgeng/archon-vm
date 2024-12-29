use crate::ast::primitive_functions::PRIMITIVE_FUNCTIONS;
use crate::ast::signature::{FunctionDefinition, Signature};
use crate::ast::term::{CTerm, CType, Effect, SpecializedType, VTerm, VType};
use crate::frontend::f_term::{Def, FTerm};
use either::{Either, Left, Right};
use std::collections::{HashMap, HashSet};

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
        let main = self.transpile_impl(
            f_term,
            &Context {
                enclosing_def_name: "",
                def_map: &HashMap::new(),
                var_map: &HashMap::new(),
            },
        );
        self.signature.insert(
            "main".to_string(),
            FunctionDefinition {
                args: vec![],
                body: main,
                c_type: CType::SpecializedF(VType::Specialized(SpecializedType::Integer)),
                var_bound: 0,
                need_simple: false,
                need_cps: false,
                need_specialized: false,
            },
        );
    }

    fn transpile_impl(&mut self, f_term: FTerm, context: &Context) -> CTerm {
        match f_term {
            FTerm::Identifier { name, effect } => {
                match self.transpile_identifier(&name, context, effect) {
                    Left(c) => c,
                    Right(v) => CTerm::Return { value: v },
                }
            }
            FTerm::Int { value } => CTerm::Return {
                value: VTerm::Int { value },
            },
            FTerm::Str { value } => CTerm::Return {
                value: VTerm::Str { value },
            },
            FTerm::Struct { values } => {
                let (transpiled_values, transpiled_computations) =
                    self.transpile_values(values, context);
                Self::squash_computations(
                    CTerm::Return {
                        value: VTerm::Struct {
                            values: transpiled_values,
                        },
                    },
                    transpiled_computations,
                )
            }
            FTerm::Lambda {
                arg_names,
                body,
                effect,
            } => {
                let mut var_map = context.var_map.clone();
                let args: Vec<_> = arg_names
                    .iter()
                    .map(|(name, v_type)| {
                        let index = self.new_local_index();
                        var_map.insert(name.clone(), index);
                        (index, *v_type)
                    })
                    .collect();
                let transpiled_body = self.transpile_impl(
                    *body,
                    &Context {
                        enclosing_def_name: context.enclosing_def_name,
                        def_map: context.def_map,
                        var_map: &var_map,
                    },
                );
                CTerm::Lambda {
                    args,
                    body: Box::new(transpiled_body),
                    effect,
                }
            }
            FTerm::Redex { function, args } => {
                let (transpiled_args, transpiled_computations) =
                    self.transpile_values(args, context);
                let body = CTerm::Redex {
                    function: Box::new(self.transpile_impl(*function, context)),
                    args: transpiled_args,
                };
                Self::squash_computations(body, transpiled_computations)
            }
            FTerm::Force { thunk, effect } => {
                self.transpile_value_and_map(*thunk, context, |(_, t)| CTerm::Force {
                    thunk: t,
                    effect,
                })
            }
            FTerm::Thunk {
                computation,
                effect,
            } => CTerm::Return {
                value: VTerm::Thunk {
                    t: Box::new(self.transpile_impl(*computation, context)),
                    effect,
                },
            },
            FTerm::CaseInt {
                t,
                result_type,
                branches,
                default_branch,
            } => {
                let mut transpiled_branches = Vec::new();
                for (value, branch) in branches {
                    transpiled_branches.push((value, self.transpile_impl(branch, context)));
                }
                let transpiled_default_branch =
                    default_branch.map(|b| Box::new(self.transpile_impl(*b, context)));
                self.transpile_value_and_map(*t, context, |(_, t)| CTerm::CaseInt {
                    t,
                    result_type,
                    branches: transpiled_branches,
                    default_branch: transpiled_default_branch,
                })
            }
            FTerm::MemGet {
                box base,
                box offset,
            } => self.transpile_value_and_map(base, context, |(s, base)| {
                s.transpile_value_and_map(offset, context, |(_, offset)| CTerm::MemGet {
                    base,
                    offset,
                })
            }),
            FTerm::MemSet {
                box base,
                box offset,
                box value,
            } => self.transpile_value_and_map(base, context, |(s, base)| {
                s.transpile_value_and_map(offset, context, |(s, offset)| {
                    s.transpile_value_and_map(value, context, |(_, value)| CTerm::MemSet {
                        base,
                        offset,
                        value,
                    })
                })
            }),
            FTerm::Let { name, t, body } => {
                let transpiled_t = self.transpile_impl(*t, context);
                let mut var_map = context.var_map.clone();
                let bound_index = self.new_local_index();
                var_map.insert(name.to_string(), bound_index);
                let transpiled_body = self.transpile_impl(
                    *body,
                    &Context {
                        enclosing_def_name: context.enclosing_def_name,
                        def_map: context.def_map,
                        var_map: &var_map,
                    },
                );
                CTerm::Let {
                    t: Box::new(transpiled_t),
                    bound_index,
                    body: Box::new(transpiled_body),
                }
            }
            FTerm::Defs { defs, body } => {
                let def_names: Vec<String> = defs.iter().map(|(s, _)| s.to_string()).collect();
                let mut def_map = context.def_map.clone();
                let def_with_names: Vec<(Def, String, Vec<String>)> = defs
                    .into_iter()
                    .map(|(name, def)| {
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
                        context.def_map.iter().for_each(|(name, _)| {
                            free_vars.remove(name.as_str());
                        });
                        let def_name = if context.enclosing_def_name.is_empty() {
                            name.clone()
                        } else {
                            format!("{}${}", context.enclosing_def_name, name)
                        };
                        let mut free_var_vec: Vec<String> =
                            free_vars.into_iter().map(|s| s.to_owned()).collect();
                        free_var_vec.sort();
                        def_map.insert(
                            name,
                            (
                                def_name.clone(),
                                free_var_vec
                                    .iter()
                                    .map(|name| VTerm::Var {
                                        index: *context.var_map.get(name).unwrap(),
                                    })
                                    .collect(),
                            ),
                        );
                        (def, def_name.clone(), free_var_vec)
                    })
                    .collect();
                def_with_names
                    .into_iter()
                    .for_each(|(def, name, free_vars)| {
                        let mut var_map = HashMap::new();
                        let bound_indexes: Vec<_> = free_vars
                            .iter()
                            .map(|v| (v.clone(), VType::Uniform))
                            .chain(def.args.clone())
                            .map(|(arg, arg_type)| {
                                let index = self.new_local_index();
                                var_map.insert(arg.clone(), index);
                                (index, arg_type)
                            })
                            .collect();
                        let def_body = self.transpile_impl(
                            *def.body,
                            &Context {
                                enclosing_def_name: &name,
                                def_map: &def_map,
                                var_map: &var_map,
                            },
                        );
                        let mut free_var_strings: Vec<String> =
                            free_vars.iter().map(|s| s.to_string()).collect();
                        free_var_strings.extend(def.args.into_iter().map(|(v, _)| v));
                        self.signature.defs.insert(
                            name.clone(),
                            FunctionDefinition {
                                args: bound_indexes,
                                body: def_body,
                                c_type: def.c_type,
                                var_bound: self.local_counter,
                                need_simple: false,
                                need_cps: false,
                                need_specialized: false,
                            },
                        );
                    });
                match body {
                    None => CTerm::Return {
                        value: VTerm::Struct { values: Vec::new() },
                    },
                    Some(body) => self.transpile_impl(
                        *body,
                        &Context {
                            enclosing_def_name: context.enclosing_def_name,
                            var_map: context.var_map,
                            def_map: &def_map,
                        },
                    ),
                }
            }
            FTerm::OperationCall {
                box eff_ins,
                op_idx,
                args,
                effect,
            } => self.transpile_value_and_map(eff_ins, context, |(s, eff_ins)| {
                let (transpiled_args, transpiled_computations) = s.transpile_values(args, context);
                let operation_call = CTerm::OperationCall {
                    eff_ins,
                    op_idx,
                    args: transpiled_args,
                    effect,
                };
                Self::squash_computations(operation_call, transpiled_computations)
            }),
            FTerm::EffCast {
                box operand,
                ops_offset,
            } => self.transpile_value_and_map(operand, context, |(_, operand)| CTerm::Return {
                value: VTerm::EffCast {
                    operand: Box::new(operand),
                    ops_offset,
                },
            }),
            FTerm::Handler {
                box parameter,
                parameter_disposer,
                parameter_replicator,
                box transform,
                handlers,
                box input,
            } => self.transpile_value_and_map(parameter, context, |(s, parameter)| {
                s.transpile_option_value_and_map(
                    parameter_disposer,
                    context,
                    |(s, parameter_disposer)| {
                        s.transpile_option_value_and_map(
                            parameter_replicator,
                            context,
                            |(s, parameter_replicator)| {
                                s.transpile_value_and_map(transform, context, |(s, transform)| {
                                    let (ip_idxes, simple_handlers, handler_types): (
                                        Vec<_>,
                                        Vec<_>,
                                        Vec<_>,
                                    ) = itertools::multiunzip(handlers);
                                    let (handlers_v, simple_handlers_c) =
                                        s.transpile_values(simple_handlers, context);
                                    s.transpile_value_and_map(input, context, |(_s, input)| {
                                        let handler = CTerm::Handler {
                                            parameter,
                                            parameter_disposer,
                                            parameter_replicator,
                                            transform,
                                            handlers: itertools::multizip((
                                                ip_idxes,
                                                handlers_v,
                                                handler_types,
                                            ))
                                            .collect(),
                                            input,
                                        };
                                        Self::squash_computations(handler, simple_handlers_c)
                                    })
                                })
                            },
                        )
                    },
                )
            }),
        }
    }

    fn transpile_value(
        &mut self,
        f_term: FTerm,
        context: &Context,
    ) -> (VTerm, Option<(usize, CTerm)>) {
        match f_term {
            FTerm::Identifier { name, effect } => {
                match self.transpile_identifier(&name, context, effect) {
                    Left(c_term) => self.new_computation(c_term),
                    Right(v_term) => (v_term, None),
                }
            }
            FTerm::Int { value } => (VTerm::Int { value }, None),
            FTerm::Str { value } => (VTerm::Str { value }, None),
            FTerm::Struct { .. } | FTerm::EffCast { .. } => {
                match self.transpile_impl(f_term, context) {
                    CTerm::Return { value } => (value, None),
                    c_term => self.new_computation(c_term),
                }
            }
            FTerm::Lambda { effect, .. } => {
                let c_term = self.transpile_impl(f_term, context);
                (
                    VTerm::Thunk {
                        t: Box::new(c_term),
                        effect,
                    },
                    None,
                )
            }
            FTerm::Thunk {
                computation,
                effect,
            } => (
                VTerm::Thunk {
                    t: Box::new(self.transpile_impl(*computation, context)),
                    effect,
                },
                None,
            ),
            FTerm::CaseInt { .. }
            | FTerm::MemGet { .. }
            | FTerm::MemSet { .. }
            | FTerm::Redex { .. }
            | FTerm::Force { .. }
            | FTerm::Let { .. }
            | FTerm::OperationCall { .. }
            | FTerm::Handler { .. }
            | FTerm::Defs { .. } => {
                let c_term = self.transpile_impl(f_term, context);
                self.new_computation(c_term)
            }
        }
    }

    fn transpile_values(
        &mut self,
        f_terms: Vec<FTerm>,
        context: &Context,
    ) -> (Vec<VTerm>, Vec<Option<(usize, CTerm)>>) {
        f_terms
            .into_iter()
            .map(|v| self.transpile_value(v, context))
            .unzip()
    }

    fn transpile_option_value_and_map<F>(
        &mut self,
        f_term: Option<Box<FTerm>>,
        context: &Context,
        f: F,
    ) -> CTerm
    where
        F: FnOnce((&mut Self, Option<VTerm>)) -> CTerm,
    {
        match f_term {
            Some(box f_term) => {
                let (v_term, computation) = self.transpile_value(f_term, context);
                if let Some((name, computation)) = computation {
                    CTerm::Let {
                        t: Box::new(computation),
                        bound_index: name,
                        body: Box::new(f((self, Some(v_term)))),
                    }
                } else {
                    f((self, Some(v_term)))
                }
            }
            None => f((self, None)),
        }
    }

    fn transpile_value_and_map<F>(&mut self, f_term: FTerm, context: &Context, f: F) -> CTerm
    where
        F: FnOnce((&mut Self, VTerm)) -> CTerm,
    {
        let (v_term, computation) = self.transpile_value(f_term, context);
        if let Some((name, computation)) = computation {
            CTerm::Let {
                t: Box::new(computation),
                bound_index: name,
                body: Box::new(f((self, v_term))),
            }
        } else {
            f((self, v_term))
        }
    }

    fn squash_computations(body: CTerm, computations: Vec<Option<(usize, CTerm)>>) -> CTerm {
        computations.into_iter().rfold(body, |c, o| {
            if let Some((bound_index, t)) = o {
                CTerm::Let {
                    t: Box::new(t),
                    bound_index,
                    body: Box::new(c),
                }
            } else {
                c
            }
        })
    }

    fn new_computation(&mut self, c_term: CTerm) -> (VTerm, Option<(usize, CTerm)>) {
        let index = self.new_local_index();
        (VTerm::Var { index }, Some((index, c_term)))
    }

    fn transpile_identifier(
        &self,
        name: &str,
        context: &Context,
        effect: Effect,
    ) -> Either<CTerm, VTerm> {
        if let Some(index) = context.var_map.get(name) {
            Right(VTerm::Var { index: *index })
        } else if let Some((name, args)) = context.def_map.get(name) {
            let term = CTerm::Redex {
                function: Box::new(CTerm::Def {
                    name: name.to_owned(),
                    effect,
                }),
                args: args.to_owned(),
            };
            Left(term.clone())
        } else if let Some(name) = PRIMITIVE_FUNCTIONS.get_key(name) {
            Left(CTerm::Def {
                name: (*name).to_owned(),
                effect,
            })
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

    fn get_free_vars<'a>(
        f_term: &'a FTerm,
        bound_names: &HashSet<&'a str>,
        free_vars: &mut HashSet<&'a str>,
    ) {
        match f_term {
            FTerm::Identifier { name, .. } => {
                if !bound_names.contains(name.as_str())
                    & &!PRIMITIVE_FUNCTIONS.contains_key(name.as_str())
                {
                    free_vars.insert(name.as_str());
                }
            }
            FTerm::Int { .. } => {}
            FTerm::Str { .. } => {}
            FTerm::Struct { values } => values
                .iter()
                .for_each(|v| Self::get_free_vars(v, bound_names, free_vars)),
            FTerm::Lambda {
                arg_names, body, ..
            } => {
                let mut new_bound_names = bound_names.clone();
                new_bound_names.extend(arg_names.iter().map(|(s, _)| s.as_str()));
                Self::get_free_vars(body, &new_bound_names, free_vars);
            }
            FTerm::Redex { function, args } => {
                Self::get_free_vars(function, bound_names, free_vars);
                args.iter()
                    .for_each(|v| Self::get_free_vars(v, bound_names, free_vars));
            }
            FTerm::Force { thunk, .. } => Self::get_free_vars(thunk, bound_names, free_vars),
            FTerm::Thunk { computation, .. } => {
                Self::get_free_vars(computation, bound_names, free_vars)
            }
            FTerm::EffCast { operand, .. } => {
                Self::get_free_vars(operand, bound_names, free_vars);
            }
            FTerm::CaseInt {
                t,
                branches,
                default_branch,
                ..
            } => {
                Self::get_free_vars(t, bound_names, free_vars);
                branches
                    .iter()
                    .for_each(|(_, v)| Self::get_free_vars(v, bound_names, free_vars));
                if let Some(default_branch) = default_branch {
                    Self::get_free_vars(default_branch, bound_names, free_vars);
                }
            }
            FTerm::MemGet { base, offset } => {
                Self::get_free_vars(base, bound_names, free_vars);
                Self::get_free_vars(offset, bound_names, free_vars);
            }
            FTerm::MemSet {
                base,
                offset,
                value,
            } => {
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
            FTerm::OperationCall {
                box eff_ins, args, ..
            } => {
                Self::get_free_vars(eff_ins, bound_names, free_vars);
                args.iter()
                    .for_each(|v| Self::get_free_vars(v, bound_names, free_vars));
            }
            FTerm::Handler {
                box parameter,
                parameter_disposer,
                parameter_replicator,
                box transform,
                handlers: simple_handlers,
                box input,
            } => {
                Self::get_free_vars(parameter, bound_names, free_vars);
                match parameter_disposer {
                    Some(box parameter_disposer) => {
                        Self::get_free_vars(parameter_disposer, bound_names, free_vars)
                    }
                    None => {}
                };
                match parameter_replicator {
                    Some(box parameter_replicator) => {
                        Self::get_free_vars(parameter_replicator, bound_names, free_vars)
                    }
                    None => {}
                };
                Self::get_free_vars(transform, bound_names, free_vars);
                simple_handlers.iter().for_each(|(_, handler, ..)| {
                    Self::get_free_vars(handler, bound_names, free_vars);
                });
                Self::get_free_vars(input, bound_names, free_vars);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::signature::{FunctionEnablement, Signature};
    use crate::backend::compiler::Compiler;
    use crate::frontend::parser::parse_f_term;
    use crate::frontend::transpiler::Transpiler;
    use cranelift_jit::JITModule;
    use std::fs;
    use std::path::PathBuf;

    fn check(test_input_path: &PathBuf) -> Result<(), String> {
        println!("checking {}", test_input_path.to_str().unwrap());
        let f_term = parse_f_term(&fs::read_to_string(test_input_path).unwrap())?;
        let mut transpiler = Transpiler {
            signature: Signature::new(),
            lambda_counter: 0,
            local_counter: 0,
        };
        transpiler.transpile(f_term.clone());
        let mut signature = transpiler.into_signature();
        signature.optimize();
        signature.enable("main", FunctionEnablement::Specialized);
        let mut defs = signature.into_defs().into_iter().collect::<Vec<_>>();
        defs.sort_by_key(|(name, _)| name.clone());

        let test_ir_path = test_input_path.with_extension("").with_extension("ir.txt");
        let expected = fs::read_to_string(&test_ir_path).unwrap_or_else(|_| "".to_owned());
        let partial_actual = format!(
            "FTerm\n========\n{:#?}\n\nDefs\n========\n{:#?}",
            f_term, defs
        );
        if expected != partial_actual {
            // Write partial actual to expected in case executing the compiled function crashes the
            // test.
            fs::write(&test_ir_path, &partial_actual).unwrap();
        }
        let test_clir_path = test_input_path
            .with_extension("")
            .with_extension("clir.txt");
        let expected = fs::read_to_string(&test_clir_path).unwrap_or_else(|_| "".to_owned());
        let mut compiler: Compiler<JITModule> = Default::default();
        let mut clir = vec![];
        compiler.compile(&defs, &mut Some(&mut clir));
        let main_func = compiler.finalize_and_get_main();

        let partial_actual = format!(
            "CLIR\n========\n{}",
            clir.iter()
                .map(|(name, clir)| format!("[{}]\n{}", name, clir))
                .collect::<Vec<_>>()
                .join("\n\n")
        );
        if expected != partial_actual {
            // Write partial actual to expected in case executing the compiled function crashes the
            // test.
            fs::write(test_clir_path, &partial_actual).unwrap();
        }

        let test_output_path = test_input_path
            .with_extension("")
            .with_extension("output.txt");
        let expected = fs::read_to_string(&test_output_path).unwrap_or_else(|_| "".to_owned());
        let result = main_func();
        let actual = format!("{}", result);
        if expected != actual {
            fs::write(test_output_path, actual).unwrap();
            Err(format!(
                "Output mismatch for {}",
                test_input_path.to_str().unwrap()
            ))
        } else {
            Ok(())
        }
    }

    fn run_test(test_name: &str) -> Result<(), String> {
        let test_file_name = format!("{}.input.txt", test_name);
        let mut resource_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        resource_dir.push("resources/frontend/transpiler_tests");
        let mut test_input_paths = fs::read_dir(resource_dir)
            .unwrap()
            .map(|r| r.unwrap().path())
            .filter(|p| p.file_name().unwrap().to_str().unwrap().eq(&test_file_name))
            .collect::<Vec<_>>();
        if test_input_paths.len() != 1 {
            return Err("No test found".to_string());
        }
        test_input_paths.sort();
        let all_results = test_input_paths
            .into_iter()
            .map(|test_input_path| {
                let result = check(&test_input_path);
                (test_input_path, result)
            })
            .filter(|(_, r)| r.is_err())
            .collect::<Vec<_>>();
        if all_results.is_empty() {
            Ok(())
        } else {
            Err(all_results
                .into_iter()
                .map(|(test_input_path, r)| {
                    format!(
                        "[{}] {}",
                        test_input_path.file_name().unwrap().to_str().unwrap(),
                        r.unwrap_err()
                    )
                })
                .collect::<Vec<_>>()
                .join("\n"))
        }
    }
    #[test]
    fn factorial() -> Result<(), String> {
        run_test("factorial")
    }
    #[test]
    fn factorial_effectful() -> Result<(), String> {
        run_test("factorial_effectful")
    }
    #[test]
    fn handler_call_simple_operation_complexly_1() -> Result<(), String> {
        run_test("handler_call_simple_operation_complexly_1")
    }
    #[test]
    fn handler_call_simple_operation_complexly_2() -> Result<(), String> {
        run_test("handler_call_simple_operation_complexly_2")
    }
    #[test]
    fn handler_complex_1() -> Result<(), String> {
        run_test("handler_complex_1")
    }
    #[test]
    fn handler_complex_2() -> Result<(), String> {
        run_test("handler_complex_2")
    }
    #[test]
    fn handler_complex_3() -> Result<(), String> {
        run_test("handler_complex_3")
    }
    #[test]
    fn handler_complex_4() -> Result<(), String> {
        run_test("handler_complex_4")
    }
    #[test]
    fn handler_complex_5_sum_0_7() -> Result<(), String> {
        run_test("handler_complex_5_sum_0_7")
    }
    #[test]
    fn handler_complex_exception_restores_execution() -> Result<(), String> {
        run_test("handler_complex_exception_restores_execution")
    }
    #[test]
    fn handler_dispose() -> Result<(), String> {
        run_test("handler_dispose")
    }
    #[test]
    fn handler_exception() -> Result<(), String> {
        run_test("handler_exception")
    }
    #[test]
    fn handler_exception_dispose() -> Result<(), String> {
        run_test("handler_exception_dispose")
    }
    #[test]
    fn handler_exception_exceptional() -> Result<(), String> {
        run_test("handler_exception_exceptional")
    }
    #[test]
    fn handler_exception_restores_execution() -> Result<(), String> {
        run_test("ler_exception_restores_execution")
    }
    #[test]
    fn handler_exception_restores_execution_exceptional() -> Result<(), String> {
        run_test("handler_exception_restores_execution_exceptional")
    }
    #[test]
    fn handler_nearby_exception_in_disposer() -> Result<(), String> {
        run_test("handler_nearby_exception_in_disposer")
    }
    #[test]
    fn handler_nested_complex() -> Result<(), String> {
        run_test("handler_nested_complex")
    }
    #[test]
    fn handler_nested_simple() -> Result<(), String> {
        run_test("handler_nested_simple")
    }
    #[test]
    fn handler_simple_1() -> Result<(), String> {
        run_test("handler_simple_1")
    }
    #[test]
    fn handler_simple_2() -> Result<(), String> {
        run_test("handler_simple_2")
    }
    #[test]
    fn handler_simple_3() -> Result<(), String> {
        run_test("handler_simple_3")
    }
    #[test]
    fn handler_simple_3_linear() -> Result<(), String> {
        run_test("handler_simple_3_linear")
    }
    #[test]
    fn handler_trivial() -> Result<(), String> {
        run_test("handler_trivial")
    }
    #[test]
    fn high_level_function() -> Result<(), String> {
        run_test("high_level_function")
    }
    #[test]
    fn high_level_function_complex_effects() -> Result<(), String> {
        run_test("high_level_function_complex_effects")
    }
    #[test]
    fn immediate_redex() -> Result<(), String> {
        run_test("immediate_redex")
    }
    #[test]
    fn lambda() -> Result<(), String> {
        run_test("lambda")
    }
    #[test]
    fn lambda_as_arg() -> Result<(), String> {
        run_test("lambda_as_arg")
    }
    #[test]
    fn local_vars_with_same_name() -> Result<(), String> {
        run_test("local_vars_with_same_name")
    }
    #[test]
    fn mem_access() -> Result<(), String> {
        run_test("mem_access")
    }
    #[test]
    fn nested_redex() -> Result<(), String> {
        run_test("nested_redex")
    }
    #[test]
    fn partial_primitive_call() -> Result<(), String> {
        run_test("partial_primitive_call")
    }
    #[test]
    fn return_computation() -> Result<(), String> {
        run_test("return_computation")
    }
    #[test]
    fn simple() -> Result<(), String> {
        run_test("simple")
    }
    #[test]
    fn simple_expression() -> Result<(), String> {
        run_test("simple_expression")
    }
    #[test]
    fn thunk() -> Result<(), String> {
        run_test("thunk")
    }
}
