use std::collections::{HashMap};
use crate::free_var::HasFreeVar;
use crate::term::{CTerm, VTerm};
use crate::transformer::Transformer;
use crate::visitor::Visitor;

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub args: Vec<usize>,
    pub body: CTerm,
    /// The (exclusive) upperbound of local variables bound in this definition. This is useful to
    /// initialize an array, which can be guaranteed to fit all local variables in this function.
    pub var_bound: usize,
}

pub struct Signature {
    pub defs: HashMap<String, FunctionDefinition>,
}

impl Signature {
    pub fn new() -> Self {
        Self {
            defs: HashMap::new(),
        }
    }

    pub fn into_defs(self) -> HashMap<String, FunctionDefinition> {
        self.defs
    }

    pub fn insert(&mut self, name: String, def: FunctionDefinition) {
        self.defs.insert(name, def);
    }

    pub fn optimize(&mut self) {
        self.normalize_redex();
        self.lift_thunks();
    }

    fn normalize_redex(&mut self) {
        let mut normalizer = RedexNormalizer {};
        self.defs.iter_mut().for_each(|(_, FunctionDefinition { body, .. })| {
            normalizer.transform_c_term(body);
        });
    }

    fn lift_thunks(&mut self) {
        self.rename_local_vars();
        let mut new_defs: Vec<(String, FunctionDefinition)> = Vec::new();
        self.defs.iter_mut().for_each(|(name, FunctionDefinition { body, .. })| {
            let mut thunk_lifter = ThunkLifter { def_name: name, thunk_counter: 0, new_defs: &mut new_defs };
            thunk_lifter.transform_c_term(body);
        });
        for (name, FunctionDefinition { mut args, mut body, var_bound: mut max_arg_size }) in new_defs.into_iter() {
            Self::rename_local_vars_in_def(&mut args, &mut body, &mut max_arg_size);
            self.insert(name, FunctionDefinition {
                args,
                body,
                var_bound: max_arg_size,
            })
        }
    }

    fn rename_local_vars(&mut self) {
        self.defs.iter_mut().for_each(|(_, FunctionDefinition { args, body, var_bound: max_arg_size })| {
            Self::rename_local_vars_in_def(args, body, max_arg_size);
        });
    }

    fn rename_local_vars_in_def(args: &mut [usize], body: &mut CTerm, max_arg_size: &mut usize) {
        let mut renamer = DistinctVarRenamer { bindings: HashMap::new(), counter: 0 };
        for i in args.iter_mut() {
            *i = renamer.add_binding(*i);
        }
        renamer.transform_c_term(body);
        *max_arg_size = renamer.counter;
    }
}

struct DistinctVarRenamer {
    bindings: HashMap<usize, Vec<usize>>,
    counter: usize,
}

impl Transformer for DistinctVarRenamer {
    fn add_binding(&mut self, index: usize) -> usize {
        let indexes = self.bindings.entry(index).or_default();
        let new_index = self.counter;
        indexes.push(new_index);
        self.counter += 1;
        new_index
    }

    fn remove_binding(&mut self, index: usize) {
        self.bindings.get_mut(&index).unwrap().pop();
    }

    fn transform_var(&mut self, v_term: &mut VTerm) {
        match v_term {
            VTerm::Var { index: name } => {
                if let Some(bindings) = self.bindings.get_mut(name) {
                    if !bindings.is_empty() {
                        *name = *bindings.last().unwrap();
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}

struct RedexNormalizer {}

impl Transformer for RedexNormalizer {
    fn transform_redex(&mut self, c_term: &mut CTerm) {
        match c_term {
            CTerm::Redex { function, args } => {
                self.transform_c_term(function);
                if args.is_empty() {
                    let mut placeholder = CTerm::Primitive { name: "", arity: 0 };
                    std::mem::swap(&mut placeholder, function);
                    *c_term = placeholder;
                } else {
                    let is_nested_redex = matches!(function.as_ref(), CTerm::Redex { .. });
                    if is_nested_redex {
                        let mut placeholder = CTerm::Primitive { name: "", arity: 0 };
                        std::mem::swap(&mut placeholder, c_term);
                        match placeholder {
                            CTerm::Redex { function, args } => {
                                match *function {
                                    CTerm::Redex { function: sub_function, args: sub_args } => {
                                        *c_term = CTerm::Redex { function: sub_function, args: sub_args.into_iter().chain(args).collect() };
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}

struct ThunkLifter<'a> {
    def_name: &'a str,
    thunk_counter: usize,
    new_defs: &'a mut Vec<(String, FunctionDefinition)>,
}

impl<'a> Transformer for ThunkLifter<'a> {
    fn transform_thunk(&mut self, v_term: &mut VTerm) {
        if let VTerm::Thunk { t: box CTerm::Redex { function: box CTerm::Def { .. }, .. } | box CTerm::Def { .. } } = v_term {
            // There is no need to lift the thunk if it's already a simple function call.
            return;
        }
        let mut free_vars: Vec<_> = v_term.free_vars().into_iter().collect();
        free_vars.sort();

        let thunk_def_name = format!("{}$__thunk_{}", self.def_name, self.thunk_counter);
        self.thunk_counter += 1;

        replace_thunk(self.new_defs, thunk_def_name, free_vars, match v_term {
            VTerm::Thunk { t } => t,
            _ => unreachable!(),
        });
    }
}

fn replace_thunk(new_defs: &mut Vec<(String, FunctionDefinition)>, thunk_def_name: String, free_vars: Vec<usize>, thunk: &mut CTerm) {
    VarNameReplacer { index_mapping: free_vars.iter().enumerate().map(|(i, v)| (*v, i)).collect() }
        .transform_c_term(thunk);
    let mut redex =
        CTerm::Redex {
            function: Box::new(CTerm::Def { name: thunk_def_name.clone() }),
            args: free_vars.iter().map(|i| VTerm::Var { index: *i }).collect(),
        };
    std::mem::swap(thunk, &mut redex);
    new_defs.push((thunk_def_name, FunctionDefinition {
        args: (0..free_vars.len()).collect(),
        body: redex,
        var_bound: free_vars.len(),
    }));
}

struct VarNameReplacer {
    index_mapping: HashMap<usize, usize>,
}

impl Transformer for VarNameReplacer {
    fn transform_var(&mut self, v_term: &mut VTerm) {
        if let VTerm::Var { index } = v_term {
            if let Some(new_index) = self.index_mapping.get(index) {
                *index = *new_index;
            }
        }
    }
}