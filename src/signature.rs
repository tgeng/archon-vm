use std::cmp::Ordering;
use std::collections::{HashMap};
use crate::free_var::HasFreeVar;
use crate::primitive_functions::{PRIMITIVE_FUNCTIONS, PrimitiveFunction};
use crate::term::{CTerm, CType, VTerm, VType};
use crate::transformer::Transformer;

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub args: Vec<(usize, VType)>,
    pub body: CTerm,
    pub c_type: CType,
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
        self.specialize_calls();
        self.lift_thunks();
        // TODO: add a pass that collapse immediate force thunk pairs
        // TODO: add a pass that converts redex to specialized function calls
    }

    fn normalize_redex(&mut self) {
        let mut normalizer = RedexNormalizer {};
        self.defs.iter_mut().for_each(|(_, FunctionDefinition { body, .. })| {
            normalizer.transform_c_term(body);
        });
    }


    fn specialize_calls(&mut self) {
        let mut new_defs: Vec<(String, FunctionDefinition)> = Vec::new();
        self.defs.iter_mut().for_each(|(name, FunctionDefinition { args, body, var_bound, .. })| {
            let mut specializer = CallSpecializer { def_name: name, new_defs: &mut new_defs, specializer_counter: 0 };
            specializer.transform_c_term(body);
        });
        self.insert_new_defs(new_defs);
    }

    fn lift_thunks(&mut self) {
        self.rename_local_vars();
        let mut new_defs: Vec<(String, FunctionDefinition)> = Vec::new();
        self.defs.iter_mut().for_each(|(name, FunctionDefinition { args, body, var_bound, .. })| {
            let local_var_types = &mut vec![VType::Uniform; *var_bound];
            for (i, ty) in args {
                local_var_types[*i] = *ty;
            }
            let lifter = ThunkLifter { def_name: name, thunk_counter: 0, new_defs: &mut new_defs, local_var_types };
            let mut thunk_lifter = lifter;
            thunk_lifter.transform_c_term(body);
        });
        self.insert_new_defs(new_defs);
    }

    fn insert_new_defs(&mut self, new_defs: Vec<(String, FunctionDefinition)>) {
        for (name, FunctionDefinition { mut args, mut body, c_type, var_bound: mut max_arg_size }) in new_defs.into_iter() {
            Self::rename_local_vars_in_def(&mut args, &mut body, &mut max_arg_size);
            self.insert(name, FunctionDefinition {
                args,
                body,
                c_type,
                var_bound: max_arg_size,
            })
        }
    }

    fn rename_local_vars(&mut self) {
        self.defs.iter_mut().for_each(|(_, FunctionDefinition { args, body, var_bound: max_arg_size, .. })| {
            Self::rename_local_vars_in_def(args, body, max_arg_size);
        });
    }
    fn rename_local_vars_in_def(args: &mut [(usize, VType)], body: &mut CTerm, max_arg_size: &mut usize) {
        let mut renamer = DistinctVarRenamer { bindings: HashMap::new(), counter: 0 };
        for (i, _) in args.iter_mut() {
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
                    let mut placeholder = CTerm::Return { value: VTerm::Int { value: 1 } };
                    std::mem::swap(&mut placeholder, function);
                    *c_term = placeholder;
                } else {
                    let is_nested_redex = matches!(function.as_ref(), CTerm::Redex { .. });
                    if is_nested_redex {
                        let mut placeholder = CTerm::Return { value: VTerm::Int { value: 1 } };
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

struct CallSpecializer<'a> {
    def_name: &'a str,
    new_defs: &'a mut Vec<(String, FunctionDefinition)>,
    specializer_counter: usize,
}

impl<'a> Transformer for CallSpecializer<'a> {
    fn transform_redex(&mut self, c_term: &mut CTerm) {
        let CTerm::Redex { box function, args } = c_term else { unreachable!() };
        let CTerm::Def { name } = function else { return; };
        if let Some((name, PrimitiveFunction { arg_types, return_type, .. })) = PRIMITIVE_FUNCTIONS.get_entry(name) {
            match arg_types.len().cmp(&args.len()) {
                Ordering::Greater => {
                    let specialized_function_name = format!("{}$__specialized_{}", self.def_name, self.specializer_counter);
                    *function = CTerm::Def { name: specialized_function_name.clone() };
                    self.new_defs.push((specialized_function_name, FunctionDefinition {
                        args: arg_types.iter().enumerate().map(|(i, t)| (i, *t)).collect(),
                        body: CTerm::PrimitiveCall {
                            name,
                            args: (0..arg_types.len()).map(|index| VTerm::Var { index }).collect(),
                        },
                        c_type: CType::SpecializedF(*return_type),
                        var_bound: arg_types.len(),
                    }))
                }
                Ordering::Equal => {
                    let CTerm::Redex { args, .. } = std::mem::replace(
                        c_term,
                        CTerm::PrimitiveCall { name, args: vec![] },
                    ) else { unreachable!() };
                    let CTerm::PrimitiveCall { args: new_args, .. } = c_term else { unreachable!() };
                    *new_args = args;
                }
                Ordering::Less => {
                    unreachable!()
                }
            }
        }
        // TODO: specialize function calls
    }
}

struct ThunkLifter<'a> {
    def_name: &'a str,
    thunk_counter: usize,
    new_defs: &'a mut Vec<(String, FunctionDefinition)>,
    local_var_types: &'a mut [VType],
}

impl<'a> ThunkLifter<'a> {
    fn replace_thunk(&mut self, free_vars: Vec<usize>, thunk: &mut CTerm) {
        let thunk_def_name = format!("{}$__thunk_{}", self.def_name, self.thunk_counter);
        self.thunk_counter += 1;

        let mut redex =
            CTerm::Redex {
                function: Box::new(CTerm::Def { name: thunk_def_name.clone() }),
                args: free_vars.iter().map(|i| VTerm::Var { index: *i }).collect(),
            };
        std::mem::swap(thunk, &mut redex);
        let var_bound = *free_vars.iter().max().unwrap_or(&0);
        let function_definition = FunctionDefinition {
            args: free_vars.into_iter().map(|v| (v, self.local_var_types[v])).collect(),
            body: redex,
            c_type: CType::Default,
            var_bound,
        };
        self.new_defs.push((thunk_def_name, function_definition));
    }
}

impl<'a> Transformer for ThunkLifter<'a> {
    fn transform_thunk(&mut self, v_term: &mut VTerm) {
        // TODO: generate thunks with more parameters for primitive calls. Note that primitive calls
        //  still need to be wrapped inside functions because thunks are always invoked via pushing
        //  args, which is not supported by primitive calls.
        if let VTerm::Thunk { t: box CTerm::Redex { function: box CTerm::Def { .. }, .. } | box CTerm::Def { .. }, .. } = v_term {
            // There is no need to lift the thunk if it's already a simple function call.
            return;
        }
        let mut free_vars: Vec<_> = v_term.free_vars().into_iter().collect();
        free_vars.sort();

        let VTerm::Thunk { t } = v_term else { unreachable!() };
        self.replace_thunk(free_vars, t);
    }
}