use std::cmp::Ordering;
use std::collections::{HashMap};
use crate::ast::free_var::HasFreeVar;
use crate::ast::primitive_functions::{PRIMITIVE_FUNCTIONS, PrimitiveFunction};
use crate::ast::term::{CTerm, CType, VTerm, VType};
use crate::ast::transformer::Transformer;

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub args: Vec<(usize, VType)>,
    pub body: CTerm,
    pub c_type: CType,
    /// The (exclusive) upperbound of local variables bound in this definition. This is useful to
    /// initialize an array, which can be guaranteed to fit all local variables in this function.
    pub var_bound: usize,
    /// This function may be simple (after specialization). In this case a simple version of the
    /// function is generated for faster calls.
    pub may_be_simple: bool,
}

impl FunctionDefinition {
    pub fn is_specializable(&self) -> bool {
        self.may_be_simple && matches!(self.c_type, CType::SpecializedF(_))
    }
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
        self.reduce_redundancy();
        self.specialize_calls();
        self.lift_thunks();
        // TODO: add a pass that collapse immediate force thunk pairs
        // TODO: implement a pass that uses special constructs when lifting handler transform components
        // TODO: lift all handler components (input, transform, handler ops, param ops).
    }

    fn reduce_redundancy(&mut self) {
        let mut normalizer = RedundancyRemover {};
        self.defs.iter_mut().for_each(|(_, FunctionDefinition { body, .. })| {
            normalizer.transform_c_term(body);
        });
    }


    fn specialize_calls(&mut self) {
        let mut new_defs: Vec<(String, FunctionDefinition)> = Vec::new();
        let specializable_functions: HashMap<_, _> = self.defs.iter()
            .filter_map(|(name, FunctionDefinition { args, body, c_type, var_bound, may_be_simple })| {
                if let CType::SpecializedF(_) = c_type {
                    Some((name.clone(), args.len()))
                } else {
                    None
                }
            }).collect();

        self.defs.iter_mut().for_each(|(name, FunctionDefinition { args, body, .. })| {
            let mut specializer = CallSpecializer { def_name: name, new_defs: &mut new_defs, primitive_wrapper_counter: 0, specializable_functions: &specializable_functions };
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
            let lifter = LambdaLifter { def_name: name, counter: 0, new_defs: &mut new_defs, local_var_types };
            let mut thunk_lifter = lifter;
            thunk_lifter.transform_c_term(body);
        });
        self.insert_new_defs(new_defs);
    }

    fn insert_new_defs(&mut self, new_defs: Vec<(String, FunctionDefinition)>) {
        for (name, FunctionDefinition { mut args, mut body, c_type, var_bound: mut max_arg_size, mut may_be_simple }) in new_defs.into_iter() {
            Self::rename_local_vars_in_def(&mut args, &mut body, &mut max_arg_size);
            self.insert(name, FunctionDefinition {
                args,
                body,
                c_type,
                var_bound: max_arg_size,
                may_be_simple,
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

struct RedundancyRemover {}

impl Transformer for RedundancyRemover {
    fn transform_redex(&mut self, c_term: &mut CTerm) {
        match c_term {
            CTerm::Redex { function, args } => {
                self.transform_c_term(function);
                args.iter_mut().for_each(|arg| self.transform_v_term(arg));
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

    fn transform_lambda(&mut self, c_term: &mut CTerm) {
        let CTerm::Lambda { args, box body } = c_term else { unreachable!() };
        self.transform_c_term(body);
        if let CTerm::Lambda { args: sub_args, body: box sub_body } = body {
            args.extend(sub_args.iter().copied());
            let mut placeholder = CTerm::Return { value: VTerm::Int { value: 0 } };
            std::mem::swap(&mut placeholder, sub_body);
            *body = placeholder
        }
        if args.is_empty() {
            let mut placeholder = CTerm::Return { value: VTerm::Int { value: 0 } };
            std::mem::swap(&mut placeholder, body);
            *c_term = placeholder;
        }
    }
}

struct CallSpecializer<'a> {
    def_name: &'a str,
    new_defs: &'a mut Vec<(String, FunctionDefinition)>,
    primitive_wrapper_counter: usize,
    specializable_functions: &'a HashMap<String, usize>,
}

impl<'a> Transformer for CallSpecializer<'a> {
    fn transform_redex(&mut self, c_term: &mut CTerm) {
        let CTerm::Redex { box function, args } = c_term else { unreachable!() };
        self.transform_c_term(function);
        args.iter_mut().for_each(|arg| self.transform_v_term(arg));
        let CTerm::Def { name, may_have_complex_effects: has_handler_effects } = function else { return; };
        if let Some((name, PrimitiveFunction { arg_types, return_type, .. })) = PRIMITIVE_FUNCTIONS.get_entry(name) {
            match arg_types.len().cmp(&args.len()) {
                Ordering::Greater => {
                    let primitive_wrapper_name = format!("{}$__primitive_wrapper_{}", self.def_name, self.primitive_wrapper_counter);
                    // Primitive calls cannot be effectful.
                    assert!(!*has_handler_effects);
                    *function = CTerm::Def { name: primitive_wrapper_name.clone(), may_have_complex_effects: false };
                    self.new_defs.push((primitive_wrapper_name, FunctionDefinition {
                        args: arg_types.iter().enumerate().map(|(i, t)| (i, *t)).collect(),
                        body: CTerm::PrimitiveCall {
                            name,
                            args: (0..arg_types.len()).map(|index| VTerm::Var { index }).collect(),
                        },
                        c_type: CType::SpecializedF(*return_type),
                        var_bound: arg_types.len(),
                        may_be_simple: true,
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
    }
}

struct LambdaLifter<'a> {
    def_name: &'a str,
    counter: usize,
    new_defs: &'a mut Vec<(String, FunctionDefinition)>,
    local_var_types: &'a mut [VType],
}

impl<'a> LambdaLifter<'a> {
    fn create_new_redex(&mut self, free_vars: &Vec<usize>) -> (String, CTerm) {
        let thunk_def_name = format!("{}$__lambda_{}", self.def_name, self.counter);
        self.counter += 1;

        let mut redex =
            CTerm::Redex {
                function: Box::new(CTerm::Def { name: thunk_def_name.clone(), may_have_complex_effects: true }),
                args: free_vars.iter().map(|i| VTerm::Var { index: *i }).collect(),
            };
        (thunk_def_name, redex)
    }

    fn create_new_def(&mut self, name: String, free_vars: Vec<usize>, args: &[(usize, VType)], body: CTerm) {
        let var_bound = *free_vars.iter().max().unwrap_or(&0);
        let function_definition = FunctionDefinition {
            args: free_vars.into_iter().map(|v| (v, self.local_var_types[v])).chain(args.iter().copied()).collect(),
            body,
            c_type: CType::Default,
            var_bound,
            // All thunks are treated as effectful to simplify compilation.
            // TODO: make this false after cps translation is done
            may_be_simple: true,
        };
        self.new_defs.push((name, function_definition));
    }
}

impl<'a> Transformer for LambdaLifter<'a> {
    fn transform_thunk(&mut self, v_term: &mut VTerm) {
        let VTerm::Thunk { t: box c_term } = v_term else { unreachable!() };
        self.transform_c_term(c_term);

        if let CTerm::Redex { function: box CTerm::Def { .. }, .. } | CTerm::Def { .. } = c_term {
            // There is no need to lift the thunk if it's already a simple function call.
            return;
        }

        let mut free_vars: Vec<_> = c_term.free_vars().into_iter().collect();
        free_vars.sort();

        let (thunk_def_name, mut redex) = self.create_new_redex(&free_vars);
        std::mem::swap(c_term, &mut redex);
        self.create_new_def(thunk_def_name, free_vars, &[], redex);
    }

    fn transform_lambda(&mut self, c_term: &mut CTerm) {
        let mut free_vars: Vec<_> = c_term.free_vars().iter().copied().collect();
        free_vars.sort();

        let CTerm::Lambda { args, body: box body } = c_term else { unreachable!() };
        self.transform_c_term(body);

        let (thunk_def_name, mut redex) = self.create_new_redex(&free_vars);
        let args = args.clone();
        std::mem::swap(c_term, &mut redex);

        let CTerm::Lambda { body: box body, .. } = redex else { unreachable!() };

        self.create_new_def(thunk_def_name, free_vars, &args, body);
    }
}