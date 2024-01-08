use std::cmp::{min, Ordering};
use std::collections::{HashMap};
use itertools::Itertools;
use crate::ast::free_var::HasFreeVar;
use crate::ast::primitive_functions::{PRIMITIVE_FUNCTIONS, PrimitiveFunction};
use crate::ast::term::{CTerm, CType, Effect, VTerm, VType};
use crate::ast::transformer::Transformer;
use crate::ast::visitor::Visitor;

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub args: Vec<(usize, VType)>,
    pub body: CTerm,
    pub c_type: CType,
    /// The (exclusive) upperbound of local variables bound in this definition. This is useful to
    /// initialize an array, which can be guaranteed to fit all local variables in this function.
    pub var_bound: usize,
    /// This function may perform simple effects and a simple CBPV version of this function is generated.
    pub need_simple: bool,
    /// This function may perform complex effects. In this case a the function is CPS transformed.
    pub need_cps: bool,
    /// This function may perform simple effects can be specialized since it returns a value and there is at least a
    /// callsite providing all the required arguments.
    pub need_specialized: bool,
}

impl FunctionDefinition {
    fn is_specializable(&self) -> bool {
        matches!(self.c_type, CType::SpecializedF(_))
    }
}

pub struct Signature {
    pub defs: HashMap<String, FunctionDefinition>,
}

pub enum FunctionEnablement {
    Specialized,
    Simple,
    Cps,
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
        self.rename_local_vars();
        self.reduce_immediate_redexes();
        self.lift_lambdas();
        self.rename_local_vars();
        self.remove_duplicate_defs();
    }

    pub fn enable(&mut self, name: &str, function_enablement: FunctionEnablement) {
        let function_definition = self.defs.get_mut(name).unwrap();
        let effect = match function_enablement {
            FunctionEnablement::Specialized => {
                if function_definition.need_specialized {
                    return;
                }
                function_definition.need_specialized = true;
                Effect::Simple
            }
            FunctionEnablement::Simple => {
                if function_definition.need_simple {
                    return;
                }
                function_definition.need_simple = true;
                Effect::Simple
            }
            FunctionEnablement::Cps => {
                if function_definition.need_cps {
                    return;
                }
                function_definition.need_cps = true;
                Effect::Complex
            }
        };

        let c_term_ptr: *const CTerm = &function_definition.body;
        // This is safe because all the writes are only one the enablement fields inside the
        // function definition. The alternative of unsafe is to clone the body or track enablement
        // in a separate data structure outside of Signature. Both are annoying and requires more
        // work.
        unsafe {
            self.visit_c_term(&*c_term_ptr, effect);
        }
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
            .filter_map(|(name, FunctionDefinition { args, c_type, need_simple: may_be_simple, .. })| {
                if let CType::SpecializedF(_) = c_type && *may_be_simple {
                    Some((name.clone(), args.len()))
                } else {
                    None
                }
            }).collect();

        self.defs.iter_mut().for_each(|(name, FunctionDefinition { body, .. })| {
            let mut specializer = CallSpecializer { def_name: name, new_defs: &mut new_defs, primitive_wrapper_counter: 0, specializable_functions: &specializable_functions };
            specializer.transform_c_term(body);
        });
        self.insert_new_defs(new_defs);
    }

    /// Assume all local variables are distinct. Also this transformation preserves this property.
    fn reduce_immediate_redexes(&mut self) {
        let mut reducer = RedexReducer {};
        self.defs.iter_mut().for_each(|(_, FunctionDefinition { body, .. })| {
            reducer.transform_c_term(body);
        });
    }

    /// Assume all local variables are distinct. Also this transformation preserves this property.
    fn lift_lambdas(&mut self) {
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
        for (name, FunctionDefinition {
            mut args,
            mut body,
            c_type,
            mut var_bound,
            need_simple: may_be_simple,
            need_cps: may_be_complex,
            need_specialized: may_be_specialized,
        }) in new_defs.into_iter() {
            Self::rename_local_vars_in_def(&mut args, &mut body, &mut var_bound);
            self.insert(name, FunctionDefinition {
                args,
                body,
                c_type,
                var_bound,
                need_simple: may_be_simple,
                need_cps: may_be_complex,
                need_specialized: may_be_specialized,
            })
        }
    }

    fn rename_local_vars(&mut self) {
        self.defs.iter_mut().for_each(|(_, FunctionDefinition { args, body, var_bound: max_arg_size, .. })| {
            Self::rename_local_vars_in_def(args, body, max_arg_size);
        });
    }

    fn remove_duplicate_defs(&mut self) {
        let mut def_replacement: HashMap<String, String> = HashMap::new();
        let mut def_content_map = HashMap::new();
        for (name, FunctionDefinition { args, body, c_type, .. }) in self.defs.iter().sorted_by_key(|(name, _)| *name) {
            let chosen_name = def_content_map.entry((args, body, c_type)).or_insert_with(|| name);
            if *chosen_name != name {
                def_replacement.insert(name.clone(), chosen_name.clone());
            }
        }
        for (name, replacement) in def_replacement.iter() {
            let function_definition = self.defs.remove(name).unwrap();
            let FunctionDefinition { need_simple, need_cps, need_specialized, .. } = function_definition;
            let replacement_function_definition = self.defs.get_mut(replacement).unwrap();
            replacement_function_definition.need_simple |= need_simple;
            replacement_function_definition.need_cps |= need_cps;
            replacement_function_definition.need_specialized |= need_specialized;
        }
        let mut def_replacer = DefReplacer { def_replacement };
        self.defs.iter_mut().for_each(|(_, FunctionDefinition { body, .. })| {
            def_replacer.transform_c_term(body);
        });
    }

    fn rename_local_vars_in_def(args: &mut [(usize, VType)], body: &mut CTerm, var_bound: &mut usize) {
        let mut renamer = DistinctVarRenamer { bindings: HashMap::new(), counter: 0 };
        for (i, _) in args.iter_mut() {
            *i = renamer.add_binding(*i);
        }
        renamer.transform_c_term(body);
        *var_bound = renamer.counter;
    }
}

// This visitor just marks functions that should be enabled recursively. It's not intended to be
// callable outside of this file.
impl Visitor for Signature {
    type Ctx = Effect;

    fn visit_thunk(&mut self, v_term: &VTerm, context_effect: Effect) {
        let VTerm::Thunk { box t, .. } = v_term else { unreachable!() };
        let empty_vec = vec![];
        let (name, args) = match t {
            CTerm::Redex { function: box CTerm::Def { name, .. }, args, } => (name, args),
            CTerm::Def { name, .. } => (name, &empty_vec),
            _ => panic!("all thunks should have been lifted at this point"),
        };
        args.iter().for_each(|arg| self.visit_v_term(arg, context_effect));
        self.enable(name, FunctionEnablement::Cps);
    }

    fn visit_redex(&mut self, c_term: &CTerm, context_effect: Effect) {
        let CTerm::Redex { box function, args } = c_term else { unreachable!() };
        args.iter().for_each(|arg| self.visit_v_term(arg, context_effect));
        let CTerm::Def { name, effect } = function else {
            self.visit_c_term(function, context_effect);
            return;
        };
        let effect = effect.intersect(context_effect);
        if effect == Effect::Complex {
            self.enable(name, FunctionEnablement::Cps);
        } else {
            let function_def = self.defs.get(name).unwrap();
            if function_def.args.len() == args.len() && function_def.is_specializable() {
                self.enable(name, FunctionEnablement::Specialized);
            } else {
                self.enable(name, FunctionEnablement::Simple);
            }
        }
    }


    fn visit_def(&mut self, c_term: &CTerm, context_effect: Effect) {
        let CTerm::Def { name, effect } = c_term else { unreachable!() };
        let effect = effect.intersect(context_effect);
        if effect == Effect::Complex {
            self.enable(name, FunctionEnablement::Cps);
        } else {
            let function_def = self.defs.get(name).unwrap();
            if function_def.args.is_empty() && function_def.is_specializable() {
                self.enable(name, FunctionEnablement::Specialized);
            } else {
                self.enable(name, FunctionEnablement::Simple);
            }
        }
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
        self.transform_redex_default(c_term);
        match c_term {
            CTerm::Redex { function, args } => {
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
        self.transform_lambda_default(c_term);
        let CTerm::Lambda { args, box body, effect } = c_term else { unreachable!() };
        if let CTerm::Lambda { args: sub_args, body: box sub_body, effect: sub_effect } = body {
            *effect = effect.union(*sub_effect);
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
        self.transform_redex_default(c_term);
        let CTerm::Redex { box function, args } = c_term else { unreachable!() };
        let CTerm::Def { name, effect } = function else { return; };
        if let Some((name, PrimitiveFunction { arg_types, return_type, .. })) = PRIMITIVE_FUNCTIONS.get_entry(name) {
            match arg_types.len().cmp(&args.len()) {
                Ordering::Greater => {
                    let primitive_wrapper_name = format!("{}$__primitive_wrapper_{}", self.def_name, self.primitive_wrapper_counter);
                    // Primitive calls cannot be effectful.
                    assert_eq!(*effect, Effect::Simple);
                    *function = CTerm::Def { name: primitive_wrapper_name.clone(), effect: Effect::Simple };
                    self.new_defs.push((primitive_wrapper_name, FunctionDefinition {
                        args: arg_types.iter().enumerate().map(|(i, t)| (i, *t)).collect(),
                        body: CTerm::PrimitiveCall {
                            name,
                            args: (0..arg_types.len()).map(|index| VTerm::Var { index }).collect(),
                        },
                        c_type: CType::SpecializedF(*return_type),
                        var_bound: arg_types.len(),
                        need_simple: false,
                        need_cps: false,
                        need_specialized: false,
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
    fn create_new_redex(&mut self, free_vars: &[usize], effect: Effect) -> (String, CTerm) {
        let thunk_def_name = format!("{}$__lambda_{}", self.def_name, self.counter);
        self.counter += 1;

        let redex =
            CTerm::Redex {
                function: Box::new(CTerm::Def { name: thunk_def_name.clone(), effect }),
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
            need_simple: false,
            need_cps: false,
            need_specialized: false,
        };
        self.new_defs.push((name, function_definition));
    }
}

impl<'a> Transformer for LambdaLifter<'a> {
    fn transform_thunk(&mut self, v_term: &mut VTerm) {
        let VTerm::Thunk { t: box c_term, effect } = v_term else { unreachable!() };
        self.transform_c_term(c_term);

        if let CTerm::Redex { function: box CTerm::Def { .. }, .. } | CTerm::Def { .. } = c_term {
            // There is no need to lift the thunk if it's already a simple function call.
            return;
        }

        let mut free_vars: Vec<_> = c_term.free_vars().into_iter().collect();
        free_vars.sort();

        let (thunk_def_name, mut redex) = self.create_new_redex(&free_vars, *effect);
        std::mem::swap(c_term, &mut redex);
        self.create_new_def(thunk_def_name, free_vars, &[], redex);
    }

    fn transform_lambda(&mut self, c_term: &mut CTerm) {
        self.transform_lambda_default(c_term);
        let mut free_vars: Vec<_> = c_term.free_vars().iter().copied().collect();
        free_vars.sort();

        let CTerm::Lambda { args, effect, .. } = c_term else { unreachable!() };

        let (thunk_def_name, mut redex) = self.create_new_redex(&free_vars, *effect);
        let args = args.clone();
        std::mem::swap(c_term, &mut redex);

        let CTerm::Lambda { box body, .. } = redex else { unreachable!() };

        self.create_new_def(thunk_def_name, free_vars, &args, body);
    }
}

struct RedexReducer {}

impl Transformer for RedexReducer {
    fn transform_redex(&mut self, c_term: &mut CTerm) {
        self.transform_redex_default(c_term);
        let CTerm::Redex { box function, args } = c_term else { unreachable!() };
        let CTerm::Lambda { args: lambda_args, body: box lambda_body, .. } = function else { return; };
        let num_args = min(args.len(), lambda_args.len());
        let matching_args = args.drain(..num_args).collect::<Vec<_>>();
        let matching_lambda_args = lambda_args.drain(..num_args).map(|(index, _)| index).collect::<Vec<_>>();
        let mut substitutor = Substitutor { bindings: HashMap::from_iter(matching_lambda_args.into_iter().zip(matching_args)) };
        substitutor.transform_c_term(lambda_body);
        RedundancyRemover {}.transform_c_term(c_term);
        // Call the reducer again to reduce the new redex. This is terminating because any loops
        // can only be introduced through recursive calls, which are not reduced by this.
        self.transform_c_term(c_term);
    }

    fn transform_force(&mut self, c_term: &mut CTerm) {
        self.transform_force_default(c_term);
        let CTerm::Force { thunk, .. } = c_term else { unreachable!() };
        let VTerm::Thunk { box t, .. } = thunk else { return; };
        // The content of placeholder does not matter here
        let mut placeholder = CTerm::Return { value: VTerm::Var { index: 0 } };
        std::mem::swap(t, &mut placeholder);
        *c_term = placeholder;
        // Call the reducer again to reduce the new redex. This is terminating because any loops
        // can only be introduced through recursive calls, which are not reduced by this.
        self.transform_c_term(c_term);
    }
}

struct Substitutor {
    bindings: HashMap<usize, VTerm>,
}

impl Transformer for Substitutor {
    fn transform_var(&mut self, v_term: &mut VTerm) {
        let VTerm::Var { index } = v_term else { unreachable!() };
        if let Some(replacement) = self.bindings.get(index) {
            *v_term = replacement.clone();
        }
    }
}

struct DefReplacer {
    def_replacement: HashMap<String, String>,
}

impl Transformer for DefReplacer {
    fn transform_def(&mut self, c_term: &mut CTerm) {
        let CTerm::Def { name, .. } = c_term else { unreachable!() };
        if let Some(replacement) = self.def_replacement.get(name.as_str()) {
            *name = replacement.clone()
        }
    }
}