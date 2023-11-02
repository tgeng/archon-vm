use std::collections::{HashMap};
use crate::free_var::HasFreeVar;
use crate::term::{CTerm, VTerm};
use crate::visitor::Visitor;

pub struct Signature {
    pub defs: HashMap<String, (Vec<usize>, CTerm)>,
}

impl Signature {
    pub fn new() -> Self {
        Self {
            defs: HashMap::new(),
        }
    }

    pub fn into_defs(self) -> HashMap<String, (Vec<usize>, CTerm)> {
        self.defs
    }

    pub fn insert(&mut self, name: String, args: Vec<usize>, body: CTerm) {
        self.defs.insert(name, (args, body));
    }

    pub fn optimize(&mut self) {
        self.normalize_redex();
        self.lift_thunks();
    }

    fn normalize_redex(&mut self) {
        let mut normalizer = RedexNormalizer {};
        self.defs.iter_mut().for_each(|(_, (_, body))| {
            normalizer.visit_c_term(body);
        });
    }

    fn lift_thunks(&mut self) {
        self.rename_local_vars();
        let mut new_defs: Vec<(String, Vec<usize>, CTerm)> = Vec::new();
        self.defs.iter_mut().for_each(|(name, (_, body))| {
            let mut thunk_lifter = ThunkLifter { def_name: name, thunk_counter: 0, new_defs: &mut new_defs };
            thunk_lifter.visit_c_term(body);
        });
        for (name, args, mut body) in new_defs.into_iter() {
            Self::rename_local_vars_in_def(&args, &mut body);
            self.insert(name, args, body)
        }
    }

    fn rename_local_vars(&mut self) {
        self.defs.iter_mut().for_each(|(_, (args, body))| {
            Self::rename_local_vars_in_def(args, body);
        });
    }

    fn rename_local_vars_in_def(args: &Vec<usize>, body: &mut CTerm) {
        let mut renamer = DistinctVarRenamer { bindings: HashMap::new(), counter: 0 };
        for i in args {
            renamer.add_binding(*i);
        }
        renamer.visit_c_term(body);
    }
}

struct DistinctVarRenamer {
    bindings: HashMap<usize, Vec<usize>>,
    counter: usize,
}

impl Visitor for DistinctVarRenamer {
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

    fn visit_var(&mut self, v_term: &mut VTerm) {
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

impl Visitor for RedexNormalizer {
    fn visit_redex(&mut self, c_term: &mut CTerm) {
        match c_term {
            CTerm::Redex { function, args } => {
                self.visit_c_term(function);
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
    new_defs: &'a mut Vec<(String, Vec<usize>, CTerm)>,
}

impl<'a> Visitor for ThunkLifter<'a> {
    fn visit_thunk(&mut self, v_term: &mut VTerm) {
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

fn replace_thunk(new_defs: &mut Vec<(String, Vec<usize>, CTerm)>, thunk_def_name: String, free_vars: Vec<usize>, thunk: &mut CTerm) {
    VarNameReplacer { index_mapping: free_vars.iter().enumerate().map(|(i, v)| (*v, i)).collect() }
        .visit_c_term(thunk);
    let mut redex =
        CTerm::Redex {
            function: Box::new(CTerm::Def { name: thunk_def_name.clone() }),
            args: free_vars.iter().map(|i| VTerm::Var { index: *i }).collect(),
        };
    std::mem::swap(thunk, &mut redex);
    new_defs.push((thunk_def_name, (0..free_vars.len()).collect(), redex));
}

struct VarNameReplacer {
    index_mapping: HashMap<usize, usize>,
}

impl Visitor for VarNameReplacer {
    fn visit_var(&mut self, v_term: &mut VTerm) {
        if let VTerm::Var { index } = v_term {
            if let Some(new_index) = self.index_mapping.get(index) {
                *index = *new_index;
            }
        }
    }
}