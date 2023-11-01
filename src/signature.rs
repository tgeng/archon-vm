use std::collections::{HashMap};
use crate::free_var::HasFreeVar;
use crate::term::{CTerm, VTerm};
use crate::visitor::Visitor;

pub struct Signature {
    pub defs: HashMap<String, (Vec<String>, CTerm)>,

}

impl Signature {
    pub fn new() -> Self {
        Self {
            defs: HashMap::new(),
        }
    }

    pub fn into_defs(self) -> HashMap<String, (Vec<String>, CTerm)> {
        self.defs
    }

    pub fn insert(&mut self, name: String, args: Vec<String>, body: CTerm) {
        self.defs.insert(name, (args, body));
    }

    pub fn optimize(&mut self) {
        self.lift_thunks();
        self.normalize_redex();
    }

    fn normalize_redex(&mut self) {
        let mut normalizer = RedexNormalizer {};
        self.defs.iter_mut().for_each(|(_, (_, body))| {
            normalizer.visit_c_term(body);
        });
    }

    fn lift_thunks(&mut self) {
        let mut new_defs: Vec<(String, Vec<String>, CTerm)> = Vec::new();
        self.defs.iter_mut().for_each(|(name, (_, body))| {
            let mut thunk_lifter = ThunkLifter { def_name: name, counter: 0, new_defs: &mut new_defs };
            thunk_lifter.visit_c_term(body);
        });
        for (name, args, body) in new_defs {
            self.insert(name, args, body)
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
    counter: usize,
    new_defs: &'a mut Vec<(String, Vec<String>, CTerm)>,
}

impl<'a> Visitor for ThunkLifter<'a> {
    fn visit_thunk(&mut self, v_term: &mut VTerm) {
        let free_vars = v_term.free_vars();
        let mut arg_names: Vec<String> = free_vars.into_iter().collect();
        arg_names.sort();

        let thunk_def_name = format!("{}$__thunk_{}", self.def_name, self.counter);
        self.counter += 1;

        replace_thunk(self.new_defs, thunk_def_name, arg_names, match v_term {
            VTerm::Thunk { t } => t,
            _ => unreachable!(),
        });
    }
}

fn replace_thunk(new_defs: &mut Vec<(String, Vec<String>, CTerm)>, thunk_def_name: String, arg_names: Vec<String>, thunk: &mut CTerm) {
    let mut redex =
        CTerm::Redex {
            function: Box::new(CTerm::Def { name: thunk_def_name.clone() }),
            args: arg_names.iter().map(|arg_name| VTerm::Var { name: arg_name.clone() }).collect(),
        };
    std::mem::swap(thunk, &mut redex);
    new_defs.push((thunk_def_name, arg_names, redex));
}
