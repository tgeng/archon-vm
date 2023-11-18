use std::collections::HashMap;
use std::iter;
use cranelift::codegen::ir::{FuncRef, Inst, StackSlot};
use cranelift::frontend::Switch;
use cranelift::prelude::*;
use cranelift::prelude::types::{F32, I32, I64};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use crate::ast::term::{CTerm, VTerm, VType, SpecializedType, PType, CType};
use enum_map::{EnumMap};
use VType::{Specialized, Uniform};
use SpecializedType::{Integer, PrimitivePtr, StructPtr};
use crate::backend::common::{BuiltinFunction, HasType, TypedReturnValue};
use crate::ast::primitive_functions::PRIMITIVE_FUNCTIONS;

pub struct FunctionTranslator<'a, M: Module> {
    pub module: &'a mut M,
    pub function_builder: FunctionBuilder<'a>,
    pub data_description: &'a mut DataDescription,
    pub builtin_functions: &'a EnumMap<BuiltinFunction, FuncId>,
    pub static_strings: &'a mut HashMap<String, DataId>,
    pub local_functions: &'a HashMap<String, FuncId>,
    pub local_vars: &'a mut [TypedReturnValue],
    pub base_address: Value,
    pub tip_address: Value,
    pub num_args: usize,
    pub uniform_func_signature: &'a Signature,
    pub tip_address_slot: StackSlot,
    pub local_function_arg_types: &'a HashMap<String, (Vec<VType>, CType)>,
    pub is_specialized: bool,
}

impl<'a, M: Module> FunctionTranslator<'a, M> {
    pub fn translate_c_term(&mut self, c_term: &CTerm, is_tail: bool) -> TypedReturnValue {
        match c_term {
            CTerm::Redex { box function, args } => {
                let arg_values = args.iter().map(|arg| {
                    let v = self.translate_v_term(arg);
                    self.convert_to_uniform(v)
                }).collect::<Vec<_>>();
                self.push_args(arg_values);
                self.translate_c_term(function, is_tail)
            }
            CTerm::Return { value } => self.translate_v_term(value),
            CTerm::Force { thunk, may_have_complex_effects } => {
                let thunk_value = self.translate_v_term(thunk);
                // We must change the thunk value to uniform representation because the built-in
                // function expects a uniform representation in order to tell a thunk from a raw
                // function pointer.
                let thunk_value = self.convert_to_uniform(thunk_value);

                self.function_builder.ins().stack_store(self.tip_address, self.tip_address_slot, 0);
                let tip_address_ptr = self.function_builder.ins().stack_addr(I64, self.tip_address_slot, 0);
                let inst = self.call_builtin_func(BuiltinFunction::ForceThunk, &[thunk_value, tip_address_ptr]);
                let func_pointer = self.function_builder.inst_results(inst)[0];
                self.tip_address = self.function_builder.ins().stack_load(I64, self.tip_address_slot, 0);

                let sig_ref = self.function_builder.import_signature(self.uniform_func_signature.clone());
                if is_tail && !self.is_specialized {
                    let base_address = self.copy_tail_call_args_and_get_new_base();
                    self.function_builder.ins().return_call_indirect(sig_ref, func_pointer, &[base_address]);
                    None
                } else {
                    let inst = self.function_builder.ins().call_indirect(sig_ref, func_pointer, &[self.tip_address]);
                    self.extract_return_value(inst)
                }
            }
            CTerm::Let { box t, bound_index, box body } => {
                let t_value = self.translate_c_term(t, false);
                self.local_vars[*bound_index] = t_value;
                self.translate_c_term(body, is_tail)
            }
            CTerm::Def { name, may_have_complex_effects } => {
                let func_ref = self.get_local_function(name);
                if is_tail && !self.is_specialized {
                    let base_address = self.copy_tail_call_args_and_get_new_base();
                    self.function_builder.ins().return_call(func_ref, &[base_address]);
                    None
                } else {
                    let inst = self.function_builder.ins().call(func_ref, &[self.tip_address]);
                    self.extract_return_value(inst)
                }
            }
            CTerm::CaseInt { t, result_type, branches, default_branch } => {
                let branch_map: HashMap<_, _> = branches.iter().map(|(i, v)| (i, v)).collect();
                let t_value = self.translate_v_term(t);
                let t_value = self.convert_to_special(t_value, Integer);

                // Create next block
                let next_block = self.function_builder.create_block();
                let result_v_type = match result_type {
                    CType::Default => &Uniform,
                    CType::SpecializedF(vty) => vty,
                };
                let result_value_type = result_v_type.get_type();
                self.function_builder.append_block_param(next_block, result_value_type);

                // Create branch blocks
                let mut branch_blocks = HashMap::new();
                for (value, _) in branches.iter() {
                    let branch_block = self.function_builder.create_block();
                    branch_blocks.insert(*value, branch_block);
                }

                let default_block = self.function_builder.create_block();

                // Create table jump
                let mut switch = Switch::new();
                for (value, branch_block) in branch_blocks.iter() {
                    switch.set_entry(*value as u128, *branch_block);
                }
                switch.emit(&mut self.function_builder, t_value, default_block);

                // Fill branch blocks
                for (value, branch_block) in branch_blocks.into_iter() {
                    let branch = branch_map.get(&value).unwrap();
                    self.create_branch_block(branch_block, is_tail, next_block, result_v_type, Some(branch));
                }

                self.create_branch_block(default_block, is_tail, next_block, result_v_type, match default_branch {
                    None => None,
                    Some(box branch) => Some(branch),
                });

                // Switch to next block for future code generation
                self.function_builder.seal_all_blocks();
                self.function_builder.switch_to_block(next_block);
                Some((self.function_builder.block_params(next_block)[0], *result_v_type))
            }
            CTerm::MemGet { base, offset } => {
                let base_value = self.translate_v_term(base);
                let offset_value = self.translate_v_term(offset);
                let base_value = self.convert_to_special(base_value, StructPtr);
                let offset_value = self.convert_to_special(offset_value, Integer);
                let offset_value = self.function_builder.ins().ishl_imm(offset_value, 3);
                let load_address = self.function_builder.ins().iadd(base_value, offset_value);
                let value = self.function_builder.ins().load(I64, MemFlags::new(), load_address, 0);
                Some((value, Uniform))
            }
            CTerm::MemSet { base, offset, value } => {
                let base_value = self.translate_v_term(base);
                let offset_value = self.translate_v_term(offset);
                let value_value = self.translate_v_term(value);
                let base_value = self.convert_to_special(base_value, StructPtr);
                let offset_value = self.convert_to_special(offset_value, Integer);
                let offset_value = self.function_builder.ins().ishl_imm(offset_value, 3);
                let value_value = self.convert_to_uniform(value_value);
                let store_address = self.function_builder.ins().iadd(base_value, offset_value);
                self.function_builder.ins().store(MemFlags::new(), value_value, store_address, 0);
                // Return the base address so that the caller can continue to use it.
                Some((base_value, Specialized(StructPtr)))
            }
            CTerm::PrimitiveCall { name, args } => {
                let args = args.iter().map(|arg| { self.translate_v_term(arg) }).collect::<Vec<_>>();
                let primitive_function = *PRIMITIVE_FUNCTIONS.get(name).unwrap();
                let arg_values: Vec<Value> = primitive_function.arg_types.iter().zip(args).map(|(ty, arg)| self.adapt_type(arg, ty)).collect();
                let return_value = (primitive_function.code_gen)(&mut self.function_builder, &arg_values);
                Some((return_value, primitive_function.return_type))
            }
            CTerm::SpecializedFunctionCall { name, args, may_have_complex_effects } => {
                let (arg_types, CType::SpecializedF(return_type)) = self.local_function_arg_types.get(name).unwrap() else { unreachable!("{} is not specialized", name) };
                let tip_address = self.tip_address;
                let all_args = iter::once(tip_address)
                    .chain(args.iter()
                        .zip(arg_types)
                        .map(|(arg, v_type)| {
                            let arg = self.translate_v_term(arg);
                            self.adapt_type(arg, v_type)
                        }
                        ))
                    .collect::<Vec<_>>();
                let name = format!("{}__specialized", name);
                let func_ref = self.get_local_function(&name);
                if is_tail && self.is_specialized {
                    self.function_builder.ins().return_call(func_ref, &all_args);
                    None
                } else {
                    let inst = self.function_builder.ins().call(func_ref, &all_args);
                    Some((self.function_builder.inst_results(inst)[0], *return_type))
                }
            }
            CTerm::OperationCall { .. } => todo!(),
            CTerm::Handler { .. } => todo!(),
            CTerm::ResumeContinuation { .. } => todo!(),
            CTerm::DisposeContinuation { .. } => todo!(),
            CTerm::ReplicateContinuation { .. } => todo!(),
        }
    }

    fn create_branch_block(&mut self, branch_block: Block, is_tail: bool, next_block: Block, result_v_type: &VType, branch: Option<&CTerm>) {
        self.function_builder.switch_to_block(branch_block);
        let typed_return_value = match branch {
            None => {
                self.function_builder.ins().trap(TrapCode::UnreachableCodeReached);
                None
            }
            Some(branch) => self.translate_c_term(branch, is_tail),
        };
        match typed_return_value {
            None => {
                // Nothing to do since tail call is already a terminating instruction.
            }
            Some(..) => {
                let value = self.adapt_type(typed_return_value, result_v_type);
                self.function_builder.ins().jump(next_block, &[value]);
            }
        }
    }

    fn extract_return_value(&mut self, inst: Inst) -> TypedReturnValue {
        let return_address = self.function_builder.inst_results(inst)[0];
        let return_value = self.function_builder.ins().load(I64, MemFlags::new(), return_address, 0);
        self.tip_address = self.function_builder.ins().iadd_imm(self.tip_address, 8);
        Some((return_value, Uniform))
    }

    fn copy_tail_call_args_and_get_new_base(&mut self) -> Value {
        if self.num_args == 0 {
            return self.tip_address;
        }
        let new_base_value = self.function_builder.ins().iadd_imm(self.tip_address, (self.num_args * 8) as i64);
        let num_bytes_to_copy = self.function_builder.ins().isub(self.base_address, self.tip_address);
        self.function_builder.call_memmove(self.module.target_config(), new_base_value, self.tip_address, num_bytes_to_copy);
        new_base_value
    }

    fn translate_v_term(&mut self, v_term: &VTerm) -> TypedReturnValue {
        match v_term {
            VTerm::Var { index } => self.local_vars[*index],
            VTerm::Thunk { box t } => {
                let empty_args = &vec![];
                let (name, args) = match t {
                    CTerm::Redex { function: box CTerm::Def { name, may_have_complex_effects }, args } => (name, args),
                    CTerm::Def { name, may_have_complex_effects } => (name, empty_args),
                    _ => unreachable!("thunk lifting should have guaranteed this")
                };
                let func_ref = self.get_local_function(name);
                let func_pointer = self.function_builder.ins().func_addr(I64, func_ref);
                let func_pointer = Some((func_pointer, Specialized(PrimitivePtr)));
                if args.is_empty() {
                    return func_pointer;
                }
                // Plus 1 to indicate this pointer points to a bare function (rather than a closure).
                let arg_size = self.function_builder.ins().iconst(I64, args.len() as i64);
                let mut thunk_components = vec![self.convert_to_uniform(func_pointer), arg_size];
                for arg in args {
                    let value_and_type = self.translate_v_term(arg);
                    let uniform_value = self.convert_to_uniform(value_and_type);
                    thunk_components.push(uniform_value);
                }
                Some((self.create_struct(thunk_components), Specialized(StructPtr)))
            }
            VTerm::Int { value } => Some((self.function_builder.ins().iconst(I64, *value), Specialized(Integer))),
            VTerm::Str { value } => {
                // Insert into the global data section if not already there.
                let data_id = self.static_strings.entry(value.clone()).or_insert_with(|| {
                    self.data_description.define(value.clone().into_bytes().into_boxed_slice());
                    let data_id = self.module.declare_data(value, Linkage::Local, false, false).unwrap();
                    self.module.define_data(data_id, self.data_description).unwrap();
                    self.data_description.clear();
                    data_id
                });
                let global_value = self.module.declare_data_in_func(*data_id, self.function_builder.func);
                Some((self.function_builder.ins().symbol_value(I64, global_value), Specialized(PrimitivePtr)))
            }
            VTerm::Struct { values } => {
                let translated = values.iter().map(|v| {
                    let v = self.translate_v_term(v);
                    self.convert_to_uniform(v)
                }).collect::<Vec<_>>();
                Some((self.create_struct(translated), Specialized(StructPtr)))
            }
        }
    }

    fn push_args(&mut self, args: Vec<Value>) {
        for arg in args.into_iter().rev() {
            self.tip_address = self.function_builder.ins().iadd_imm(self.tip_address, -8);
            self.function_builder.ins().store(MemFlags::new().with_aligned(), arg, self.tip_address, 0);
        }
    }

    fn get_local_function(&mut self, name: &String) -> FuncRef {
        let func_id = self.local_functions.get(name).unwrap();
        self.module.declare_func_in_func(*func_id, self.function_builder.func)
    }

    fn create_struct(&mut self, values: Vec<Value>) -> Value {
        let struct_size = values.len() * 8;
        let struct_size_value = self.function_builder.ins().iconst(I64, struct_size as i64);
        let runtime_alloc_call = self.call_builtin_func(BuiltinFunction::Alloc, &[struct_size_value]);
        let struct_address = self.function_builder.inst_results(runtime_alloc_call)[0];
        for (offset, value) in values.into_iter().enumerate() {
            self.function_builder.ins().store(
                MemFlags::new().with_aligned(),
                value,
                struct_address,
                (offset * 8) as i32);
        }
        struct_address
    }

    fn call_builtin_func(&mut self, builtin_function: BuiltinFunction, args: &[Value]) -> Inst {
        let func_ref = self.module.declare_func_in_func(self.builtin_functions[builtin_function], self.function_builder.func);
        self.function_builder.ins().call(func_ref, args)
    }

    pub(crate) fn convert_to_uniform(&mut self, value_and_type: TypedReturnValue) -> Value {
        let (value, value_type) = Self::extract_value_and_type(value_and_type);
        match value_type {
            Uniform => value,
            Specialized(s) => match s {
                Integer => self.function_builder.ins().ishl_imm(value, 1),
                StructPtr => self.function_builder.ins().iadd_imm(value, 1),
                PrimitivePtr => self.function_builder.ins().iadd_imm(value, 0b11),
                SpecializedType::Primitive(p) => match p {
                    PType::I64 | PType::F64 => {
                        let alloc_size = self.function_builder.ins().iconst(I64, 8);
                        let inst = self.call_builtin_func(BuiltinFunction::Alloc, &[alloc_size]);
                        let ptr = self.function_builder.inst_results(inst)[0];
                        self.function_builder.ins().store(MemFlags::new(), value, ptr, 0);
                        // Add 0b11 to the end to signify this is a primitive pointer
                        self.function_builder.ins().iadd_imm(ptr, 0b11)
                    }
                    PType::I32 => {
                        let extended = self.function_builder.ins().sextend(I64, value);
                        self.function_builder.ins().ishl_imm(extended, 32)
                    }
                    PType::F32 => {
                        let casted = self.function_builder.ins().bitcast(I32, MemFlags::new(), value);
                        let extended = self.function_builder.ins().sextend(I64, casted);
                        self.function_builder.ins().ishl_imm(extended, 32)
                    }
                }
            }
        }
    }

    fn convert_to_special(&mut self, value_and_type: TypedReturnValue, specialized_type: SpecializedType) -> Value {
        let (value, value_type) = Self::extract_value_and_type(value_and_type);
        match value_type {
            Uniform => match specialized_type {
                Integer => self.function_builder.ins().sshr_imm(value, 1),
                StructPtr => self.function_builder.ins().iadd_imm(value, -1),
                PrimitivePtr => self.function_builder.ins().iadd_imm(value, -0b11),
                SpecializedType::Primitive(p) => match p {
                    PType::I64 | PType::F64 => {
                        let ptr = self.function_builder.ins().iadd_imm(value, -0b11);
                        self.function_builder.ins().load(p.get_type(), MemFlags::new(), ptr, 0)
                    }
                    PType::I32 => {
                        let shifted = self.function_builder.ins().sshr_imm(value, 32);
                        self.function_builder.ins().ireduce(I32, shifted)
                    }
                    PType::F32 => {
                        let shifted = self.function_builder.ins().sshr_imm(value, 32);
                        let truncated = self.function_builder.ins().ireduce(I32, shifted);
                        self.function_builder.ins().bitcast(F32, MemFlags::new(), truncated)
                    }
                }
            }
            Specialized(s) => if s == specialized_type {
                value
            } else {
                unreachable!("type conversion between two specialized types is not supported and this must be a type error in the input program")
            }
        }
    }

    pub(crate) fn adapt_type(&mut self, value_and_type: TypedReturnValue, target_type: &VType) -> Value {
        let (value, value_type) = Self::extract_value_and_type(value_and_type);
        if value_type == *target_type {
            return value;
        }
        match (value_type, target_type) {
            (Uniform, Specialized(s)) => self.convert_to_special(value_and_type, s.clone()),
            (Specialized(_), Uniform) => self.convert_to_uniform(value_and_type),
            (Specialized(a), Specialized(b)) => unreachable!("type conversion from {:?} to {:?} is not supported and this must be a type error in the input program", a, b),
            _ => unreachable!()
        }
    }

    fn extract_value_and_type(value_and_type: TypedReturnValue) -> (Value, VType) {
        value_and_type.expect("non-local return value cannot be converted and this must be a bug in the compilation logic or input is not well-typed")
    }
}
