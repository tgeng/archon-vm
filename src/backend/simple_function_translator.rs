use std::collections::HashMap;
use std::iter;
use cranelift::codegen::ir::{Endianness, FuncRef, Inst, StackSlot};
use cranelift::frontend::Switch;
use cranelift::prelude::*;
use cranelift::prelude::types::{F32, I32, I64};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use crate::ast::term::{CTerm, VTerm, VType, SpecializedType, PType, CType};
use enum_map::{EnumMap};
use VType::{Specialized, Uniform};
use SpecializedType::{Integer, PrimitivePtr, StructPtr};
use crate::backend::common::{BuiltinFunction, FunctionFlavor, HasType, TypedReturnValue, TypedValue};
use crate::ast::primitive_functions::PRIMITIVE_FUNCTIONS;
use crate::ast::signature::FunctionDefinition;
use crate::backend::compiler::Compiler;

pub struct SimpleFunctionTranslator<'a, M: Module> {
    pub module: &'a mut M,
    pub function_builder: FunctionBuilder<'a>,
    pub data_description: DataDescription,
    pub builtin_functions: EnumMap<BuiltinFunction, FuncId>,
    pub static_strings: &'a mut HashMap<String, DataId>,
    pub local_functions: &'a HashMap<String, FuncId>,
    pub local_vars: Vec<TypedReturnValue>,
    pub base_address: Value,
    pub tip_address: Value,
    pub num_args: usize,
    pub uniform_func_signature: Signature,
    pub uniform_cps_func_signature: Signature,
    pub uniform_cps_impl_func_signature: Signature,
    pub tip_address_slot: StackSlot,
    pub local_function_arg_types: &'a HashMap<String, (Vec<VType>, CType)>,
    pub is_specialized: bool,
    /// The pointer to the start of the local variable storage allocated inside the current
    /// continuation object. Note that function arguments are not stored in the continuation object,
    /// so the local variables are offset by the number of function arguments.
    pub local_var_ptr: Value,
}

impl<'a, M: Module> SimpleFunctionTranslator<'a, M> {
    pub fn compile_simple_function(
        name: &str,
        compiler: &mut Compiler<M>,
        function_definition: &FunctionDefinition,
        local_function_arg_types: &HashMap<String, (Vec<VType>, CType)>,
        clir: &mut Option<&mut Vec<(String, String)>>,
    ) {
        // All functions have the same signature `i64 -> i64`, where the single argument is the
        // base address of the parameter stack and the single return value is the address of the
        // return address. Actual parameters can be obtained by offsetting this base address.
        //
        // Callee should compute the return address by adding the total size of the parameters to
        // the callee base address minus one word. See diagram below for the following call
        //
        // fn caller(x, y) {
        //   ...
        //   let i = callee(a, b, c);
        //   ...
        // }
        //
        //  |---------|
        //  |    y    |
        //  |---------| <- caller return address
        //  |    x    |
        //  |---------| <- caller base address
        //  |  c / i  |
        //  |---------| <- callee return address: the address where callee put the return value
        //  |    b    |
        //  |---------|
        //  |    a    |
        //  |---------| <- callee base address: the address from which callee finds arguments
        //
        // Caller tracks a current tip address, which initially points to the caller base address.
        // When evaluating a redex, this pointer is bumped and a new parameter is stored at this
        // address. After all parameters are pushed, this pointer becomes the callee base address.
        // Whenever a function call completes, this tip is set to the callee return address plus
        // size of one word, so that the next call can happen normally.

        // Here we transform the function body to non-specialized version, hence the argument types
        // are ignored.

        let mut translator = SimpleFunctionTranslator::new(
            compiler,
            compiler.uniform_func_signature.clone(),
            function_definition,
            local_function_arg_types,
            false,
            |function_builder, entry_block| function_builder.block_params(entry_block)[0],
            |translator, _entry_block, i, _v_type| {
                // v is the variable index and i is the offset in the parameter list. The parameter
                // stack grows from higher address to lower address, so parameter list grows in the
                // reverse order and hence the offset is the index of the parameter in the parameter
                // list.
                let value = translator.function_builder.ins().load(I64, MemFlags::new(), translator.base_address, (i * 8) as i32);
                Some((value, Uniform))
            },
        );
        // The return value will be returned so its type does not matter. Treating it as an integer
        // is sufficient.
        let return_value_or_param = translator.translate_c_term(&function_definition.body, true);
        match return_value_or_param {
            Some(_) => {
                let return_value_address = translator.store_return_value_on_argument_stack(
                    return_value_or_param,
                    function_definition.args.len(),
                );
                translator.function_builder.ins().return_(&[return_value_address]);
            }
            None => {
                // Nothing to do since tail call is already a terminating instruction.
            }
        }
        translator.function_builder.seal_all_blocks();
        translator.function_builder.finalize();

        let simple_name = FunctionFlavor::Simple.decorate_name(name);
        let func_id = compiler.local_functions.get(&simple_name).unwrap();
        SimpleFunctionTranslator::define_function(&mut compiler.module, &mut compiler.ctx, &simple_name, *func_id, clir);
    }

    pub fn store_return_value_on_argument_stack(&mut self, return_value_or_param: TypedReturnValue, num_args: usize) -> Value {
        let value = self.convert_to_uniform(return_value_or_param);
        let return_address_offset = (num_args as i64 - 1) * 8;
        let return_address = self.function_builder.ins().iadd_imm(self.base_address, return_address_offset);
        self.function_builder.ins().store(MemFlags::new(), value, return_address, 0);
        return_address
    }
    pub fn compile_specialized_function(
        name: &str,
        compiler: &mut Compiler<M>,
        sig: Signature,
        function_definition: &FunctionDefinition,
        local_function_arg_types: &HashMap<String, (Vec<VType>, CType)>,
        clir: &mut Option<&mut Vec<(String, String)>>,
    ) {
        let mut translator = SimpleFunctionTranslator::new(
            compiler,
            sig,
            function_definition,
            local_function_arg_types,
            true,
            |function_builder, entry_block| function_builder.block_params(entry_block)[0],
            |translator, entry_block, i, v_type| {
                // v is the variable index and i is the offset in the parameter list. The parameter
                // stack grows from higher address to lower address, so parameter list grows in the
                // reverse order and hence the offset is the index of the parameter in the parameter
                // list.
                // In addition, i + 1 is the parameter index in the entry block
                let value = translator.function_builder.block_params(entry_block)[i + 1];
                Some((value, *v_type))
            },
        );
        // The return value will be returned so its type does not matter. Treating it as an integer
        // is sufficient.
        let return_value_or_param = translator.translate_c_term(&function_definition.body, true);
        match return_value_or_param {
            Some(_) => {
                let CType::SpecializedF(v_type) = function_definition.c_type else { unreachable!() };
                let value = translator.adapt_type(return_value_or_param, &v_type);
                translator.function_builder.ins().return_(&[value]);
            }
            None => {
                // Nothing to do since tail call is already a terminating instruction.
            }
        }
        translator.function_builder.seal_all_blocks();
        translator.function_builder.finalize();

        let specialized_name = FunctionFlavor::Specialized.decorate_name(name);
        let func_id = compiler.local_functions.get(&specialized_name).unwrap();
        SimpleFunctionTranslator::define_function(&mut compiler.module, &mut compiler.ctx, &specialized_name, *func_id, clir);
    }
    pub fn new<F>(
        compiler: &'a mut Compiler<M>,
        sig: Signature,
        function_definition: &'a FunctionDefinition,
        local_function_arg_types: &'a HashMap<String, (Vec<VType>, CType)>,
        is_specialized: bool,
        base_address_getter: F,
        parameter_initializer: fn(&mut SimpleFunctionTranslator<M>, Block, usize, &VType) -> TypedReturnValue,
    ) -> SimpleFunctionTranslator<'a, M> where F: FnOnce(&mut FunctionBuilder, Block) -> Value {
        // Parameters of specialized functions are just passed normally.

        compiler.ctx.clear();
        compiler.ctx.func.signature = sig;

        let mut function_builder = FunctionBuilder::new(&mut compiler.ctx.func, &mut compiler.builder_context);
        let entry_block = function_builder.create_block();

        // Allocate slot for storing the tip address so that a pointer to the tip address can be
        // passed to built-in force call helper function in order to have the tip address updated.
        let tip_address_slot = function_builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
        function_builder.append_block_params_for_function_params(entry_block);
        function_builder.switch_to_block(entry_block);
        function_builder.seal_block(entry_block);

        let base_address = base_address_getter(&mut function_builder, entry_block);
        let mut translator = SimpleFunctionTranslator {
            module: &mut compiler.module,
            function_builder,
            data_description: DataDescription::new(),
            builtin_functions: compiler.builtin_functions,
            static_strings: &mut compiler.static_strings,
            local_functions: &compiler.local_functions,
            local_vars: vec![None; function_definition.var_bound],
            base_address,
            tip_address: base_address,
            num_args: function_definition.args.len(),
            uniform_func_signature: compiler.uniform_func_signature.clone(),
            uniform_cps_func_signature: compiler.uniform_cps_func_signature.clone(),
            uniform_cps_impl_func_signature: compiler.uniform_cps_impl_func_signature.clone(),
            tip_address_slot,
            local_function_arg_types,
            is_specialized,
            // This is a placeholder value. This value is overwritten by [CpsImplFunctionTranslator]
            local_var_ptr: Value::with_number(0).unwrap(),
        };
        // Here we transform the function body to non-specialized version, hence the argument types
        // are ignored.
        for (i, (v, v_type)) in function_definition.args.iter().enumerate() {
            translator.local_vars[*v] = parameter_initializer(&mut translator, entry_block, i, v_type);
        }
        translator
    }

    pub fn translate_c_term(&mut self, c_term: &CTerm, is_tail: bool) -> TypedReturnValue {
        match c_term {
            CTerm::Redex { box function, args } => {
                if let CTerm::Def { name, .. } = function {
                    // Handle specialized function call
                    let (arg_types, return_type) = self.local_function_arg_types.get(name).unwrap();
                    if let CType::SpecializedF(return_type) = return_type && arg_types.len() == args.len() {
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
                        let func_ref = self.get_local_function(name, FunctionFlavor::Specialized);
                        if is_tail && self.is_specialized {
                            self.function_builder.ins().return_call(func_ref, &all_args);
                            return None;
                        } else {
                            let inst = self.function_builder.ins().call(func_ref, &all_args);
                            return Some((self.function_builder.inst_results(inst)[0], *return_type));
                        }
                    }
                }
                self.push_arg_v_terms(args);
                self.translate_c_term(function, is_tail)
            }
            CTerm::Return { value } => self.translate_v_term(value),
            CTerm::Force { thunk, .. } => {
                let inst = self.call_builtin_func(BuiltinFunction::GetTrivialContinuation, &[]);
                let trivial_continuation = self.function_builder.inst_results(inst)[0];
                self.invoke_thunk(is_tail, thunk, trivial_continuation)
            }
            CTerm::Let { box t, bound_index, box body } => {
                let t_value = self.translate_c_term(t, false);
                self.local_vars[*bound_index] = t_value;
                self.translate_c_term(body, is_tail)
            }
            CTerm::Def { name, .. } => {
                let func_ref = self.get_local_function(name, FunctionFlavor::Simple);
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
                let joining_block = self.function_builder.create_block();
                let result_v_type = match result_type {
                    CType::Default => &Uniform,
                    CType::SpecializedF(vty) => vty,
                };
                let result_value_type = result_v_type.get_type();
                // return value
                self.function_builder.append_block_param(joining_block, result_value_type);
                // tip address
                self.function_builder.append_block_param(joining_block, I64);

                // Create branch blocks
                let mut branch_blocks = HashMap::new();
                for (value, _) in branches.iter() {
                    let branch_block = self.function_builder.create_block();
                    branch_blocks.insert(*value, branch_block);
                }

                let default_block = self.function_builder.create_block();

                // Create table jump
                let mut branch_blocks: Vec<_> = branch_blocks.into_iter().collect();
                branch_blocks.sort_by_key(|(k, _)| *k);
                let mut switch = Switch::new();
                for (value, branch_block) in branch_blocks.iter() {
                    switch.set_entry(*value as u128, *branch_block);
                }
                switch.emit(&mut self.function_builder, t_value, default_block);

                // Fill branch blocks
                let start_tip_address = self.tip_address;
                for (value, branch_block) in branch_blocks.into_iter() {
                    self.tip_address = start_tip_address;
                    let branch = branch_map.get(&value).unwrap();
                    self.create_branch_block(branch_block, is_tail, joining_block, result_v_type, Some(branch));
                }

                self.tip_address = start_tip_address;
                self.create_branch_block(default_block, is_tail, joining_block, result_v_type, match default_branch {
                    None => None,
                    Some(box branch) => Some(branch),
                });

                // Switch to joining block for future code generation
                self.function_builder.seal_all_blocks();
                self.function_builder.switch_to_block(joining_block);
                self.tip_address = self.function_builder.block_params(joining_block)[1];
                Some((self.function_builder.block_params(joining_block)[0], *result_v_type))
            }
            CTerm::Lambda { .. } => unreachable!("lambda should have been lifted away"),
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
            CTerm::OperationCall { eff, args, may_be_complex } => {
                assert!(!may_be_complex);
                let eff_value = self.translate_v_term(eff);
                let eff_value = self.convert_to_uniform(eff_value);
                self.push_arg_v_terms(args);
                let inst = self.call_builtin_func(BuiltinFunction::HandleSimpleOperation, &[eff_value, self.tip_address]);
                let result = self.function_builder.inst_results(inst)[0];
                Some((result, Uniform))
            }
            CTerm::Handler { .. } => {
                let inst = self.call_builtin_func(BuiltinFunction::GetTrivialContinuation, &[]);
                let next_continuation = self.function_builder.inst_results(inst)[0];

                self.translate_handler(c_term, is_tail, next_continuation)
            }
            CTerm::LongJump { .. } => todo!(),
        }
    }

    pub fn translate_handler(&mut self, handler_c_term: &CTerm, is_tail: bool, next_continuation: Value) -> TypedReturnValue {
        let CTerm::Handler {
            parameter,
            parameter_disposer,
            parameter_replicator,
            transform,
            complex_handlers,
            simple_handlers,
            input
        } = handler_c_term else { unreachable!() };
        let parameter_typed_value = self.translate_v_term(parameter);
        let parameter_value = self.convert_to_uniform(parameter_typed_value);
        let parameter_disposer_typed_value = self.translate_v_term(parameter_disposer);
        let parameter_disposer_value = self.convert_to_uniform(parameter_disposer_typed_value);
        let parameter_replicator_typed_value = self.translate_v_term(parameter_replicator);
        let parameter_replicator_value = self.convert_to_uniform(parameter_replicator_typed_value);
        let transform_typed_value = self.translate_v_term(transform);
        let transform_value = self.convert_to_uniform(transform_typed_value);
        let transform_loader_cps_impl_func_ref = self.module.declare_func_in_func(
            self.builtin_functions[BuiltinFunction::TransformLoaderCpsImpl],
            self.function_builder.func,
        );
        let transform_loader_cps_impl_func_ptr = self.function_builder.ins().func_addr(I64, transform_loader_cps_impl_func_ref);
        let tip_address_ptr = self.store_tip_address_to_stack();
        let inst = self.call_builtin_func(BuiltinFunction::RegisterHandlerAndGetTransformContinuation, &[
            tip_address_ptr,
            next_continuation,
            parameter_value,
            parameter_disposer_value,
            parameter_replicator_value,
            transform_value,
            transform_loader_cps_impl_func_ptr,
        ]);
        let handler = self.function_builder.inst_results(inst)[0];
        self.load_tip_address_from_stack();

        // set up handlers
        let add_simple_handler_func_ref = self.module.declare_func_in_func(
            self.builtin_functions[BuiltinFunction::AddSimpleHandler],
            self.function_builder.func);
        self.add_handlers(handler, add_simple_handler_func_ref, simple_handlers);
        let add_complex_handler_func_ref = self.module.declare_func_in_func(
            self.builtin_functions[BuiltinFunction::AddComplexHandler],
            self.function_builder.func);
        self.add_handlers(handler, add_complex_handler_func_ref, complex_handlers);

        // The transform loader continuation is the first value inside the handler struct.
        let transform_loader_continuation = self.function_builder.ins().load(I64, MemFlags::new(), handler, 0);
        self.invoke_thunk(is_tail, input, transform_loader_continuation)
    }

    fn add_handlers(&mut self, handler: Value, add_handler_func_ref: FuncRef, handler_impls: &Vec<(VTerm, VTerm)>) {
        for (eff, handler_impl) in handler_impls {
            let eff_value = self.translate_v_term(eff);
            let eff_value = self.convert_to_uniform(eff_value);
            let handler_impl_value = self.translate_v_term(handler_impl);
            let handler_impl_value = self.convert_to_uniform(handler_impl_value);
            self.function_builder.ins().call(add_handler_func_ref, &[handler, eff_value, handler_impl_value]);
        }
    }

    fn invoke_thunk(&mut self, is_tail: bool, thunk: &VTerm, next_continuation: Value) -> Option<TypedValue> {
        let func_pointer = self.process_thunk(thunk, next_continuation);

        let sig_ref = self.function_builder.import_signature(self.uniform_cps_func_signature.clone());
        if is_tail && !self.is_specialized {
            let base_address = self.copy_tail_call_args_and_get_new_base();
            self.function_builder.ins().return_call_indirect(sig_ref, func_pointer, &[
                base_address,
                next_continuation,
            ]);
            None
        } else {
            let inst = self.function_builder.ins().call_indirect(sig_ref, func_pointer, &[
                self.tip_address,
                next_continuation,
            ]);
            self.extract_return_value(inst)
        }
    }

    pub fn process_thunk(&mut self, thunk: &VTerm, next_continuation: Value) -> Value {
        let thunk_value = self.translate_v_term(thunk);
        // We must change the thunk value to uniform representation because the built-in
        // force thunk function expects a uniform representation in order to tell a thunk from a raw
        // function pointer.
        let thunk_value = self.convert_to_uniform(thunk_value);

        let tip_address_ptr = self.store_tip_address_to_stack();
        let inst = self.call_builtin_func(BuiltinFunction::ForceThunk, &[thunk_value, tip_address_ptr]);
        let func_pointer = self.function_builder.inst_results(inst)[0];
        let old_tip_address = self.tip_address;
        self.load_tip_address_from_stack();
        let continuation_frame_height_delta_bytes = self.function_builder.ins().isub(old_tip_address, self.tip_address);
        let continuation_frame_height_delta = self.function_builder.ins().ushr_imm(continuation_frame_height_delta_bytes, 3);
        let continuation_frame_height = self.function_builder.ins().load(I64, MemFlags::new(), next_continuation, 8);
        let new_continuation_frame_height = self.function_builder.ins().iadd(continuation_frame_height, continuation_frame_height_delta);
        self.function_builder.ins().store(MemFlags::new(), new_continuation_frame_height, next_continuation, 8);
        func_pointer
    }

    fn load_tip_address_from_stack(&mut self) {
        self.tip_address = self.function_builder.ins().stack_load(I64, self.tip_address_slot, 0);
    }

    fn store_tip_address_to_stack(&mut self) -> Value {
        self.function_builder.ins().stack_store(self.tip_address, self.tip_address_slot, 0);
        let tip_address_ptr = self.function_builder.ins().stack_addr(I64, self.tip_address_slot, 0);
        tip_address_ptr
    }

    pub fn push_arg_v_terms(&mut self, args: &Vec<VTerm>) {
        let arg_values = args.iter().map(|arg| {
            let v = self.translate_v_term(arg);
            self.convert_to_uniform(v)
        }).collect::<Vec<_>>();
        self.push_args(arg_values);
    }

    fn create_branch_block(&mut self, branch_block: Block, is_tail: bool, joining_block: Block, result_v_type: &VType, branch: Option<&CTerm>) {
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
                self.function_builder.ins().jump(joining_block, &[value, self.tip_address]);
            }
        }
    }

    pub fn extract_return_value(&mut self, inst: Inst) -> TypedReturnValue {
        let return_address = self.function_builder.inst_results(inst)[0];
        let return_value = self.function_builder.ins().load(I64, MemFlags::new(), return_address, 0);
        self.tip_address = self.function_builder.ins().iadd_imm(return_address, 8);
        Some((return_value, Uniform))
    }

    pub fn copy_tail_call_args_and_get_new_base(&mut self) -> Value {
        if self.num_args == 0 {
            return self.tip_address;
        }
        let new_base_value = self.function_builder.ins().iadd_imm(self.tip_address, (self.num_args * 8) as i64);
        let num_bytes_to_copy = self.function_builder.ins().isub(self.base_address, self.tip_address);
        self.function_builder.call_memmove(self.module.target_config(), new_base_value, self.tip_address, num_bytes_to_copy);
        new_base_value
    }

    pub fn translate_v_term(&mut self, v_term: &VTerm) -> TypedReturnValue {
        match v_term {
            VTerm::Var { index } => match self.local_vars[*index] {
                None => {
                    if *index < self.num_args {
                        let base_address = self.base_address;
                        let value = self.function_builder.ins().load(I64, MemFlags::new(), base_address, (8 * index) as i32);
                        let typed_return_value = Some((value, Uniform));
                        self.local_vars[*index] = typed_return_value;
                        typed_return_value
                    } else {
                        let local_var_index = *index - self.num_args;
                        let local_var_ptr = self.local_var_ptr;
                        let value = self.function_builder.ins().load(I64, MemFlags::new(), local_var_ptr, (8 * local_var_index) as i32);
                        let typed_return_value = Some((value, Uniform));
                        self.local_vars[*index] = typed_return_value;
                        typed_return_value
                    }
                }
                v => v,
            },
            VTerm::Thunk { box t, .. } => {
                let empty_args = &vec![];
                let (name, args) = match t {
                    CTerm::Redex { function: box CTerm::Def { name, .. }, args } => (name, args),
                    CTerm::Def { name, .. } => (name, empty_args),
                    _ => unreachable!("thunk lifting should have guaranteed this")
                };
                let func_ref = self.get_local_function(name, FunctionFlavor::Cps);
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
                    let len_in_words = value.len().div_ceil(8);
                    let bytes = value.clone().into_bytes();
                    let len_bytes = match self.module.isa().endianness() {
                        Endianness::Little => len_in_words.to_le_bytes(),
                        Endianness::Big => len_in_words.to_be_bytes(),
                    };
                    // prepend bytes with length of the string in machine words.
                    let bytes = len_bytes.iter().copied().chain(bytes).collect::<Vec<_>>();
                    self.data_description.define(bytes.into_boxed_slice());
                    // Align to 8 bytes because comparison logic compares aligned words.
                    self.data_description.align = Some(8);
                    let data_id = self.module.declare_data(value, Linkage::Local, false, false).unwrap();
                    self.module.define_data(data_id, &self.data_description).unwrap();
                    self.data_description.clear();
                    data_id
                });
                let global_value = self.module.declare_data_in_func(*data_id, self.function_builder.func);
                let raw_data_ptr = self.function_builder.ins().symbol_value(I64, global_value);
                // Offset the pointer by 8 bytes to skip the length field.
                let data_ptr = self.function_builder.ins().iadd_imm(raw_data_ptr, 8);
                Some((data_ptr, Specialized(PrimitivePtr)))
            }
            VTerm::Struct { values } => {
                // TODO: use a common empty struct if values is empty
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

    pub fn get_local_function(&mut self, name: &str, flavor: FunctionFlavor) -> FuncRef {
        let desired_func_name = flavor.decorate_name(name);
        let func_id = self.local_functions.get(&desired_func_name).unwrap_or_else(|| panic!("Cannot get function '{}'", &desired_func_name));
        self.module.declare_func_in_func(*func_id, self.function_builder.func)
    }

    fn create_struct(&mut self, values: Vec<Value>) -> Value {
        let struct_size = values.len();
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

    pub(crate) fn call_builtin_func(&mut self, builtin_function: BuiltinFunction, args: &[Value]) -> Inst {
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

    pub(crate) fn convert_to_special(&mut self, value_and_type: TypedReturnValue, specialized_type: SpecializedType) -> Value {
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

    pub fn define_function(module: &mut M, ctx: &mut codegen::Context, name: &str, func_id: FuncId, clir: &mut Option<&mut Vec<(String, String)>>) {
        if let Some(clir) = clir {
            clir.push((name.to_owned(), format!("{}", ctx.func.display())));
        }
        module.define_function(func_id, ctx).unwrap_or_else(|e| {
            panic!("failed to define function {}: {:#?}\nDetails: {}", name, e, ctx.func.display());
        });
    }

    pub fn handle_complex_operation_call(&mut self, eff_value: Value, new_base_address: Value, continuation: Value, num_args: usize, may_be_complex: bool) {
        let num_args_value = self.function_builder.ins().iconst(I64, num_args as i64);
        let captured_continuation_thunk_impl_ref = self.module.declare_func_in_func(self.builtin_functions[BuiltinFunction::ConvertCapturedContinuationThunkImpl], self.function_builder.func);
        let captured_continuation_thunk_impl_ptr = self.function_builder.ins().func_addr(I64, captured_continuation_thunk_impl_ref);
        let may_be_complex_value = self.function_builder.ins().iconst(I64, may_be_complex as i64);
        let inst = self.call_builtin_func(BuiltinFunction::PrepareOperation, &[eff_value, new_base_address, continuation, num_args_value, captured_continuation_thunk_impl_ptr, may_be_complex_value]);
        let result_ptr = self.function_builder.inst_results(inst)[0];
        let handler_impl = self.function_builder.ins().load(I64, MemFlags::new(), result_ptr, 0);
        let handler_base_address = self.function_builder.ins().load(I64, MemFlags::new(), result_ptr, 8);
        let next_continuation = self.function_builder.ins().load(I64, MemFlags::new(), result_ptr, 16);

        let signature = self.uniform_cps_func_signature.clone();
        let sig_ref = self.function_builder.import_signature(signature);

        self.function_builder.ins().return_call_indirect(sig_ref, handler_impl, &[handler_base_address, next_continuation]);
    }
}
