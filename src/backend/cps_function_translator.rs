use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use cranelift::prelude::{Block, InstBuilder, MemFlags, TrapCode, Value};
use cranelift::prelude::types::I64;
use cranelift_module::{FuncId, Linkage, Module};
use cranelift::frontend::Switch;
use crate::ast::signature::FunctionDefinition;
use crate::ast::term::{CTerm, CType, VType};
use crate::ast::term::SpecializedType::Integer;
use crate::ast::term::VType::Uniform;
use crate::backend::common::{BuiltinFunction, FunctionFlavor, HasType, TypedReturnValue};
use crate::backend::compiler::Compiler;
use crate::backend::function_analyzer::FunctionAnalyzer;
use crate::backend::simple_function_translator::SimpleFunctionTranslator;

pub struct CpsFunctionTranslator {}

impl CpsFunctionTranslator {
    pub(crate) fn compile_cps_function<M: Module>(
        name: &str,
        compiler: &mut Compiler<M>,
        function_definition: &FunctionDefinition,
        local_function_arg_types: &HashMap<String, (Vec<VType>, CType)>,
        clir: &mut Option<&mut Vec<(String, String)>>,
    ) {
        let mut function_analyzer = FunctionAnalyzer::new();
        function_analyzer.analyze(&function_definition.body, true);
        let num_blocks = function_analyzer.count;
        let case_blocks = function_analyzer.case_blocks;
        if function_analyzer.has_non_tail_complex_effects {
            let cps_impl_func_id = ComplexCpsFunctionTranslator::compile_cps_impl_function(name, compiler, function_definition, local_function_arg_types, num_blocks, case_blocks, clir);
            ComplexCpsFunctionTranslator::compile_cps_function(name, compiler, function_definition, local_function_arg_types, cps_impl_func_id, clir);
        } else {
            SimpleCpsFunctionTranslator::compile_cps_function(name, compiler, function_definition, local_function_arg_types, clir);
        }
    }
}

/// The translated native function takes the following arguments:
/// - the base address on the argument stack
/// - the next continuation that takes the final result of the current function
/// The translated function is what callers of the CPS function will call. The implementation
/// does one of two things
/// - if this function only tail calls another effectful function (aka, there is only one state
///   of the continuation object), then it just translates the function via
///   [SimpleFunctionTranslator] and then calls the last effectful function as a tail call.
/// - if this function has calls multiple effectful functions, then it creates a continuation
///   object and calls the CPS implementation function.
struct SimpleCpsFunctionTranslator<'a, M: Module> {
    function_translator: SimpleFunctionTranslator<'a, M>,
    next_continuation: Value,
}

impl<'a, M: Module> Deref for SimpleCpsFunctionTranslator<'a, M> {
    type Target = SimpleFunctionTranslator<'a, M>;

    fn deref(&self) -> &Self::Target {
        &self.function_translator
    }
}

impl<'a, M: Module> DerefMut for SimpleCpsFunctionTranslator<'a, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.function_translator
    }
}

impl<'a, M: Module> SimpleCpsFunctionTranslator<'a, M> {
    fn compile_cps_function(
        name: &str,
        compiler: &mut Compiler<M>,
        function_definition: &FunctionDefinition,
        local_function_arg_types: &HashMap<String, (Vec<VType>, CType)>,
        clir: &mut Option<&mut Vec<(String, String)>>,
    ) {
        let mut translator = SimpleCpsFunctionTranslator::new(compiler, function_definition, local_function_arg_types);
        let typed_return_value = translator.translate_c_term_cps(&function_definition.body, true);
        match typed_return_value {
            None => {}
            Some(_) => {
                let return_value_address = translator.store_return_value_on_argument_stack(
                    typed_return_value,
                    function_definition.args.len(),
                );
                let continuation = translator.next_continuation;
                invoke_next_continuation_in_the_end(&mut translator, return_value_address, continuation);
            }
        }
        translator.function_translator.function_builder.finalize();

        let cps_name = FunctionFlavor::Cps.decorate_name(name);
        let func_id = compiler.module.declare_function(&cps_name, Linkage::Local, &compiler.uniform_cps_func_signature).unwrap();
        SimpleFunctionTranslator::define_function(&mut compiler.module, &mut compiler.ctx, &cps_name, func_id, clir);
    }

    fn new(
        compiler: &'a mut Compiler<M>,
        function_definition: &'a FunctionDefinition,
        local_function_arg_types: &'a HashMap<String, (Vec<VType>, CType)>,
    ) -> Self {
        let signature = compiler.uniform_cps_func_signature.clone();

        let mut continuation: Value = Value::with_number(0).unwrap();
        let function_translator = SimpleFunctionTranslator::new(
            compiler,
            signature,
            function_definition,
            local_function_arg_types,
            false,
            |function_builder, entry_block| {
                continuation = function_builder.block_params(entry_block)[1];
                function_builder.block_params(entry_block)[0]
            },
            |translator, _entry_block, i, _v_type| {
                // v is the variable index and i is the offset in the parameter list. The parameter
                // stack grows from higher address to lower address, so parameter list grows in the
                // reverse order and hence the offset is the index of the parameter in the parameter
                // list.
                let value = translator.function_builder.ins().load(I64, MemFlags::new(), translator.base_address, (i * 8) as i32);
                Some((value, Uniform))
            },
        );

        Self {
            function_translator,
            next_continuation: continuation,
        }
    }


    fn translate_c_term_cps(&mut self, c_term: &CTerm, is_tail: bool) -> TypedReturnValue {
        match c_term {
            CTerm::Force { thunk, may_have_complex_effects: true } if is_tail => {
                let continuation = self.next_continuation;
                let func_pointer = self.process_thunk(thunk, continuation);

                let signature = self.uniform_cps_func_signature.clone();
                let sig_ref = self.function_builder.import_signature(signature);
                let new_base_address = compute_cps_tail_call_base_address(self, continuation);
                self.function_builder.ins().return_call_indirect(sig_ref, func_pointer, &[
                    new_base_address, continuation,
                ]);
                None
            }
            CTerm::Def { name, may_have_complex_effects: true } if is_tail => {
                let func_ref = self.get_local_function(name, FunctionFlavor::Cps);
                let continuation = self.next_continuation;
                let new_base_address = compute_cps_tail_call_base_address(self, continuation);
                self.function_builder.ins().return_call(func_ref, &[new_base_address, continuation]);
                None
            }
            CTerm::OperationCall { eff, args, complex: true } if is_tail => {
                let eff_value = self.translate_v_term(eff);
                let eff_value = self.convert_to_uniform(eff_value);
                self.push_arg_v_terms(args);
                let next_continuation = self.next_continuation;
                let new_base_address = compute_cps_tail_call_base_address(self, next_continuation);
                self.handle_complex_operation_call(eff_value, new_base_address, next_continuation, args.len());
                None
            }
            _ => self.translate_c_term(c_term, is_tail),
        }
    }
}

/// The translated native function to a CPS function with the same signature as the simple CPS
/// translator.
/// In addition, it creates a helper function that takes the following arguments:
/// - the base address on the argument stack
/// - the current continuation
/// - the address to the last result. Initially it's set to be base address - 8.
/// The translated function body basically contains logic that transitions across states of the
/// continuation object.
struct ComplexCpsFunctionTranslator<'a, M: Module> {
    function_translator: SimpleFunctionTranslator<'a, M>,
    /// The pointer to the current continuation.
    continuation: Value,
    /// The pointer to the last result passed to the continuation. This is also the value needed to
    /// execute the next step.
    last_result: Value,
    /// The tip address at the beginning of the CPS impl function. This is computed from the last
    /// result pointer.
    start_tip_address: Value,
    /// The number of arguments of the current function.
    argument_count: usize,
    /// The ID of the current block. Initially it's zero, which means the first block right after
    /// entry block. The state tracked in the continuation is the block ID of the next block to
    /// execute.
    current_block_id: usize,
    /// All the blocks in this function. The first block is the entry block.
    blocks: Vec<Block>,
    /// Mapping from a block ending with a case to all the branch blocks, default branch block, and
    /// the block joining the branches. These branch blocks won't appear as the next block in the
    /// continuation state because they are not created due to calling an effectful function.
    case_blocks: HashMap<usize, (Vec<usize>, usize, usize)>,
    /// Local variables that are used in the current block. They will all need to be stored in the
    /// continuation object before calling the next effectful function.
    touched_vars_in_current_session: HashSet<usize>,
}

impl<'a, M: Module> Deref for ComplexCpsFunctionTranslator<'a, M> {
    type Target = SimpleFunctionTranslator<'a, M>;

    fn deref(&self) -> &Self::Target {
        &self.function_translator
    }
}

impl<'a, M: Module> DerefMut for ComplexCpsFunctionTranslator<'a, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.function_translator
    }
}

impl<'a, M: Module> ComplexCpsFunctionTranslator<'a, M> {
    fn compile_cps_function(
        name: &str,
        compiler: &mut Compiler<M>,
        function_definition: &FunctionDefinition,
        local_function_arg_types: &HashMap<String, (Vec<VType>, CType)>,
        cps_impl_func_id: FuncId,
        clir: &mut Option<&mut Vec<(String, String)>>,
    ) {
        let mut next_continuation: Value = Value::with_number(0).unwrap();
        let mut translator = SimpleFunctionTranslator::new(
            compiler,
            compiler.uniform_cps_func_signature.clone(),
            function_definition,
            local_function_arg_types,
            false,
            |function_builder, entry_block| {
                next_continuation = function_builder.block_params(entry_block)[1];
                function_builder.block_params(entry_block)[0]
            },
            // We don't do anything here and each block will load the parameters on demand.
            |_, _, _, _| None);
        let num_local_vars = function_definition.var_bound - function_definition.args.len();
        let continuation_size = 32 + num_local_vars * 8;
        let continuation_size_value = translator.function_builder.ins().iconst(I64, continuation_size as i64);
        let inst = translator.call_builtin_func(BuiltinFunction::Alloc, &[continuation_size_value]);
        let continuation = translator.function_builder.inst_results(inst)[0];
        // Initially sets the last result pointer to the base address -8 so that the tip address is
        // updated correctly when the continuation implementation function is called.
        let last_result_ptr = translator.function_builder.ins().iadd_imm(translator.base_address, -8);
        let cps_impl_func_ref = translator.module.declare_func_in_func(cps_impl_func_id, translator.function_builder.func);
        translator.function_builder.ins().return_call(cps_impl_func_ref, &[translator.base_address, continuation, last_result_ptr]);
        translator.function_builder.finalize();

        let cps_name = FunctionFlavor::Cps.decorate_name(name);
        let func_id = compiler.local_functions.get(&cps_name).unwrap();
        SimpleFunctionTranslator::define_function(&mut compiler.module, &mut compiler.ctx, &cps_name, *func_id, clir);
    }

    fn compile_cps_impl_function(
        name: &str,
        compiler: &mut Compiler<M>,
        function_definition: &FunctionDefinition,
        local_function_arg_types: &HashMap<String, (Vec<VType>, CType)>,
        num_blocks: usize,
        case_blocks: HashMap<usize, (Vec<usize>, usize, usize)>,
        clir: &mut Option<&mut Vec<(String, String)>>,
    ) -> FuncId {
        assert!(num_blocks > 1, "if there is only a single block, one should not create a cps_impl function at all!");
        let mut translator = ComplexCpsFunctionTranslator::new(
            compiler,
            function_definition,
            local_function_arg_types,
            num_blocks,
            case_blocks,
        );
        let typed_return_value = translator.translate_c_term_cps_impl(&function_definition.body, true);
        match typed_return_value {
            None => {
                // Nothing to do since tail call is already a terminating instruction
            }
            Some(_) => {
                // store return value on the argument stack
                let return_address = translator.store_return_value_on_argument_stack(
                    typed_return_value,
                    function_definition.args.len(),
                );

                let continuation = translator.continuation;
                let next_continuation = translator.function_builder.ins().load(I64, MemFlags::new(), continuation, 16);

                invoke_next_continuation_in_the_end(&mut translator, return_address, next_continuation);
            }
        }
        translator.function_translator.function_builder.finalize();

        let cps_impl_name = FunctionFlavor::CpsImpl.decorate_name(name);
        let func_id = compiler.module.declare_function(&cps_impl_name, Linkage::Local, &compiler.uniform_cps_impl_func_signature).unwrap();
        SimpleFunctionTranslator::define_function(&mut compiler.module, &mut compiler.ctx, &cps_impl_name, func_id, clir);
        func_id
    }

    fn new(
        compiler: &'a mut Compiler<M>,
        function_definition: &'a FunctionDefinition,
        local_function_arg_types: &'a HashMap<String, (Vec<VType>, CType)>,
        num_blocks: usize,
        case_blocks: HashMap<usize, (Vec<usize>, usize, usize)>,
    ) -> Self {
        // The values here are just a placeholders.
        let mut continuation: Value = Value::with_number(0).unwrap();
        let mut last_result_ptr: Value = Value::with_number(0).unwrap();
        let mut function_translator = SimpleFunctionTranslator::new(
            compiler,
            compiler.uniform_cps_impl_func_signature.clone(),
            function_definition,
            local_function_arg_types,
            false,
            |function_builder, entry_block| {
                continuation = function_builder.block_params(entry_block)[1];
                last_result_ptr = function_builder.block_params(entry_block)[2];
                function_builder.block_params(entry_block)[0]
            },
            // We don't do anything here and each block will load the parameters on demand.
            |_, _, _, _| None);
        let state = function_translator.function_builder.ins().load(I64, MemFlags::new(), continuation, 24);
        // local vars are stored in the continuation object starting at the fifth word
        function_translator.local_var_ptr = function_translator.function_builder.ins().iadd_imm(continuation, 32);

        // set the tip address according to the last result pointer.
        let start_tip_address = function_translator.function_builder.ins().iadd_imm(last_result_ptr, 8);
        function_translator.tip_address = start_tip_address;

        let last_result = function_translator.function_builder.ins().load(I64, MemFlags::new(), last_result_ptr, 0);

        let argument_count = function_definition.args.len();

        // create the switch table for jumping to the right block based on the state
        let mut blocks = Vec::new();
        let mut switch = Switch::new();

        // skip case blocks because these blocks won't suspend.
        let mut skipped_block_ids = HashSet::new();
        case_blocks.values().for_each(|(branch_block_ids, default_block_id, joining_block_id)| {
            skipped_block_ids.extend(branch_block_ids);
            skipped_block_ids.insert(*default_block_id);
            skipped_block_ids.insert(*joining_block_id);
        });
        for i in 0..num_blocks {
            let block = function_translator.function_builder.create_block();
            blocks.push(block);
            if !skipped_block_ids.contains(&i) {
                switch.set_entry(i as u128, block);
            }
        }
        let first_block = blocks[0];
        // The state number cannot be outside of the range of the switch table so the default block
        // is unreachable. Hence we just arbitrarily set it to the first block.
        switch.emit(&mut function_translator.function_builder, state, first_block);
        function_translator.function_builder.seal_block(first_block);
        function_translator.function_builder.switch_to_block(first_block);

        Self {
            function_translator,
            continuation,
            last_result,
            start_tip_address,
            argument_count,
            current_block_id: 0,
            blocks,
            case_blocks,
            touched_vars_in_current_session: HashSet::new(),
        }
    }

    fn translate_c_term_cps_impl(&mut self, c_term: &CTerm, is_tail: bool) -> TypedReturnValue {
        match c_term {
            CTerm::Redex { box function, args } => {
                self.function_translator.push_arg_v_terms(args);
                self.translate_c_term_cps_impl(function, is_tail)
            }
            CTerm::Force { thunk, may_have_complex_effects: true } => {
                let continuation = self.continuation;
                let func_pointer = self.process_thunk(thunk, continuation);

                let signature = self.uniform_cps_func_signature.clone();
                let sig_ref = self.function_builder.import_signature(signature);
                if is_tail {
                    let (new_base_address, next_continuation) = self.adjust_next_continuation_frame_height(continuation);
                    self.function_builder.ins().return_call_indirect(sig_ref, func_pointer, &[
                        new_base_address, next_continuation,
                    ]);
                    None
                } else {
                    self.pack_up_continuation();
                    let tip_address = self.tip_address;
                    self.function_builder.ins().return_call_indirect(sig_ref, func_pointer, &[
                        tip_address,
                        continuation,
                    ]);
                    self.advance_for_complex_effect()
                }
            }
            CTerm::Let { box t, bound_index, box body } => {
                let t_value = self.translate_c_term_cps_impl(t, false);
                self.local_vars[*bound_index] = t_value;
                self.touched_vars_in_current_session.insert(*bound_index);
                self.translate_c_term_cps_impl(body, is_tail)
            }
            CTerm::Def { name, may_have_complex_effects: true } => {
                let func_ref = self.get_local_function(name, FunctionFlavor::Cps);
                if is_tail {
                    let (new_base_address, next_continuation) = self.adjust_next_continuation_frame_height(self.continuation);
                    self.function_builder.ins().return_call(func_ref, &[new_base_address, next_continuation]);
                    None
                } else {
                    self.pack_up_continuation();
                    let args = [self.tip_address, self.continuation];
                    self.function_builder.ins().return_call(func_ref, &args);
                    self.advance_for_complex_effect()
                }
            }
            CTerm::CaseInt { t, result_type, branches, default_branch } => {
                let (branch_block_ids, default_block_id, joining_block_id) = &self.case_blocks[&self.current_block_id].clone();
                let t_value = self.translate_v_term(t);
                let t_value = self.convert_to_special(t_value, Integer);

                // Create table jump
                let branch_body_and_blocks: Vec<_> = branches.iter().zip(branch_block_ids.iter().map(|id| (*id, self.blocks[*id]))).collect();
                let mut switch = Switch::new();
                for ((value, _), (_, branch_block)) in branch_body_and_blocks.iter() {
                    switch.set_entry(*value as u128, *branch_block);
                }
                let default_block = self.blocks[*default_block_id];
                switch.emit(&mut self.function_builder, t_value, default_block);

                // Set up joining block
                let joining_block = self.blocks[*joining_block_id];
                let result_v_type = match result_type {
                    CType::Default => &Uniform,
                    CType::SpecializedF(vty) => vty,
                };
                // return value
                self.function_builder.append_block_param(joining_block, result_v_type.get_type());
                // tip address
                self.function_builder.append_block_param(joining_block, I64);

                // Fill branch blocks
                let start_tip_address = self.tip_address;
                for ((_, c_term), (block_id, branch_block)) in branch_body_and_blocks.into_iter() {
                    self.advance_for_case();
                    assert_eq!(self.current_block_id, block_id);
                    self.tip_address = start_tip_address;
                    self.create_branch_block(branch_block, is_tail, joining_block, result_v_type, Some(c_term));
                }

                self.advance_for_case();
                assert_eq!(self.current_block_id, *default_block_id);
                self.tip_address = start_tip_address;
                self.create_branch_block(default_block, is_tail, joining_block, result_v_type, match default_branch {
                    None => None,
                    Some(box branch) => Some(branch),
                });

                // Switch to joining block for future code generation
                self.advance_for_case();
                assert_eq!(self.current_block_id, *joining_block_id);
                self.tip_address = self.function_builder.block_params(joining_block)[1];
                Some((self.function_builder.block_params(joining_block)[0], *result_v_type))
            }
            CTerm::OperationCall { eff, args, complex: true } => {
                let eff_value = self.translate_v_term(eff);
                let eff_value = self.convert_to_uniform(eff_value);
                self.push_arg_v_terms(args);
                let (new_base_address, continuation) = if is_tail {
                    self.adjust_next_continuation_frame_height(self.continuation)
                } else {
                    self.pack_up_continuation();
                    (self.tip_address, self.continuation)
                };

                self.handle_complex_operation_call(eff_value, new_base_address, continuation, args.len());
                if is_tail {
                    None
                } else {
                    self.advance_for_complex_effect()
                }
            }
            CTerm::Handler { .. } => {
                let continuation = self.continuation;
                self.translate_handler(c_term, is_tail, continuation)
            }
            _ => self.translate_c_term(c_term, is_tail),
        }
    }

    fn adjust_next_continuation_frame_height(&mut self, continuation: Value) -> (Value, Value) {
        let next_continuation = self.function_builder.ins().load(I64, MemFlags::new(), continuation, 16);

        let new_base_address = compute_cps_tail_call_base_address(self, next_continuation);
        (new_base_address, next_continuation)
    }

    fn create_branch_block(&mut self, branch_block: Block, is_tail: bool, joining_block: Block, result_v_type: &VType, branch: Option<&CTerm>) {
        self.function_builder.switch_to_block(branch_block);
        let typed_return_value = match branch {
            None => {
                self.function_builder.ins().trap(TrapCode::UnreachableCodeReached);
                None
            }
            Some(branch) => self.translate_c_term_cps_impl(branch, is_tail),
        };
        match typed_return_value {
            None => {
                // Nothing to do since tail call is already a terminating instruction.
            }
            Some(..) => {
                let value = self.adapt_type(typed_return_value, result_v_type);
                let tip_address = self.tip_address;
                self.function_builder.ins().jump(joining_block, &[value, tip_address]);
            }
        }
    }

    fn pack_up_continuation(&mut self) {
        let continuation = self.continuation;

        // Store current height
        let tip_address = self.tip_address;
        let base_address = self.base_address;
        let arg_stack_frame_height_bytes = self.function_builder.ins().isub(tip_address, base_address);
        let arg_stack_frame_height = self.function_builder.ins().ushr_imm(arg_stack_frame_height_bytes, 3);
        self.function_builder.ins().store(MemFlags::new(), arg_stack_frame_height, continuation, 8);

        // Store next state
        let current_block_id = self.current_block_id;
        let next_block_id = self.function_builder.ins().iconst(I64, current_block_id as i64 + 1);
        self.function_builder.ins().store(MemFlags::new(), next_block_id, continuation, 24);

        // Store local vars
        let local_var_ptr = self.local_var_ptr;
        let mut touched_vars: Vec<_> = self.touched_vars_in_current_session.iter().copied().collect();
        touched_vars.sort();
        for index in touched_vars {
            let local_var = self.local_vars[index];
            let value = self.convert_to_uniform(local_var);
            self.function_builder.ins().store(MemFlags::new(), value, local_var_ptr, (index * 8) as i32);
        }
    }

    fn advance_for_complex_effect(&mut self) -> TypedReturnValue {
        self.touched_vars_in_current_session.clear();
        // Clear up local vars so next block can reload them from the continuation object.
        for v in self.local_vars.iter_mut() {
            *v = None
        }
        self.current_block_id += 1;
        let next_block = self.blocks[self.current_block_id];
        self.function_builder.seal_block(next_block);
        self.function_builder.switch_to_block(next_block);
        self.tip_address = self.start_tip_address;
        Some((self.last_result, Uniform))
    }

    fn advance_for_case(&mut self) {
        self.current_block_id += 1;
        let next_block = self.blocks[self.current_block_id];
        self.function_builder.seal_block(next_block);
        self.function_builder.switch_to_block(next_block);
    }
}

fn compute_cps_tail_call_base_address<M: Module>(translator: &mut SimpleFunctionTranslator<M>, next_continuation: Value) -> Value {
    // accommodate the height of the next continuation is updated here because tail
    // call causes the next continuation to be directly passed to the callee, which,
    // when later invokes the next continuation, will need to compute the base
    // address from this new height. The height can be different because the
    // callee args are effectively altered by the current function.
    let new_base_address = translator.copy_tail_call_args_and_get_new_base();
    let next_continuation_height = translator.function_builder.ins().load(I64, MemFlags::new(), next_continuation, 8);
    let base_address = translator.base_address;
    let next_continuation_height_bytes = translator.function_builder.ins().ishl_imm(next_continuation_height, 3);
    let next_continuation_base = translator.function_builder.ins().iadd(base_address, next_continuation_height_bytes);
    let new_next_continuation_height_bytes = translator.function_builder.ins().isub(new_base_address, next_continuation_base);
    let new_next_continuation_height = translator.function_builder.ins().ushr_imm(new_next_continuation_height_bytes, 3);
    translator.function_builder.ins().store(MemFlags::new(), new_next_continuation_height, next_continuation, 8);
    new_base_address
}

fn invoke_next_continuation_in_the_end<M: Module>(translator: &mut SimpleFunctionTranslator<M>, return_address: Value, next_continuation: Value) {
    // compute next base address
    let next_continuation_height = translator.function_builder.ins().load(I64, MemFlags::new(), next_continuation, 8);
    let next_continuation_height_bytes = translator.function_builder.ins().ishl_imm(next_continuation_height, 3);
    let base_address = translator.base_address;
    let next_base_address = translator.function_builder.ins().iadd(base_address, next_continuation_height_bytes);

    // get next continuation impl function
    let next_continuation_impl = translator.function_builder.ins().load(I64, MemFlags::new(), next_continuation, 0);

    // call the next continuation
    let signature = translator.uniform_cps_impl_func_signature.clone();
    let sig_ref = translator.function_builder.import_signature(signature);
    translator.function_builder.ins().return_call_indirect(sig_ref, next_continuation_impl, &[next_base_address, next_continuation, return_address]);
}
