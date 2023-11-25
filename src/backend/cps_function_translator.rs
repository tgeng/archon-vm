use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use cranelift::prelude::{AbiParam, Block, InstBuilder, MemFlags, TrapCode, Value};
use cranelift::prelude::types::I64;
use cranelift_module::Module;
use cranelift::frontend::Switch;
use crate::ast::signature::{FunctionDefinition};
use crate::ast::term::{CTerm, CType, VType};
use crate::ast::term::SpecializedType::Integer;
use crate::ast::term::VType::Uniform;
use crate::backend::common::{BuiltinFunction, FunctionFlavor, HasType, TypedReturnValue};
use crate::backend::compiler::Compiler;
use crate::backend::simple_function_translator::SimpleFunctionTranslator;

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
pub struct CpsFunctionTranslator<'a, M: Module> {
    function_translator: SimpleFunctionTranslator<'a, M>,
}

/// The translated native function takes the following arguments:
/// - the base address on the argument stack
/// - the current continuation
/// - the address to the last result. Initially it's set to be base address - 8.
/// The translated function body basically contains logic that transitions across states of the
/// continuation object.
struct CpsImplFunctionTranslator<'a, M: Module> {
    function_translator: SimpleFunctionTranslator<'a, M>,
    /// The pointer to the current continuation.
    continuation: Value,
    /// The last result passed to the continuation. This is also the value needed to execute the
    /// next step.
    last_result: Value,
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

impl<'a, M: Module> Deref for CpsImplFunctionTranslator<'a, M> {
    type Target = SimpleFunctionTranslator<'a, M>;

    fn deref(&self) -> &Self::Target {
        &self.function_translator
    }
}

impl<'a, M: Module> DerefMut for CpsImplFunctionTranslator<'a, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.function_translator
    }
}

impl<'a, M: Module> CpsImplFunctionTranslator<'a, M> {
    fn compile_cps_impl_function(
        compiler: &mut Compiler<M>,
        function_definition: &FunctionDefinition,
        local_function_arg_types: &HashMap<String, (Vec<VType>, CType)>,
        num_blocks: usize,
        case_blocks: HashMap<usize, (Vec<usize>, usize, usize)>,
    ) {
        let signature = compiler.uniform_cps_func_signature.clone();
        let mut translator = CpsImplFunctionTranslator::new(
            compiler,
            function_definition,
            local_function_arg_types,
            num_blocks,
            case_blocks,
        );
        let typed_return_value = translator.translate_c_term_cps(&function_definition.body, true);
        match typed_return_value {
            None => {
                // Nothing to do since tail call is already a terminating instruction
            }
            Some(_) => {
                // store return value on the argument stack
                let value = translator.convert_to_uniform(typed_return_value);
                let return_address_offset = (function_definition.args.len() as i64 - 1) * 8;
                let base_address = translator.base_address;
                let return_address = translator.function_builder.ins().iadd_imm(base_address, return_address_offset);
                translator.function_builder.ins().store(MemFlags::new(), value, return_address, 0);

                let continuation = translator.continuation;
                let next_continuation = translator.function_builder.ins().load(I64, MemFlags::new(), continuation, 8);
                // compute next base address
                let next_continuation_height = translator.function_builder.ins().load(I64, MemFlags::new(), next_continuation, 16);
                let next_base_address = translator.function_builder.ins().iadd(base_address, next_continuation_height);

                // get next continuation impl function
                let next_continuation_impl = translator.function_builder.ins().load(I64, MemFlags::new(), next_continuation, 0);

                // call the next continuation
                let sig_ref = translator.function_builder.import_signature(signature);
                translator.function_builder.ins().return_call_indirect(sig_ref, next_continuation_impl, &[next_base_address, next_continuation, return_address]);
            }
        }
        translator.function_translator.function_builder.finalize();
    }

    fn new(
        compiler: &'a mut Compiler<M>,
        function_definition: &'a FunctionDefinition,
        local_function_arg_types: &'a HashMap<String, (Vec<VType>, CType)>,
        num_blocks: usize,
        case_blocks: HashMap<usize, (Vec<usize>, usize, usize)>,
    ) -> Self {
        assert!(num_blocks > 1, "if there is only a single block, one should not create a cps_impl function at all!");
        let mut sig = compiler.module.make_signature();
        // continuation
        sig.params.push(AbiParam::new(I64));
        // last result as pointer to result on the argument stack
        sig.params.push(AbiParam::new(I64));
        // the result is always tail returned from the parent continuation, which, in turn, can be
        // a special trivial continuation that just returns the address of the first argument.
        sig.returns.push(AbiParam::new(I64));
        // The values here are just a placeholders.
        let mut continuation: Value = Value::with_number(0).unwrap();
        let mut last_result_ptr: Value = Value::with_number(0).unwrap();
        let mut function_translator = SimpleFunctionTranslator::new(
            compiler,
            sig,
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
        let last_result = function_translator.function_builder.ins().load(I64, MemFlags::new(), last_result_ptr, 0);
        function_translator.tip_address = function_translator.function_builder.ins().iadd_imm(last_result_ptr, 8);

        let argument_count = function_definition.args.len();

        // create the switch table for jumping to the right block based on the state
        let mut blocks = Vec::new();
        let mut switch = Switch::new();
        for i in 0..num_blocks {
            let block = function_translator.function_builder.create_block();
            blocks.push(block);
            switch.set_entry(i as u128, block);
        }
        let first_block = blocks[0];
        switch.emit(&mut function_translator.function_builder, state, first_block);
        function_translator.function_builder.seal_block(first_block);
        function_translator.function_builder.switch_to_block(first_block);

        Self {
            function_translator,
            continuation,
            last_result,
            argument_count,
            current_block_id: 0,
            blocks,
            case_blocks,
            touched_vars_in_current_session: HashSet::new(),
        }
    }

    fn translate_c_term_cps(&mut self, c_term: &CTerm, is_tail: bool) -> TypedReturnValue {
        match c_term {
            CTerm::Redex { box function, args } => {
                self.function_translator.push_arg_v_terms(args);
                self.translate_c_term_cps(function, is_tail)
            }
            CTerm::Force { thunk, may_have_complex_effects: true } => {
                let thunk_value = self.translate_v_term(thunk);
                // We must change the thunk value to uniform representation because the built-in
                // function expects a uniform representation in order to tell a thunk from a raw
                // function pointer.
                let thunk_value = self.convert_to_uniform(thunk_value);

                let tip_address = self.tip_address;
                let tip_address_slot = self.tip_address_slot;
                self.function_builder.ins().stack_store(tip_address, tip_address_slot, 0);
                let tip_address_ptr = self.function_builder.ins().stack_addr(I64, tip_address_slot, 0);
                let inst = self.call_builtin_func(BuiltinFunction::ForceThunk, &[thunk_value, tip_address_ptr]);
                let func_pointer = self.function_builder.inst_results(inst)[0];
                let tip_address = self.function_builder.ins().stack_load(I64, tip_address_slot, 0);

                let signature = self.uniform_func_signature.clone();
                let sig_ref = self.function_builder.import_signature(signature);
                let r = if is_tail {
                    let (new_base_address, next_continuation) = self.adjust_next_continuation_frame_height(self.continuation);
                    self.function_builder.ins().return_call_indirect(sig_ref, func_pointer, &[
                        new_base_address, next_continuation,
                    ]);
                    None
                } else {
                    self.pack_up_continuation();
                    let continuation = self.continuation;
                    self.function_builder.ins().return_call_indirect(sig_ref, func_pointer, &[
                        tip_address,
                        continuation,
                    ]);
                    Some((self.last_result, Uniform))
                };
                self.advance();
                r
            }
            CTerm::Let { box t, bound_index, box body } => {
                let t_value = self.translate_c_term_cps(t, false);
                self.local_vars[*bound_index] = t_value;
                self.touched_vars_in_current_session.insert(*bound_index);
                self.translate_c_term_cps(body, is_tail)
            }
            CTerm::Def { name, may_have_complex_effects: true } => {
                let (func_ref, _) = self.get_local_function(name, FunctionFlavor::Cps);
                let r = if is_tail {
                    let (new_base_address, next_continuation) = self.adjust_next_continuation_frame_height(self.continuation);
                    self.function_builder.ins().return_call(func_ref, &[new_base_address, next_continuation]);
                    None
                } else {
                    self.pack_up_continuation();
                    let args = [self.tip_address, self.continuation];
                    self.function_builder.ins().return_call(func_ref, &args);
                    Some((self.last_result, Uniform))
                };
                self.advance();
                r
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
                let result_value_type = result_v_type.get_type();
                self.function_builder.append_block_param(joining_block, result_value_type);

                // Fill branch blocks
                for ((_, c_term), (block_id, branch_block)) in branch_body_and_blocks.into_iter() {
                    self.current_block_id = block_id;
                    self.create_branch_block(branch_block, is_tail, joining_block, result_v_type, Some(c_term));
                }

                self.current_block_id = *default_block_id;
                self.create_branch_block(default_block, is_tail, joining_block, result_v_type, match default_branch {
                    None => None,
                    Some(box branch) => Some(branch),
                });

                // Switch to joining block for future code generation
                self.current_block_id = *joining_block_id;
                self.function_builder.seal_block(joining_block);
                self.function_builder.switch_to_block(joining_block);
                Some((self.function_builder.block_params(joining_block)[0], *result_v_type))
            }
            CTerm::OperationCall { .. } => todo!(),
            CTerm::Handler { .. } => todo!(),
            CTerm::ResumeContinuation { .. } => todo!(),
            CTerm::DisposeContinuation { .. } => todo!(),
            CTerm::ReplicateContinuation { .. } => todo!(),
            CTerm::LongJump { .. } => todo!(),
            _ => self.translate_c_term(c_term, is_tail),
        }
    }

    fn adjust_next_continuation_frame_height(&mut self, continuation: Value) -> (Value, Value) {
// accommodate the height of the next continuation is updated here because tail
        // call causes the next continuation to be directly passed to the callee, which,
        // when later invokes the next continuation, will need to compute the base
        // address from this new height. The height can be different because the
        // callee args are effectively altered by the current function.
        let new_base_address = self.copy_tail_call_args_and_get_new_base();
        let next_continuation = self.function_builder.ins().load(I64, MemFlags::new(), continuation, 8);
        let next_continuation_height = self.function_builder.ins().load(I64, MemFlags::new(), next_continuation, 16);
        let base_address = self.base_address;
        let next_continuation_base = self.function_builder.ins().iadd(base_address, next_continuation_height);
        let new_next_continuation_height = self.function_builder.ins().isub(new_base_address, next_continuation_base);
        self.function_builder.ins().store(MemFlags::new(), new_next_continuation_height, next_continuation, 16);
        (new_base_address, next_continuation)
    }

    fn create_branch_block(&mut self, branch_block: Block, is_tail: bool, joining_block: Block, result_v_type: &VType, branch: Option<&CTerm>) {
        self.function_builder.switch_to_block(branch_block);
        let typed_return_value = match branch {
            None => {
                self.function_builder.ins().trap(TrapCode::UnreachableCodeReached);
                None
            }
            Some(branch) => self.translate_c_term_cps(branch, is_tail),
        };
        match typed_return_value {
            None => {
                // Nothing to do since tail call is already a terminating instruction.
            }
            Some(..) => {
                let value = self.adapt_type(typed_return_value, result_v_type);
                self.function_builder.ins().jump(joining_block, &[value]);
            }
        }
    }

    fn pack_up_continuation(&mut self) {
        let continuation = self.continuation;

        // Store current height
        let tip_address = self.tip_address;
        let base_address = self.base_address;
        let arg_stack_frame_height = self.function_builder.ins().isub(tip_address, base_address);
        self.function_builder.ins().store(MemFlags::new(), arg_stack_frame_height, continuation, 16);

        // Store next state
        let current_block_id = self.current_block_id;
        let next_block_id = self.function_builder.ins().iconst(I64, current_block_id as i64 + 1);
        self.function_builder.ins().store(MemFlags::new(), next_block_id, continuation, 24);

        // Store local vars
        let local_var_ptr = self.local_var_ptr;
        let touched_vars: Vec<_> = self.touched_vars_in_current_session.iter().copied().collect();
        for index in touched_vars {
            let local_var = self.local_vars[index];
            let value = self.convert_to_uniform(local_var);
            self.function_builder.ins().store(MemFlags::new(), value, local_var_ptr, (index * 8) as i32);
        }
    }

    fn advance(&mut self) {
        // Clear up local vars so next block can reload them from the continuation object.
        for v in self.local_vars.iter_mut() {
            *v = None
        }
        self.current_block_id += 1;
        let next_block = self.blocks[self.current_block_id];
        self.function_builder.seal_block(next_block);
        self.function_builder.switch_to_block(next_block);
    }
}
