use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use cranelift::prelude::{AbiParam, Block, InstBuilder, MemFlags, Value};
use cranelift::prelude::types::I64;
use cranelift_module::Module;
use cranelift::frontend::Switch;
use crate::ast::signature::{FunctionDefinition};
use crate::ast::term::{CTerm, CType, VTerm, VType};
use crate::ast::term::VType::Uniform;
use crate::backend::common::TypedReturnValue;
use crate::backend::compiler::Compiler;
use crate::backend::function_translator::FunctionTranslator;

pub struct CpsFunctionTranslator<'a, M: Module> {
    function_translator: FunctionTranslator<'a, M>,
}

struct CpsImplFunctionTranslator<'a, M: Module> {
    function_translator: FunctionTranslator<'a, M>,
    /// The pointer to the current continuation.
    continuation: Value,
    /// The last result passed to the continuation. This is also the value needed to execute the
    /// next step.
    last_result: Value,
    /// The pointer to the start of the local variable storage allocated inside the current
    /// continuation object. Note that function arguments are not stored in the continuation object,
    /// so the local variables are offset by the number of function arguments.
    local_var_ptr: Value,
    /// The number of arguments of the current function.
    argument_count: usize,
    /// The ID of the current block. Initially it's zero, which means the first block right after
    /// entry block. The state tracked in the continuation is the block ID of the next block to
    /// execute.
    current_block_id: usize,
    /// All the blocks in this function. The first block is the entry block.
    blocks: Vec<Block>,
    /// Mapping from a block ending with a case to all the branch blocks. These branch blocks won't
    /// appear as the next block in the continuation state because they are not created due to
    /// calling an effectful function.
    case_blocks: HashMap<usize, Vec<usize>>,
    /// Local variables that are used in the current block. They will all need to be stored in the
    /// continuation object before calling the next effectful function.
    touched_vars_in_current_session: HashSet<usize>,
}

impl<'a, M: Module> Deref for CpsImplFunctionTranslator<'a, M> {
    type Target = FunctionTranslator<'a, M>;

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
    fn new(
        compiler: &'a mut Compiler<M>,
        function_definition: &'a FunctionDefinition,
        local_function_arg_types: &'a HashMap<String, (Vec<VType>, CType)>,
        num_blocks: usize,
        case_blocks: HashMap<usize, Vec<usize>>,
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
        let mut last_result: Value = Value::with_number(0).unwrap();
        let mut function_translator = FunctionTranslator::new(
            compiler,
            sig,
            function_definition,
            local_function_arg_types,
            false,
            |function_builder, entry_block| {
                continuation = function_builder.block_params(entry_block)[0];
                last_result = function_builder.block_params(entry_block)[1];
                // The third word in the continuation object is the base address
                function_builder.ins().load(I64, MemFlags::new(), continuation, 16)
            },
            // We don't do anything here and each block will load the parameters on demand.
            |_, _, _, _| None);
        let state = function_translator.function_builder.ins().load(I64, MemFlags::new(), continuation, 24);
        // local vars are stored in the continuation object starting at the fifth word
        let local_var_ptr = function_translator.function_builder.ins().iadd_imm(continuation, 32);
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
            local_var_ptr,
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
            CTerm::Return { .. } => todo!(),
            CTerm::Force { .. } => todo!(),
            CTerm::Def { .. } => todo!(),
            CTerm::CaseInt { .. } => todo!(),
            CTerm::SpecializedFunctionCall { .. } => todo!(),
            CTerm::OperationCall { .. } => todo!(),
            CTerm::Handler { .. } => todo!(),
            CTerm::ResumeContinuation { .. } => todo!(),
            CTerm::DisposeContinuation { .. } => todo!(),
            CTerm::ReplicateContinuation { .. } => todo!(),
            CTerm::LongReturn { .. } => todo!(),
            CTerm::Let { .. } => todo!(),
            CTerm::MemGet { .. } => todo!(),
            CTerm::MemSet { .. } => todo!(),
            CTerm::PrimitiveCall { .. } => todo!(),
        }
    }

    fn translate_v_term_cps(&mut self, v_term: &VTerm) -> TypedReturnValue {
        match v_term {
            VTerm::Var { index } => {
                match self.local_vars[*index] {
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
                }
            }
            _ => self.translate_v_term(v_term),
        }
    }
}
