use std::collections::HashMap;
use cranelift::prelude::{Block, Value};
use cranelift_module::Module;
use crate::ast::signature::FunctionDefinition;
use crate::ast::term::{CTerm, CType, VType};
use crate::backend::common::TypedReturnValue;
use crate::backend::compiler::Compiler;
use crate::backend::function_translator::FunctionTranslator;

pub struct CpsFunctionTranslator<'a, M: Module> {
    function_translator: FunctionTranslator<'a, M>,
    /// The pointer to the parent continuation when translating this function.
    parent_continuation: Value,
    /// The pointer to the current continuation, if needed. It's not needed if there are only one
    /// state possible inside this function. For example, if the function simply calls another
    /// effectful function at the end, then there is no need to create a continuation for this
    /// function. Simply pass the parent continuation to the callee is sufficient.
    current_continuation: Option<Value>,
    /// The pointer to the start of the local variable storage allocated inside the current
    /// continuation object. It's only set if the current continuation is allocated.
    local_vars: Option<Value>,
    /// The ID of the current block. Initially it's zero, which means the entry block. The state
    /// tracked in the continuation is the block ID of the next block to execute.
    current_block_id: usize,
    /// All the blocks in this function. The first block is the entry block.
    blocks: &'a mut [Block],
    /// Mapping from a block ending with a case to all the branch blocks. These branch blocks won't
    /// appear as the next block in the continuation state because they are not created due to
    /// calling an effectful function.
    case_blocks: HashMap<usize, Vec<usize>>,
}

impl<'a, M: Module> CpsFunctionTranslator<'a, M> {
    pub fn new(
        compiler: &'a mut Compiler<M>,
        function_definition: FunctionDefinition,
        local_function_arg_types: &'a HashMap<String, (Vec<VType>, CType)>,
    ) -> Self {
        todo!()
    }

    pub fn translate_c_term_cps(&mut self, c_term: &CTerm, is_tail: bool) -> TypedReturnValue {
        match c_term {
            CTerm::Redex { box function, args } => {
                self.function_translator.push_arg_v_terms(args);
                self.translate_c_term_cps(function, is_tail)
            }
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
            _ => self.function_translator.translate_c_term(c_term, is_tail),
        }
    }
}
