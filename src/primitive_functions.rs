use cranelift::prelude::{FunctionBuilder, InstBuilder, Value};
use phf::phf_map;
use crate::term::VType;
use crate::term::SpecializedType;
use SpecializedType::*;
use VType::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PrimitiveFunction {
    pub arg_types: &'static [VType],
    pub return_type: VType,
    pub code_gen: fn(&mut FunctionBuilder, &[Value]) -> Value,
}

pub static PRIMITIVE_FUNCTIONS: phf::Map<&'static str, &'static PrimitiveFunction> = phf_map! {
    "_int_pos" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 1);
            args[0]
        }
    },
    "_int_neg" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 1);
            function_builder.ins().irsub_imm(args[0], 0)
        }
    },
    "_int_add" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer), Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 2);
            function_builder.ins().iadd(args[0], args[1])
        }
    },
    "_int_sub" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer), Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 2);
            function_builder.ins().isub(args[0], args[1])
        }
    },
    "_int_mul" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer), Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 2);
            function_builder.ins().imul(args[0], args[1])
        }
    },
    "_int_mod" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer), Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 2);
            function_builder.ins().srem(args[0], args[1])
        }
    },
};

