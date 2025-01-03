use crate::ast::term::SpecializedType;
use crate::ast::term::VType;
use cranelift::prelude::{FunctionBuilder, InstBuilder, Value};
use phf::phf_map;
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
        code_gen: |_function_builder, args| {
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
    "_int_gt" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer), Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 2);
            function_builder.ins().icmp(cranelift::prelude::IntCC::SignedGreaterThan, args[0], args[1])
        }
    },
    "_int_lt" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer), Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 2);
            function_builder.ins().icmp(cranelift::prelude::IntCC::SignedLessThan, args[0], args[1])
        }
    },
    "_int_gte" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer), Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 2);
            function_builder.ins().icmp(cranelift::prelude::IntCC::SignedGreaterThanOrEqual, args[0], args[1])
        }
    },
    "_int_lte" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer), Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 2);
            function_builder.ins().icmp(cranelift::prelude::IntCC::SignedLessThanOrEqual, args[0], args[1])
        }
    },
    "_int_eq" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer), Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 2);
            function_builder.ins().icmp(cranelift::prelude::IntCC::Equal, args[0], args[1])
        }
    },
    "_int_ne" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer), Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 2);
            function_builder.ins().icmp(cranelift::prelude::IntCC::NotEqual, args[0], args[1])
        }
    },
    "_bool_not" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 1);
            let v = function_builder.ins().bnot(args[0]);
            function_builder.ins().band_imm(v, 1)
        }
    },
    "_bool_and" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer), Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 2);
            let v = function_builder.ins().band(args[0], args[1]);
            function_builder.ins().band_imm(v, 1)
        }
    },
    "_bool_or" => &PrimitiveFunction {
        arg_types: &[Specialized(Integer), Specialized(Integer)],
        return_type: Specialized(Integer),
        code_gen: |function_builder, args| {
            assert_eq!(args.len(), 2);
            let v = function_builder.ins().bor(args[0], args[1]);
            function_builder.ins().band_imm(v, 1)
        }
    },
};
