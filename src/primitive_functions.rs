use phf::phf_map;
use crate::term::VType;
use crate::term::PrimitiveType;
use PrimitiveType::*;
use VType::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PrimitiveFunction {
    pub arg_types: &'static [VType],
    pub return_type: VType,
}

pub static PRIMITIVE_FUNCTIONS: phf::Map<&'static str, &'static PrimitiveFunction> = phf_map! {
    "_int_pos" => &PrimitiveFunction { arg_types: &[Specialized(Integer)], return_type: Specialized(Integer) },
    "_int_neg" => &PrimitiveFunction { arg_types: &[Specialized(Integer)], return_type: Specialized(Integer) },
    "_int_add" => &PrimitiveFunction { arg_types: &[Specialized(Integer), Specialized(Integer)], return_type: Specialized(Integer) },
    "_int_sub" => &PrimitiveFunction { arg_types: &[Specialized(Integer), Specialized(Integer)], return_type: Specialized(Integer) },
    "_int_mul" => &PrimitiveFunction { arg_types: &[Specialized(Integer), Specialized(Integer)], return_type: Specialized(Integer) },
    "_int_mod" => &PrimitiveFunction { arg_types: &[Specialized(Integer), Specialized(Integer)], return_type: Specialized(Integer) },
};

