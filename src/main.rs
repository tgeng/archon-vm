#![feature(trait_alias)]
#![feature(box_patterns)]
#![allow(dead_code)]
#![feature(let_chains)]

mod compiler;
mod free_var;
// mod parser;
mod primitive_functions;
mod signature;
mod term;
mod test_utils;
mod transformer;
// mod transpiler;
// mod u_term;
mod visitor;

fn main() {
    println!("Hello, world!");
}