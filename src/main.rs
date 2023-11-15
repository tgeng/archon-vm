#![feature(trait_alias)]
#![feature(box_patterns)]
#![allow(dead_code)]
#![feature(let_chains)]

mod term;
mod signature;
mod visitor;
mod u_term;
mod transpiler;
mod parser;
mod test_utils;
mod free_var;
mod compiler;
mod transformer;
mod primitive_functions;

fn main() {
    println!("Hello, world!");
}