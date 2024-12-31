mod ast_parser;
mod codegen;
mod ir;
mod parser;
mod register;
mod tokenizer;

use ast_parser::ast_parser;
use codegen::gen_x86;
use codegen::ProgramEnvironment;
use ir::gen_ir;
use register::allocate_register;
use std::env;
use tokenizer::tokenizer;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input: &[u8] = args[1].as_bytes();
    let result = tokenizer().parse(input);
    let tokens = result.expect("failed to tokenize").1;
    let node = ast_parser().parse(&tokens).expect("failed to build AST").1;
    let mut env = ProgramEnvironment::new();
    let ir_vec = gen_ir(&node, &mut env);
    let ir_vec = allocate_register(ir_vec, &mut env);

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    gen_x86(ir_vec, &mut env);
}
