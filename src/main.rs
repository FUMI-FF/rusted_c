mod ast_parser;
mod codegen;
mod ir;
mod parser;
mod register;
mod tokenizer;

use ast_parser::ast_parser;
use codegen::generate;
use std::env;
use tokenizer::tokenizer;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input: &[u8] = args[1].as_bytes();
    let tokenizer = tokenizer();
    let result = tokenizer.parse(input);
    let tokens = result.expect("failed to tokenize").1;
    let node = ast_parser().parse(&tokens).expect("failed to build AST").1;

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    generate(&node);
}
