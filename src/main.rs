mod ast_parser;
mod codegen;
mod parser;
mod tokenizer;

use ast_parser::expr;
use codegen::generate;
use std::env;
use tokenizer::tokenizer;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input: &str = args[1].as_str();
    let tokenizer = tokenizer();
    let result = tokenizer.parse(input);
    let tokens = result.expect("failed to tokenize").1;
    let node = expr(&tokens).expect("failed to build AST").1;

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    println!("  mov rax, {}", generate(node));
    println!("  ret");
}
