mod ast_parser;
mod codegen;
mod ir;
mod parser;
mod register;
mod tokenizer;

use ast_parser::ast_parser;
use clap::Parser;
use codegen::gen_x86;
use codegen::ProgramEnvironment;
use ir::gen_ir;
use register::allocate_register;
use tokenizer::tokenizer;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    input: String,
    #[arg(short, long)]
    debug: bool,
}

fn main() {
    let args = Args::parse();
    let input: &[u8] = args.input.as_bytes();
    let result = tokenizer().parse(input);
    let tokens = result.expect("failed to tokenize").1;
    if args.debug {
        eprintln!("tokens: {:?}", tokens);
    }
    let node = ast_parser().parse(&tokens).expect("failed to build AST").1;
    if args.debug {
        eprintln!("ast node: {:?}", node);
    }
    let mut env = ProgramEnvironment::new();
    let ir_vec = gen_ir(&node, &mut env);
    let ir_vec = allocate_register(ir_vec, &mut env);
    if args.debug {
        eprintln!("irs: {:?}", ir_vec);
    }

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    gen_x86(ir_vec, &mut env);
}
