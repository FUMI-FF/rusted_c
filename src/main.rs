mod parser;
mod tokenizer;

use std::env;
use tokenizer::{tokenizer, Token};

fn main() {
    let args: Vec<String> = env::args().collect();
    let input: &str = args[1].as_str();
    let tokenizer = tokenizer();
    let result = tokenizer.parse(input);
    let tokens = result.expect("failed to tokenize").1;
    let fst_token = tokens.get(0).expect("first token must exist");
    let fst_val = match fst_token {
        Token::Int(i) => i,
        _ => panic!("first token must be an interger"),
    };

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    println!("  mov rax, {}", fst_val);

    let mut i = 1;
    while i < tokens.len() {
        match (tokens.get(i), tokens.get(i + 1)) {
            (Some(Token::Symbol('+')), Some(Token::Int(i))) => {
                println!("  add rax, {}", i);
            }
            (Some(Token::Symbol('-')), Some(Token::Int(i))) => {
                println!("  sub rax, {}", i);
            }
            _ => panic!("unexpected tokens"),
        }
        i += 2;
    }

    println!("  ret");
}
