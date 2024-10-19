mod parser;
mod tokenizer;

use std::env;
use tokenizer::{tokenizer, Token};

enum Node {
    Number(i32),
    Binary {
        op: String,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
}

type ASTResult<'a> = Result<(&'a [Token], Node), String>;

fn number(tokens: &[Token]) -> ASTResult {
    match tokens[0] {
        Token::Int(val) => Ok((&tokens[1..], Node::Number(val))),
        _ => Err(format!("number expected, but got {:?}", tokens[0])),
    }
}

fn expr<'a>(tokens: &'a [Token]) -> ASTResult<'a> {
    let (mut cur, mut lhs) = number(tokens).expect("must be a number");

    while cur.len() > 0 {
        let op = match cur[0] {
            Token::Symbol(op) => op,
            _ => return Err(format!("operator exepcted, but got {:?}", tokens[0])),
        };
        let (next, number) = number(&cur[1..]).expect("must be a number");
        lhs = Node::Binary {
            op: op.to_string(),
            lhs: Box::new(lhs),
            rhs: Box::new(number),
        };
        cur = next;
    }

    Ok((cur, lhs))
}

// code generator
static REGISTORS: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];
static mut INDEX: usize = 0;

fn generate<'a>(node: Node) -> &'a str {
    if let Node::Number(val) = node {
        unsafe {
            if INDEX > 8 {
                panic!("registors exhausted")
            }
            let reg = REGISTORS[INDEX];
            INDEX += 1;
            println!("  mov {}, {}", reg, val);
            return reg;
        }
    };

    if let Node::Binary { op, lhs, rhs } = node {
        let dst = generate(*lhs);
        let src = generate(*rhs);

        if op == "+" {
            println!("  add {}, {}", dst, src);
            return dst;
        }
        if op == "-" {
            println!("  sub {}, {}", dst, src);
            return dst;
        }
    }
    panic!("unexpected node");
}

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
