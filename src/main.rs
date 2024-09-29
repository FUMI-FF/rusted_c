use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input: i32 = args[1].parse::<i32>().unwrap();

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    println!("  mov rax, {}", input);
    println!("  ret");
}
