use crate::ast_parser::Node;
use crate::ir::{gen_ir, IR};
use crate::register::RegAllocator;
use crate::register::RegIndex;
use std::collections::HashMap;

pub struct ProgramEnvironment<'a> {
    pub reg_alloc: RegAllocator<'a>,
    pub vars: HashMap<String, usize>, // variables
    pub bpoff: usize,                 // base pointer offset
    pub basereg: usize,               // base register address
    pub label_num: usize,             // label number
}

impl ProgramEnvironment<'_> {
    fn new() -> Self {
        Self {
            reg_alloc: RegAllocator::new(1), // HACK: first register is used for basereg storing
            vars: HashMap::new(),
            bpoff: 0,
            basereg: 0,
            label_num: 0,
        }
    }
}

// code generator
pub fn generate<'a>(node: &Node) {
    let mut env = ProgramEnvironment::new();
    let ir_vec = gen_ir(node, &mut env);
    let ir_vec = allocate_register(ir_vec, &mut env);
    gen_x86(ir_vec, &mut env);
}

fn allocate_register(ir_vec: Vec<IR>, env: &mut ProgramEnvironment) -> Vec<IR> {
    ir_vec
        .iter()
        .map(|ir| match ir {
            IR::IMM { reg, val } => {
                let reg: RegIndex = env.reg_alloc.allocate(*reg);
                IR::IMM { reg, val: *val }
            }
            IR::MOV { dst, src } => {
                let dst: RegIndex = env.reg_alloc.allocate(*dst);
                let src: RegIndex = env.reg_alloc.allocate(*src);
                IR::MOV { dst, src }
            }
            IR::ADD { dst, src } => {
                let dst: RegIndex = env.reg_alloc.allocate(*dst);
                let src: RegIndex = env.reg_alloc.allocate(*src);
                IR::ADD { dst, src }
            }
            IR::AddImm { dst, val } => {
                let dst: RegIndex = env.reg_alloc.allocate(*dst);
                IR::AddImm { dst, val: *val }
            }
            IR::SUB { dst, src } => {
                let dst: RegIndex = env.reg_alloc.allocate(*dst);
                let src: RegIndex = env.reg_alloc.allocate(*src);
                IR::SUB { dst, src }
            }
            IR::MUL { dst, src } => {
                let dst: RegIndex = env.reg_alloc.allocate(*dst);
                let src: RegIndex = env.reg_alloc.allocate(*src);
                IR::MUL { dst, src }
            }
            IR::DIV { dst, src } => {
                let dst: RegIndex = env.reg_alloc.allocate(*dst);
                let src: RegIndex = env.reg_alloc.allocate(*src);
                IR::DIV { dst, src }
            }
            IR::LOAD { reg, addr } => {
                let reg: RegIndex = env.reg_alloc.allocate(*reg);
                let addr: RegIndex = env.reg_alloc.allocate(*addr);
                IR::LOAD { reg, addr }
            }
            IR::STORE { addr, reg } => {
                let reg: RegIndex = env.reg_alloc.allocate(*reg);
                let addr: RegIndex = env.reg_alloc.allocate(*addr);
                IR::STORE { addr, reg }
            }
            IR::ALLOCA { basereg, offset } => {
                let basereg: RegIndex = env.reg_alloc.allocate(*basereg);
                IR::ALLOCA {
                    basereg,
                    offset: *offset,
                }
            }
            IR::RETURN(reg) => {
                let reg = env.reg_alloc.allocate(*reg);
                IR::RETURN(reg)
            }
            IR::KILL(reg) => {
                env.reg_alloc.kill(*reg);
                IR::NOP
            }
            IR::NOP => IR::NOP,
        })
        .collect()
}

fn gen_label(env: &mut ProgramEnvironment) -> String {
    let label = format!(".L{}", env.label_num);
    env.label_num += 1;
    return label;
}

fn gen_x86(ir_vec: Vec<IR>, env: &mut ProgramEnvironment) {
    let label = gen_label(env);

    println!("  push rbp");
    println!("  mov rbp, rsp");

    ir_vec.iter().for_each(|ir| match ir {
        IR::IMM { reg, val } => {
            let reg_str = env.reg_alloc.get_register(*reg);
            println!("  mov {}, {}", reg_str, val); // use the variable here
        }
        IR::MOV { dst, src } => {
            let dst = env.reg_alloc.get_register(*dst);
            let src = env.reg_alloc.get_register(*src);
            println!("  mov {}, {}", dst, src);
        }
        IR::ADD { dst, src } => {
            let dst = env.reg_alloc.get_register(*dst);
            let src = env.reg_alloc.get_register(*src);
            println!("  add {}, {}", dst, src);
        }
        IR::AddImm { dst, val } => {
            let dst = env.reg_alloc.get_register(*dst);
            println!("  add {}, {}", dst, val)
        }
        IR::SUB { dst, src } => {
            let dst = env.reg_alloc.get_register(*dst);
            let src = env.reg_alloc.get_register(*src);
            println!("  sub {}, {}", dst, src);
        }
        IR::MUL { dst, src } => {
            let dst = env.reg_alloc.get_register(*dst);
            let src = env.reg_alloc.get_register(*src);
            println!("  mov rax, {}", src);
            println!("  mul {}", dst);
            println!("  mov {}, rax", dst);
        }
        IR::DIV { dst, src } => {
            let dst = env.reg_alloc.get_register(*dst);
            let src = env.reg_alloc.get_register(*src);
            println!("  mov rax, {}", dst);
            println!("  cqo");
            println!("  div {}", src);
            println!("  mov {}, rax", dst);
        }
        IR::RETURN(reg) => {
            let reg = env.reg_alloc.get_register(*reg);
            println!("  mov rax, {}", reg);
            println!("  jmp {}", label);
        }
        IR::ALLOCA { basereg, offset } => {
            let basereg = env.reg_alloc.get_register(*basereg);
            println!("  sub rsp, {}", offset);
            println!("  mov {}, rsp", basereg);
        }
        IR::LOAD { reg, addr } => {
            let reg = env.reg_alloc.get_register(*reg);
            let addr = env.reg_alloc.get_register(*addr);
            println!("  mov {}, [{}]", reg, addr);
        }
        IR::STORE { addr, reg } => {
            let addr = env.reg_alloc.get_register(*addr);
            let reg = env.reg_alloc.get_register(*reg);
            println!("  mov [{}], {}", addr, reg);
        }
        _ => {}
    });

    println!("{}:", label);
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}
