use crate::ir::IR;
use crate::register::RegAllocator;
use std::collections::HashMap;

pub struct ProgramEnvironment<'a> {
    pub reg_alloc: RegAllocator<'a>,
    pub vars: HashMap<String, usize>, // variables
    pub bpoff: usize,                 // base pointer offset
    pub basereg: usize,               // base register address
    pub label_num: usize,             // label number
}

impl ProgramEnvironment<'_> {
    pub fn new() -> Self {
        Self {
            reg_alloc: RegAllocator::new(1), // HACK: first register is used for basereg storing
            vars: HashMap::new(),
            bpoff: 0,
            basereg: 0,
            label_num: 0,
        }
    }

    pub fn gen_label(&mut self) -> usize {
        let label = self.label_num;
        self.label_num += 1;
        return label;
    }
}

// code generator
pub fn gen_x86(ir_vec: Vec<IR>, env: &mut ProgramEnvironment) {
    let label = ".Lend";

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
        IR::LABEL(label) => {
            println!(".L{}:", label);
        }
        IR::UNLESS { reg, label } => {
            let reg = env.reg_alloc.get_register(*reg);
            println!("  cmp {}, 0", reg);
            println!("  je .L{}", label);
        }
        IR::JMP(label) => {
            println!("  jmp .L{}", label);
        }
        _ => {}
    });

    println!("{}:", label);
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}
