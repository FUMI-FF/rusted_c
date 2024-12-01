use std::collections::HashMap;

use crate::ast_parser::Node;
use crate::register::RegAllocator;
use crate::register::{RegIndex, RegNo};

// intermidiate representaion
#[derive(Debug)]
enum IR {
    IMM { reg: RegNo, val: i32 },
    MOV { dst: RegNo, src: RegNo },
    ADD { dst: RegNo, src: RegNo },
    AddImm { dst: RegNo, val: i32 },
    SUB { dst: RegNo, src: RegNo },
    MUL { dst: RegNo, src: RegNo },
    DIV { dst: RegNo, src: RegNo },
    LOAD { reg: RegNo, addr: RegNo },
    STORE { addr: RegNo, reg: RegNo },
    ALLOCA { basereg: RegNo, offset: i32 },
    RETURN(RegNo),
    KILL(RegNo),
    NOP,
}

struct ProgramEnvironment<'a> {
    reg_alloc: RegAllocator<'a>,
    vars: HashMap<String, usize>, // variables
    bpoff: usize,                 // base pointer offset
    basereg: usize,               // base register address
    label_num: usize,             // label number
}

impl ProgramEnvironment<'_> {
    fn new() -> Self {
        Self {
            reg_alloc: RegAllocator::new(1),
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

// ir generate
fn gen_ir(node: &Node, env: &mut ProgramEnvironment) -> Vec<IR> {
    match node {
        Node::CompoundStmt(_) => {
            let mut stmt_vec = gen_stmt(node, env);
            let mut vec = vec![IR::ALLOCA {
                basereg: env.basereg,
                offset: env.bpoff as i32,
            }];
            vec.append(&mut stmt_vec);
            vec
        }
        _ => panic!("The node must be CompoundStmt, {:?}", node),
    }
}

fn gen_stmt(node: &Node, env: &mut ProgramEnvironment) -> Vec<IR> {
    match node {
        Node::Return(expr) => {
            let (reg, mut vec) = gen_expr(expr, env);
            vec.push(IR::RETURN(reg));
            vec.push(IR::KILL(reg));
            vec
        }
        Node::ExprStmt(expr) => {
            let (reg, mut vec) = gen_expr(expr, env);
            vec.push(IR::KILL(reg));
            vec
        }
        Node::CompoundStmt(stmts) => {
            let mut vec = Vec::new();
            for stmt in stmts {
                let mut v = gen_stmt(stmt, env);
                vec.append(&mut v);
            }
            vec
        }
        _ => panic!("unexpected node: {:?}", node),
    }
}

fn gen_expr(node: &Node, env: &mut ProgramEnvironment) -> (RegNo, Vec<IR>) {
    if let Node::Int(val) = node {
        let reg = env.reg_alloc.issue_regno();
        return (reg, vec![IR::IMM { reg, val: *val }]);
    }
    if let Node::Ident(_) = node {
        let (reg, mut vec) = gen_lval(node, env);
        vec.push(IR::LOAD { reg, addr: reg });
        return (reg, vec);
    }
    if let Node::Binary { op, lhs, rhs } = node {
        if op == "+" {
            let (dst, mut v1) = gen_expr(lhs, env);
            let (src, mut v2) = gen_expr(rhs, env);
            v1.append(&mut v2);
            v1.push(IR::ADD { dst, src });
            v1.push(IR::KILL(src));
            return (dst, v1);
        }
        if op == "-" {
            let (dst, mut v1) = gen_expr(lhs, env);
            let (src, mut v2) = gen_expr(rhs, env);
            v1.append(&mut v2);
            v1.push(IR::SUB { dst, src });
            v1.push(IR::KILL(src));
            return (dst, v1);
        }
        if op == "*" {
            let (dst, mut v1) = gen_expr(lhs, env);
            let (src, mut v2) = gen_expr(rhs, env);
            v1.append(&mut v2);
            v1.push(IR::MUL { dst, src });
            v1.push(IR::KILL(src));
            return (dst, v1);
        }
        if op == "/" {
            let (dst, mut v1) = gen_expr(lhs, env);
            let (src, mut v2) = gen_expr(rhs, env);
            v1.append(&mut v2);
            v1.push(IR::DIV { dst, src });
            v1.push(IR::KILL(src));
            return (dst, v1);
        }
        if op == "=" {
            let (r1, mut v1) = gen_expr(rhs, env);
            let (r2, mut v2) = gen_lval(lhs, env);
            v1.append(&mut v2);
            v1.push(IR::STORE { addr: r2, reg: r1 });
            v1.push(IR::KILL(r1));
            return (r2, v1);
        }
    }

    panic!("node: {:?} is not supported", node);
}

fn gen_lval(node: &Node, env: &mut ProgramEnvironment) -> (RegNo, Vec<IR>) {
    let name = match node {
        Node::Ident(name) => name,
        _ => panic!("node must be ident but got {:?}", node),
    };
    if !env.vars.contains_key(name) {
        env.vars.insert(name.to_string(), env.bpoff);
        env.bpoff += 8;
    }
    let r = env.reg_alloc.issue_regno();
    let offset = env
        .vars
        .get(name)
        .expect(format!("var {} not exists", name).as_str());

    // load variable address into r1
    (
        r,
        vec![
            IR::MOV {
                dst: r,
                src: env.basereg,
            },
            IR::AddImm {
                dst: r,
                val: *offset as i32,
            },
        ],
    )
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
