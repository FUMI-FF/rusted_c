use crate::ast_parser::Node;
use crate::register::RegAllocator;
use crate::register::{RegIndex, RegNo};

// intermidiate representaion
enum IR {
    IMM { reg: RegNo, val: i32 },
    MOV { dst: RegNo, src: RegNo },
    ADD { dst: RegNo, src: RegNo },
    SUB { dst: RegNo, src: RegNo },
    MUL { dst: RegNo, src: RegNo },
    DIV { dst: RegNo, src: RegNo },
    RETURN(RegNo),
    KILL(RegNo),
    NOP,
}

struct ProgramEnvironment<'a> {
    reg_alloc: RegAllocator<'a>,
}

impl ProgramEnvironment<'_> {
    fn new() -> Self {
        Self {
            reg_alloc: RegAllocator::new(),
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
    let mut ir_vec: Vec<IR> = vec![];
    match node {
        Node::CompoundStmt(_) => gen_stmt(node, &mut ir_vec, env),
        _ => panic!("The node must be CompoundStmt, {:?}", node),
    }
    return ir_vec;
}

fn gen_stmt(node: &Node, ir_vec: &mut Vec<IR>, env: &mut ProgramEnvironment) {
    match node {
        Node::Return(expr) => {
            let reg = gen_expr(expr, ir_vec, env);
            ir_vec.push(IR::RETURN(reg));
            ir_vec.push(IR::KILL(reg));
        }
        Node::ExprStmt(expr) => {
            let reg = gen_expr(expr, ir_vec, env);
            ir_vec.push(IR::KILL(reg));
        }
        Node::CompoundStmt(stmts) => {
            for stmt in stmts {
                gen_stmt(stmt, ir_vec, env)
            }
        }
        _ => panic!("unexpected node: {:?}", node),
    }
}

fn gen_expr(node: &Node, ir_vec: &mut Vec<IR>, env: &mut ProgramEnvironment) -> RegNo {
    if let Node::Int(val) = node {
        let reg = env.reg_alloc.issue_regno();
        ir_vec.push(IR::IMM { reg, val: *val });
        return reg;
    }
    if let Node::Binary { op, lhs, rhs } = node {
        let dst = gen_expr(lhs, ir_vec, env);
        let src = gen_expr(rhs, ir_vec, env);
        if op == "+" {
            ir_vec.push(IR::ADD { dst, src });
            ir_vec.push(IR::KILL(src));
            return dst;
        }
        if op == "-" {
            ir_vec.push(IR::SUB { dst, src });
            ir_vec.push(IR::KILL(src));
            return dst;
        }
        if op == "*" {
            ir_vec.push(IR::MUL { dst, src });
            ir_vec.push(IR::KILL(src));
            return dst;
        }
        if op == "/" {
            ir_vec.push(IR::DIV { dst, src });
            ir_vec.push(IR::KILL(src));
            return dst;
        }
    }

    panic!("node: {:?} is not supported", node);
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

fn gen_x86(ir_vec: Vec<IR>, env: &mut ProgramEnvironment) {
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
            println!("  ret");
        }
        _ => {}
    })
}
