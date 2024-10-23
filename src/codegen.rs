use crate::ast_parser::Node;
use crate::register::VirtualRegister;
use crate::register::{RegIndex, RegNo};

// intermidiate representaion
enum IR {
    IMM { reg: RegNo, val: i32 },
    MOV { dst: RegNo, src: RegNo },
    ADD { dst: RegNo, src: RegNo },
    SUB { dst: RegNo, src: RegNo },
    RETURN(RegNo),
    KILL(RegNo),
    NOP,
}

// code generator
pub fn generate<'a>(node: &Node) {
    let mut virtual_register = VirtualRegister::new();
    let ir_vec = generate_ir(node, &mut virtual_register);
    let ir_vec = allocate_register(ir_vec, &mut virtual_register);
    gen_x86(ir_vec, &virtual_register);
}

// ir generate
fn generate_ir(node: &Node, virtual_register: &mut VirtualRegister) -> Vec<IR> {
    let mut ir_vec: Vec<IR> = vec![];
    let reg = _generate_ir(node, &mut ir_vec, virtual_register);
    ir_vec.push(IR::RETURN(reg));
    return ir_vec;
}

fn _generate_ir(
    node: &Node,
    ir_vec: &mut Vec<IR>,
    virtual_register: &mut VirtualRegister,
) -> RegNo {
    if let Node::Int(val) = node {
        let reg = virtual_register.issue_regno();
        ir_vec.push(IR::IMM { reg, val: *val });
        return reg;
    }
    if let Node::Binary { op, lhs, rhs } = node {
        let dst = _generate_ir(lhs, ir_vec, virtual_register);
        let src = _generate_ir(rhs, ir_vec, virtual_register);
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
    }

    panic!("node: {:?} is not supported", node);
}

fn allocate_register(ir_vec: Vec<IR>, virtual_register: &mut VirtualRegister) -> Vec<IR> {
    ir_vec
        .iter()
        .map(|ir| match ir {
            IR::IMM { reg, val } => {
                let reg: RegIndex = virtual_register.allocate(*reg);
                IR::IMM { reg, val: *val }
            }
            IR::MOV { dst, src } => {
                let dst: RegIndex = virtual_register.allocate(*dst);
                let src: RegIndex = virtual_register.allocate(*src);
                IR::MOV { dst, src }
            }
            IR::ADD { dst, src } => {
                let dst: RegIndex = virtual_register.allocate(*dst);
                let src: RegIndex = virtual_register.allocate(*src);
                IR::ADD { dst, src }
            }
            IR::SUB { dst, src } => {
                let dst: RegIndex = virtual_register.allocate(*dst);
                let src: RegIndex = virtual_register.allocate(*src);
                IR::SUB { dst, src }
            }
            IR::RETURN(reg) => {
                virtual_register.kill(*reg);
                IR::RETURN(*reg)
            }
            IR::KILL(reg) => {
                virtual_register.kill(*reg);
                IR::NOP
            }
            IR::NOP => IR::NOP,
        })
        .collect()
}

fn gen_x86(ir_vec: Vec<IR>, virtual_register: &VirtualRegister) {
    ir_vec.iter().for_each(|ir| match ir {
        IR::IMM { reg, val } => {
            let reg_str = virtual_register.get_register(*reg);
            println!("  mov {}, {}", reg_str, val); // use the variable here
        }
        IR::MOV { dst, src } => {
            let dst = virtual_register.get_register(*dst);
            let src = virtual_register.get_register(*src);
            println!("  mov {}, {}", dst, src);
        }
        IR::ADD { dst, src } => {
            let dst = virtual_register.get_register(*dst);
            let src = virtual_register.get_register(*src);
            println!("  add {}, {}", dst, src);
        }
        IR::SUB { dst, src } => {
            let dst = virtual_register.get_register(*dst);
            let src = virtual_register.get_register(*src);
            println!("  sub {}, {}", dst, src);
        }
        IR::RETURN(reg) => {
            let reg = virtual_register.get_register(*reg);
            println!("  mov rax, {}", reg);
            println!("  ret");
        }
        _ => {}
    })
}
