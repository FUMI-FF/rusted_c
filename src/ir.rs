use crate::ast_parser::Node;
use crate::codegen::ProgramEnvironment;
use crate::register::RegNo;

// intermidiate representaion
#[derive(Debug)]
pub enum IR {
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

// ir generate
pub fn gen_ir(node: &Node, env: &mut ProgramEnvironment) -> Vec<IR> {
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
