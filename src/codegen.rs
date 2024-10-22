use crate::ast_parser::Node;
use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};

// intermidiate representaion
type RegNo = usize;
type RegIndex = usize;

enum IR {
    IMM { reg: RegNo, val: i32 },
    MOV { dst: RegNo, src: RegNo },
    ADD { dst: RegNo, src: RegNo },
    SUB { dst: RegNo, src: RegNo },
    RETURN(RegNo),
    KILL(RegNo),
    NOP,
}

struct VirtualRegister<'a> {
    registors: [(&'a str, bool); 8],
    counter: RegNo,
    dict: HashMap<RegNo, RegIndex>,
}

impl<'a> VirtualRegister<'a> {
    fn new() -> VirtualRegister<'a> {
        VirtualRegister {
            registors: [
                ("rdi", false),
                ("rsi", false),
                ("r10", false),
                ("r11", false),
                ("r12", false),
                ("r13", false),
                ("r14", false),
                ("r15", false),
            ],
            counter: 0,
            dict: HashMap::new(),
        }
    }

    fn issue_regno(&mut self) -> RegNo {
        let regno = self.counter;
        self.counter += 1;
        return regno;
    }

    fn allocate(&mut self, regno: RegNo) -> RegIndex {
        match self.dict.get(&regno) {
            Some(val) => {
                assert!(self.registors[*val].1);
                return *val;
            }
            None => {
                for i in 0..self.registors.len() {
                    if self.registors[i].1 {
                        continue;
                    }
                    self.registors[i].1 = true;
                    self.dict.insert(regno, i);
                    return i;
                }
                panic!("registor exhausted");
            }
        }
    }

    fn get_register(&self, i: RegIndex) -> String {
        assert!(i < self.registors.len());
        return self.registors[i].0.to_string();
    }

    fn kill(&mut self, reg: RegNo) {
        let i: RegIndex = *self.dict.get(&reg).unwrap();
        assert!(self.registors[i].1);
        self.registors[i].1 = false;
    }
}

// code generator
pub fn generate<'a>(node: &Node) {
    let ir_vec = generate_ir(node);
    let ir_vec = allocate_register(ir_vec);
    gen_x86(ir_vec);
}

static VIRTUAL_REGISTER: LazyLock<Mutex<VirtualRegister<'static>>> =
    LazyLock::new(|| Mutex::new(VirtualRegister::new()));

fn _generate(node: &Node, ir_vec: &mut Vec<IR>) -> RegNo {
    if let Node::Int(val) = node {
        let reg = VIRTUAL_REGISTER.lock().unwrap().issue_regno();
        ir_vec.push(IR::IMM { reg, val: *val });
        return reg;
    }
    if let Node::Binary { op, lhs, rhs } = node {
        let dst = _generate(lhs, ir_vec);
        let src = _generate(rhs, ir_vec);
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

fn generate_ir(node: &Node) -> Vec<IR> {
    let mut ir_vec: Vec<IR> = vec![];
    let reg = _generate(node, &mut ir_vec);
    ir_vec.push(IR::RETURN(reg));
    return ir_vec;
}

fn allocate_register(ir_vec: Vec<IR>) -> Vec<IR> {
    ir_vec
        .iter()
        .map(|ir| match ir {
            IR::IMM { reg, val } => {
                let reg: RegIndex = VIRTUAL_REGISTER.lock().unwrap().allocate(*reg);
                IR::IMM { reg, val: *val }
            }
            IR::MOV { dst, src } => {
                let dst: RegIndex = VIRTUAL_REGISTER.lock().unwrap().allocate(*dst);
                let src: RegIndex = VIRTUAL_REGISTER.lock().unwrap().allocate(*src);
                IR::MOV { dst, src }
            }
            IR::ADD { dst, src } => {
                let dst: RegIndex = VIRTUAL_REGISTER.lock().unwrap().allocate(*dst);
                let src: RegIndex = VIRTUAL_REGISTER.lock().unwrap().allocate(*src);
                IR::ADD { dst, src }
            }
            IR::SUB { dst, src } => {
                let dst: RegIndex = VIRTUAL_REGISTER.lock().unwrap().allocate(*dst);
                let src: RegIndex = VIRTUAL_REGISTER.lock().unwrap().allocate(*src);
                IR::SUB { dst, src }
            }
            IR::RETURN(reg) => {
                VIRTUAL_REGISTER.lock().unwrap().kill(*reg);
                IR::RETURN(*reg)
            }
            IR::KILL(reg) => {
                VIRTUAL_REGISTER.lock().unwrap().kill(*reg);
                IR::NOP
            }
            IR::NOP => IR::NOP,
        })
        .collect()
}

fn gen_x86(ir_vec: Vec<IR>) {
    ir_vec.iter().for_each(|ir| match ir {
        IR::IMM { reg, val } => {
            let reg_str = VIRTUAL_REGISTER.lock().unwrap().get_register(*reg);
            println!("  mov {}, {}", reg_str, val); // use the variable here
        }
        IR::MOV { dst, src } => {
            let dst = VIRTUAL_REGISTER.lock().unwrap().get_register(*dst);
            let src = VIRTUAL_REGISTER.lock().unwrap().get_register(*src);
            println!("  mov {}, {}", dst, src);
        }
        IR::ADD { dst, src } => {
            let dst = VIRTUAL_REGISTER.lock().unwrap().get_register(*dst);
            let src = VIRTUAL_REGISTER.lock().unwrap().get_register(*src);
            println!("  add {}, {}", dst, src);
        }
        IR::SUB { dst, src } => {
            let dst = VIRTUAL_REGISTER.lock().unwrap().get_register(*dst);
            let src = VIRTUAL_REGISTER.lock().unwrap().get_register(*src);
            println!("  sub {}, {}", dst, src);
        }
        IR::RETURN(reg) => {
            let reg = VIRTUAL_REGISTER.lock().unwrap().get_register(*reg);
            println!("  mov rax, {}", reg);
            println!("  ret");
        }
        _ => {}
    })
}
