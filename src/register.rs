use crate::codegen::ProgramEnvironment;
use crate::ir::IR;
use std::collections::HashMap;

pub type RegNo = usize;
pub type RegIndex = usize;

pub fn allocate_register(ir_vec: Vec<IR>, env: &mut ProgramEnvironment) -> Vec<IR> {
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
            IR::LABEL(label) => IR::LABEL(*label),
            IR::UNLESS { reg, label } => {
                let reg = env.reg_alloc.allocate(*reg);
                IR::UNLESS { reg, label: *label }
            }
        })
        .collect()
}

// RegAllocator: manage CPU registers
pub struct RegAllocator<'a> {
    registors: [(&'a str, bool); 8], // avaiable physical registers
    counter: RegNo,                  // virtual register number
    dict: HashMap<RegNo, RegIndex>, // mapping from virtual register number to physical register index
}

impl<'a> RegAllocator<'a> {
    pub fn new(count: usize) -> RegAllocator<'a> {
        RegAllocator {
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
            counter: count,
            dict: HashMap::new(),
        }
    }

    pub fn issue_regno(&mut self) -> RegNo {
        let regno = self.counter;
        self.counter += 1;
        return regno;
    }

    pub fn allocate(&mut self, regno: RegNo) -> RegIndex {
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

    pub fn get_register(&self, i: RegIndex) -> String {
        assert!(i < self.registors.len());
        return self.registors[i].0.to_string();
    }

    pub fn kill(&mut self, reg: RegNo) {
        let i: RegIndex = *self.dict.get(&reg).unwrap();
        assert!(self.registors[i].1);
        self.registors[i].1 = false;
    }
}
