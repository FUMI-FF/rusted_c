use crate::ast_parser::Node;
use std::collections::HashMap;

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

    fn get_register(&self, i: RegIndex) -> &str {
        assert!(i < self.registors.len());
        return self.registors[i].0;
    }
}

// code generator
static REGISTORS: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];
static mut INDEX: usize = 0;

pub fn generate<'a>(node: &Node) -> &'a str {
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
        let dst = generate(lhs);
        let src = generate(rhs);

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
