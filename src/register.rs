use std::collections::HashMap;

pub type RegNo = usize;
pub type RegIndex = usize;

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
