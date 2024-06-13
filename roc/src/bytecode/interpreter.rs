use std::collections::HashMap;

use super::{
    instructions::{Instruction, Source, Target},
    BcFunction, BcValue, Registers,
};

pub struct BcFlags {
    lt: bool,
    gt: bool,
    eq: bool,
}

impl BcFlags {
    pub fn none() -> Self {
        Self {
            lt: false,
            gt: false,
            eq: false,
        }
    }

    pub fn compute(&mut self, lhs: BcValue, rhs: BcValue) {
        self.lt = lhs < rhs;
        self.gt = lhs > rhs;
        self.eq = lhs == rhs;
    }
}

pub type RegId = u8;
pub type ConstId = u32;

pub enum Imm {
    Float(f64),
    Int(i64),
    Char(char),
}

pub type FunctionIndex = String;

pub struct RunningFunction<'a> {
    base: &'a BcFunction,
    regs: &'a mut Registers,
    call_regs: Registers,
    flags: BcFlags,
}
impl<'a> RunningFunction<'a> {
    pub fn from_func(base: &'a BcFunction, regs: &'a mut Registers) -> Self {
        Self {
            base,
            regs,
            call_regs: Default::default(),
            flags: BcFlags::none(),
        }
    }

    pub fn write_target(&mut self, target: &Target, val: BcValue) {
        match target {
            Target::Register(reg) => self.regs.0[*reg as usize] = val,
            Target::CallRegister(reg) => self.call_regs.0[*reg as usize] = val,
        }
    }

    pub fn get_source(&self, source: &Source) -> BcValue {
        match source {
            Source::Register(reg) => self.regs.0[*reg as usize].clone(),
            Source::Constant(cst) => self.base.consts[*cst as usize].clone(),
        }
    }
}

pub struct Interpreter<'a> {
    pub functions: &'a HashMap<FunctionIndex, BcFunction>,
}

impl<'a> Interpreter<'a> {
    pub fn run(&mut self, name: &str) -> Option<BcValue> {
        let mut regs = Registers::default();
        self.run_fn(name.into(), &mut regs);
        return None;
    }
    pub fn run_fn(&mut self, name: FunctionIndex, regs: &mut Registers) -> Option<BcValue> {
        if name == "println" {
            println!("{}", regs.0[0]);
            return None;
        }

        let func = self.functions.get(&name).unwrap();

        let mut running = RunningFunction::from_func(&func, regs);

        let mut idx = 0;

        loop {
            let inst = running.base.instructions.get(idx);
            if let Some(inst) = inst {
                idx += 1;
                match inst {
                    Instruction::Mov { target, source } => {
                        // println!(
                        //     "moving {source:?}({:?}) to {target:?}",
                        //     running.get_source(source),
                        // );
                        running.write_target(target, running.get_source(source));
                    }
                    Instruction::Call { target, function } => {
                        // println!("calling {function:?} and storing in {target:?}");
                        let res = self.run_fn(function.clone(), &mut running.call_regs);
                        if res.is_some() && target.is_some() {
                            running.write_target(target.as_ref().unwrap(), res.unwrap());
                        }
                    }
                    Instruction::Cmp { lhs, rhs } => {
                        // println!(
                        //     "comparing {lhs:?}({:?}) to {rhs:?}({:?})",
                        //     running.get_source(lhs),
                        //     running.get_source(rhs)
                        // );
                        running
                            .flags
                            .compute(running.get_source(lhs), running.get_source(rhs));
                    }
                    Instruction::Jgt { goal } => {
                        // println!("if {} then jump to {goal}", running.flags.gt);
                        if running.flags.gt {
                            idx = *goal as usize;
                        }
                    }
                    Instruction::Sub { target, lhs, rhs } => {
                        // println!(
                        //     "subtracting {rhs:?}({:?}) from {lhs:?}({:?}) into {target:?}",
                        //     running.get_source(rhs),
                        //     running.get_source(lhs)
                        // );
                        running.write_target(
                            target,
                            running.get_source(lhs) - running.get_source(rhs),
                        );
                    }
                    Instruction::Add { target, lhs, rhs } => {
                        // println!(
                        //     "adding {lhs:?}({:?}) and {rhs:?}({:?}) into {target:?}",
                        //     running.get_source(lhs),
                        //     running.get_source(rhs)
                        // );
                        running
                            .write_target(target, running.get_source(lhs) + running.get_source(rhs))
                    }
                    Instruction::Return { val } => {
                        // println!("returning {val:?}({:?})", running.get_source(val));
                        return Some(running.get_source(val));
                    }
                    _ => unimplemented!("{inst:?}"),
                }
            } else {
                break;
            }
        }
        None
    }
}
