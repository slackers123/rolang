use std::{collections::HashMap, fmt::Display};

use instructions::{Instruction, Source, Target};
use interpreter::Interpreter;

use crate::ty::Literal;

pub mod generator;
pub mod instructions;
pub mod interpreter;

pub struct BcFunction {
    instructions: Vec<Instruction>,
    consts: Vec<BcValue>,
}

pub struct Registers([BcValue; 16]);

impl Default for Registers {
    fn default() -> Self {
        unsafe {
            let mut this: Self = std::mem::zeroed();
            for item in this.0.iter_mut() {
                std::ptr::write(item, BcValue::default());
            }
            return this;
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum BcValue {
    String(String),
    Number(f64),
    Bool(bool),
    #[default]
    Undefined,
}

impl BcValue {
    pub fn from_literal(literal: Literal) -> Self {
        match literal {
            Literal::String(s) => Self::String(s),
            Literal::Number(n) => Self::Number(n),
        }
    }
}

impl std::cmp::PartialOrd for BcValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self {
            Self::Number(n) => match other {
                Self::Number(n1) => n.partial_cmp(n1),
                _ => None,
            },
            _ => None,
        }
    }
}

impl std::ops::Sub for BcValue {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Self::Number(n1) => match rhs {
                Self::Number(n2) => Self::Number(n1 - n2),
                _ => panic!("it is not possible to subtract non numbers"),
            },
            _ => panic!("it is not possible to subtract non numbers"),
        }
    }
}

impl std::ops::Add for BcValue {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Self::Number(n1) => match rhs {
                Self::Number(n2) => Self::Number(n1 + n2),
                _ => panic!("it is not possible to add non numbers yet"),
            },
            Self::String(s1) => match rhs {
                Self::Bool(b2) => Self::String(format!("{s1}{b2}")),
                Self::String(s2) => Self::String(format!("{s1}{s2}")),
                Self::Number(n2) => Self::String(format!("{s1}{n2}")),
                Self::Undefined => Self::String(format!("{s1}undefined")),
            },
            _ => panic!("it is only possible to add to numbers or strings"),
        }
    }
}

impl Display for BcValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "{s}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Undefined => write!(f, "undefined"),
        }
    }
}

pub fn run_test() {
    let main_consts = vec![BcValue::Number(30.0)];
    let main_instructions = vec![
        Instruction::Mov {
            target: Target::CallRegister(0),
            source: Source::Constant(0),
        },
        Instruction::Call {
            target: Some(Target::CallRegister(0)),
            function: "fib".into(),
        },
        Instruction::Call {
            target: None,
            function: "println".into(),
        },
    ];

    let main_function = BcFunction {
        instructions: main_instructions,
        consts: main_consts,
    };

    let fib_consts = vec![BcValue::Number(1.0), BcValue::Number(2.0)];
    let fib_instructions = vec![
        Instruction::Mov {
            target: Target::Register(1),
            source: Source::Constant(0),
        },
        Instruction::Cmp {
            lhs: Source::Register(0),
            rhs: Source::Register(1),
        },
        Instruction::Jgt { goal: 2 },
        Instruction::Return {
            val: Source::Register(0),
        },
        Instruction::Sub {
            target: Target::CallRegister(0),
            lhs: Source::Register(0),
            rhs: Source::Constant(0),
        },
        Instruction::Call {
            target: Some(Target::Register(2)),
            function: "fib".into(),
        },
        Instruction::Sub {
            target: Target::CallRegister(0),
            lhs: Source::Register(0),
            rhs: Source::Constant(1),
        },
        Instruction::Call {
            target: Some(Target::Register(3)),
            function: "fib".into(),
        },
        Instruction::Add {
            target: Target::Register(0),
            lhs: Source::Register(2),
            rhs: Source::Register(3),
        },
        Instruction::Return {
            val: Source::Register(0),
        },
    ];

    let fib_function = BcFunction {
        instructions: fib_instructions,
        consts: fib_consts,
    };

    let mut int = Interpreter {
        functions: &HashMap::from([("main".into(), main_function), ("fib".into(), fib_function)]),
    };

    int.run("main");
}
