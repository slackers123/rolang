use super::interpreter::{ConstId, FunctionIndex, RegId};

#[derive(Debug)]
pub enum Instruction {
    Mov {
        target: Target,
        source: Source,
    },
    Add {
        target: Target,
        lhs: Source,
        rhs: Source,
    },
    Sub {
        target: Target,
        lhs: Source,
        rhs: Source,
    },
    Mul {
        target: Target,
        lhs: Source,
        rhs: Source,
    },
    Div {
        target: Target,
        lhs: Source,
        rhs: Source,
    },
    Cmp {
        lhs: Source,
        rhs: Source,
    },
    Jne {
        goal: BytecodeIndex,
    },
    Je {
        goal: BytecodeIndex,
    },
    Jlt {
        goal: BytecodeIndex,
    },
    Jlte {
        goal: BytecodeIndex,
    },
    Jgt {
        goal: BytecodeIndex,
    },
    Jgte {
        goal: BytecodeIndex,
    },
    Call {
        target: Option<Target>,
        function: FunctionIndex,
    },
    Return {
        val: Source,
    },
}

pub type BytecodeIndex = u64;
#[derive(Debug)]
pub enum Target {
    Register(RegId),
    CallRegister(RegId),
}
#[derive(Debug)]
pub enum Source {
    Register(RegId),
    Constant(ConstId),
}
