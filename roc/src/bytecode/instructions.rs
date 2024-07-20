use crate::ty::BinOp;

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
    Mod {
        target: Target,
        lhs: Source,
        rhs: Source,
    },
    Cmp {
        lhs: Source,
        rhs: Source,
    },
    LdCmp {
        target: Target,
        ty: LdCmpType,
    },
    Jne {
        goal: RelBcIndex,
    },
    Je {
        goal: RelBcIndex,
    },
    Jlt {
        goal: RelBcIndex,
    },
    Jlte {
        goal: RelBcIndex,
    },
    Jgt {
        goal: RelBcIndex,
    },
    Jgte {
        goal: RelBcIndex,
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

pub type RelBcIndex = i64;

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

#[derive(Debug)]
pub enum LdCmpType {
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
}

impl LdCmpType {
    pub fn from_binop(binop: BinOp) -> Self {
        match binop {
            BinOp::Eq => Self::Eq,
            BinOp::Neq => Self::Neq,
            BinOp::Gt => Self::Gt,
            BinOp::Gte => Self::Gte,
            BinOp::Lt => Self::Lt,
            BinOp::Lte => Self::Lte,
            _ => panic!("invalid binop for compare type: {:?}", binop),
        }
    }
}
