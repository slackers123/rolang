use std::fmt::Debug;

#[derive(Debug, Clone)]
pub enum BinOp {
    Eq,
    Neq,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Lte,
    Gt,
    Gte,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
}

pub enum AOrB<A, B> {
    A(A),
    B(B),
}

// impl<A: Debug, B: Debug> AOrB<A, B> {
//     pub fn get_a(self) -> A {
//         match self {
//             Self::A(a) => a,
//             Self::B(b) => panic!("expected A but found B: {b:?}"),
//         }
//     }

//     pub fn get_b(self) -> B {
//         match self {
//             Self::A(a) => panic!("expected B but found A: {a:?}"),
//             Self::B(b) => b,
//         }
//     }
// }
