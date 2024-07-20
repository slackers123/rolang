use pest::iterators::{Pair, Pairs};
use thiserror::Error;

use crate::ty::AOrB;
use crate::{
    parser::Rule,
    ty::{BinOp, Literal},
};

trait FromPair {
    fn from_pair(pair: Pair<'_, Rule>) -> Self;
}

#[derive(Debug, Clone, Error)]
pub enum ASTError {}

pub type ASTResult<T> = Result<T, ASTError>;

pub fn gen_ast(parsed: Pairs<'_, crate::parser::Rule>) -> ASTResult<Ast> {
    let file = parsed.into_iter().next().unwrap();

    let mut ast = Ast {
        outer_stmts: vec![],
    };

    for outerstmt in file.into_inner() {
        match outerstmt.as_rule() {
            Rule::OuterStmt => ast.outer_stmts.push(ASTOuterStmt::from_pair(outerstmt)),
            Rule::EOI => {}
            _ => unreachable!(),
        }
    }
    Ok(ast)
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub outer_stmts: Vec<ASTOuterStmt>,
}

#[derive(Debug, Clone)]
pub enum ASTOuterStmt {
    Entry(ASTEntry),
    Use(ASTUse),
    FnDef(ASTFnDef),
}

impl FromPair for ASTOuterStmt {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let first = pair.into_inner().next().unwrap();
        match first.as_rule() {
            Rule::Entry => Self::Entry(ASTEntry::from_pair(first)),
            Rule::Use => Self::Use(ASTUse::from_pair(first)),
            Rule::FnDef => Self::FnDef(ASTFnDef::from_pair(first)),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTEntry {
    pub fn_name: String,
}

impl FromPair for ASTEntry {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let inner = pair.into_inner().next().unwrap();
        let fn_name = string_from_ident(inner);
        Self { fn_name }
    }
}

#[derive(Debug, Clone)]
pub struct ASTUse {
    pub path: ASTTypePath,
}

impl FromPair for ASTUse {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let inner = pair.into_inner().next().unwrap();
        assert_eq!(inner.as_rule(), Rule::ModPathOrTypePath);
        let inner = inner.into_inner().next().unwrap();
        assert_eq!(inner.as_rule(), Rule::TypePath);
        let path = ASTTypePath::from_pair(inner);
        Self { path }
    }
}

#[derive(Debug, Clone)]
pub struct ASTFnDef {
    pub fn_name: String,
    pub args: Vec<ASTTypedVar>,
    pub ret_type: Option<ASTTypePath>,
    pub block: ASTBlock,
}

impl FromPair for ASTFnDef {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let mut inner = pair.into_inner();
        let fn_name = string_from_ident(inner.next().unwrap());

        let next = inner.next().unwrap();

        match next.as_rule() {
            Rule::TypedVarList => {
                let args = get_list(next);
                let next = inner.next().unwrap();
                match next.as_rule() {
                    Rule::TypePath => {
                        let ret_type = ASTTypePath::from_pair(next);
                        let block = ASTBlock::from_pair(inner.next().unwrap());
                        Self {
                            fn_name,
                            args,
                            ret_type: Some(ret_type),
                            block,
                        }
                    }
                    Rule::Block => {
                        let block = ASTBlock::from_pair(next);
                        Self {
                            fn_name,
                            args,
                            ret_type: None,
                            block,
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Rule::TypePath => {
                let ret_type = ASTTypePath::from_pair(next);
                let block = ASTBlock::from_pair(inner.next().unwrap());
                Self {
                    fn_name,
                    args: vec![],
                    ret_type: Some(ret_type),
                    block,
                }
            }
            Rule::Block => {
                let block = ASTBlock::from_pair(next);
                Self {
                    fn_name,
                    args: vec![],
                    ret_type: None,
                    block,
                }
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTTypedVar {
    pub var: String,
    pub ty: ASTTypePath,
}

impl FromPair for ASTTypedVar {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let mut inner = pair.into_inner();
        let var = string_from_ident(inner.next().unwrap());
        let ty = ASTTypePath::from_pair(inner.next().unwrap());
        Self { var, ty }
    }
}

#[derive(Debug, Clone)]
pub struct ASTTypePath {
    pub path: Vec<String>,
}

impl FromPair for ASTTypePath {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let path = pair.into_inner().map(|i| string_from_ident(i)).collect();
        Self { path }
    }
}

#[derive(Debug, Clone)]
pub struct ASTBlock {
    pub stmts: Vec<ASTBlockStmt>,
}

impl FromPair for ASTBlock {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let stmts = get_list(pair);
        Self { stmts }
    }
}

#[derive(Debug, Clone)]
pub enum ASTBlockStmt {
    VarDef(ASTVarDef),
    FnCall(ASTFnCall),
    VarAssign(ASTVarAssign),
    IfStmt(ASTIfStmt),
    RetStmt(ASTRetStmt),
}

impl FromPair for ASTBlockStmt {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::VarDef => Self::VarDef(ASTVarDef::from_pair(inner)),
            Rule::FnCall => Self::FnCall(ASTFnCall::from_pair(inner)),
            Rule::VarAssign => Self::VarAssign(ASTVarAssign::from_pair(inner)),
            Rule::IfStmt => Self::IfStmt(ASTIfStmt::from_pair(inner)),
            Rule::RetStmt => Self::RetStmt(ASTRetStmt::from_pair(inner)),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTRetStmt {
    pub expr: ASTExpr,
}

impl FromPair for ASTRetStmt {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let expr = pair.into_inner().next().unwrap();
        Self {
            expr: ASTExpr::from_pair(expr),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTIfStmt {
    pub expr: ASTExpr,
    pub block: ASTBlock,
}

impl FromPair for ASTIfStmt {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let mut inner = pair.into_inner();
        let expr = ASTExpr::from_pair(inner.next().unwrap());
        let block = ASTBlock::from_pair(inner.next().unwrap());
        // TODO: else and so on
        Self { expr, block }
    }
}

#[derive(Debug, Clone)]
pub struct ASTVarDef {
    pub name: String,
    pub ty: Option<ASTTypePath>,
    pub assign: Option<ASTExpr>,
}

impl FromPair for ASTVarDef {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let mut inner = pair.into_inner();
        let next = inner.next().unwrap();
        let name;
        let mut ty = None;
        match next.as_rule() {
            Rule::Ident => {
                name = string_from_ident(next);
            }
            Rule::TypedVar => {
                let test = ASTTypedVar::from_pair(next);
                name = test.var;
                ty = Some(test.ty);
            }
            _ => unimplemented!(),
        }
        if let Some(next) = inner.next() {
            let assign = Some(ASTExpr::from_pair(next));
            Self { name, ty, assign }
        } else {
            Self {
                name,
                ty,
                assign: None,
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTFnCall {
    pub function: String,
    pub args: Vec<ASTExpr>,
}

impl FromPair for ASTFnCall {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let mut inner = pair.into_inner();

        let function = string_from_ident(inner.next().unwrap());

        let args = if let Some(inner) = inner.next() {
            inner.into_inner().map(|p| ASTExpr::from_pair(p)).collect()
        } else {
            vec![]
        };

        Self { function, args }
    }
}

#[derive(Debug, Clone)]
pub struct ASTVarAssign {
    pub target: String,
    pub source: ASTExpr,
}

impl FromPair for ASTVarAssign {
    fn from_pair(pair: Pair<'_, Rule>) -> Self {
        let mut inner = pair.into_inner();
        let target = string_from_ident(inner.next().unwrap());
        let source = ASTExpr::from_pair(inner.next().unwrap());

        Self { target, source }
    }
}

#[derive(Debug, Clone)]
pub enum ASTExpr {
    InnerExpr(ASTInnerExpr),
    Variable(String),
    Literal(Literal),
    FnCall(ASTFnCall),
}

impl FromPair for ASTExpr {
    fn from_pair(expr: Pair<'_, Rule>) -> Self {
        // TODO: negation

        match expr.as_rule() {
            Rule::Expr | Rule::MulExpr | Rule::AddExpr | Rule::EqualExpr => {
                match ASTInnerExpr::from_pair(expr) {
                    AOrB::A(i) => Self::InnerExpr(i),
                    AOrB::B(p) => Self::from_pair(p),
                }
            }

            Rule::PrimExpr => {
                let inner = expr.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::string => {
                        let string = inner.as_str();
                        let len = string.len();
                        Self::Literal(Literal::String(string[1..len - 1].to_owned()))
                    }
                    Rule::number => Self::Literal(Literal::Number(inner.as_str().parse().unwrap())),

                    Rule::Expr => Self::from_pair(inner),

                    Rule::Ident => Self::Variable(string_from_ident(inner)),
                    Rule::FnCall => Self::FnCall(ASTFnCall::from_pair(inner)),
                    _ => unreachable!(),
                }
            }
            // other
            e => panic!("{e:?}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTInnerExpr {
    pub lhs: Box<ASTExpr>,
    pub op: BinOp,
    pub rhs: Box<ASTExpr>,
}

impl ASTInnerExpr {
    fn from_pair(pair: Pair<'_, Rule>) -> AOrB<Self, Pair<'_, Rule>> {
        let mut inner: Pairs<'_, Rule> = pair.into_inner();

        if inner.len() == 1 {
            let next = inner.next().unwrap();

            if next.as_rule() == Rule::PrimExpr {
                AOrB::B(next)
            } else {
                ASTInnerExpr::from_pair(next)
            }
        } else {
            let lhs = Box::new(ASTExpr::from_pair(inner.next().unwrap()));
            let op = binop_from_pair(inner.next().unwrap());
            let rhs = Box::new(ASTExpr::from_pair(inner.next().unwrap()));

            AOrB::A(Self { lhs, op, rhs })
        }
    }
}

fn binop_from_pair(pair: Pair<'_, Rule>) -> BinOp {
    match pair.as_rule() {
        Rule::Eq => BinOp::Eq,
        Rule::Neq => BinOp::Neq,
        Rule::Add => BinOp::Add,
        Rule::Sub => BinOp::Sub,
        Rule::Mul => BinOp::Mul,
        Rule::Div => BinOp::Div,
        Rule::Mod => BinOp::Mod,
        Rule::GT => BinOp::Gt,
        Rule::GTE => BinOp::Gte,
        Rule::LT => BinOp::Lt,
        Rule::LTE => BinOp::Lte,
        _ => unreachable!(),
    }
}

fn get_list<T: FromPair>(pair: Pair<'_, Rule>) -> Vec<T> {
    pair.into_inner().map(|i| T::from_pair(i)).collect()
}

fn string_from_ident(pair: Pair<'_, Rule>) -> String {
    assert_eq!(pair.as_rule(), Rule::Ident);
    pair.as_str().into()
}
