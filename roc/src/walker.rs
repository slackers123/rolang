use crate::ast::{
    ASTBlock, ASTBlockStmt, ASTExpr, ASTFnDef, ASTInnerExpr, ASTOuterStmt, ASTTypePath,
    ASTTypedVar, Ast,
};
use crate::ty::{BinOp, Literal};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct FileWalker {
    functions: HashMap<String, ASTFnDef>,
    entry: Option<String>,
}

impl FileWalker {
    pub fn new(ast: Ast) -> Self {
        let mut entry = None;
        let mut functions = HashMap::new();

        for stmt in ast.outer_stmts {
            match stmt {
                ASTOuterStmt::Entry(e) => {
                    entry = Some(e.fn_name);
                }
                ASTOuterStmt::Use(u) => {
                    if u.path.path[0] != "std" {
                        todo!("importing")
                    }
                    let fn_name = u.path.path[1].clone();
                    functions.insert(
                        fn_name.clone(),
                        match fn_name.as_str() {
                            "println" => ASTFnDef {
                                fn_name,
                                args: vec![ASTTypedVar {
                                    var: "str".into(),
                                    ty: ASTTypePath {
                                        path: vec!["String".into()],
                                    },
                                }],
                                ret_type: None,
                                block: ASTBlock { stmts: vec![] },
                            },
                            _ => panic!("unknown standard function: {fn_name}"),
                        },
                    );
                }
                ASTOuterStmt::FnDef(f) => {
                    functions.insert(f.fn_name.clone(), f);
                }
            }
        }

        Self { entry, functions }
    }

    pub fn run(&mut self) {
        let start_fn = self
            .entry
            .clone()
            .expect("to run an entrypoint needs to be defined");

        self.run_fn(start_fn, vec![]);
    }

    fn run_fn(&mut self, name: String, args: Vec<WalkerValue>) -> Option<WalkerValue> {
        let function = self
            .functions
            .get(&name)
            .unwrap_or_else(|| panic!("the function {name:?} wasn't found."))
            .clone();
        let mut env = WalkerEnv {
            locals: args
                .into_iter()
                .zip(function.args)
                .map(|(v, n)| (n.var, v))
                .collect(),
        };
        #[allow(clippy::single_match)]
        match function.fn_name.as_str() {
            "println" => {
                println!("{}", env.locals.get("str").unwrap())
            }
            _ => (),
        }
        self.run_block(&function.block, &mut env)
    }

    fn run_block(&mut self, block: &ASTBlock, env: &mut WalkerEnv) -> Option<WalkerValue> {
        for stmt in &block.stmts {
            match stmt {
                ASTBlockStmt::VarDef(v) => {
                    // TODO: type checking
                    if let Some(assign) = &v.assign {
                        let val = self.run_expr(assign, env);
                        env.locals.insert(v.name.clone(), val);
                    } else {
                        env.locals.insert(v.name.clone(), WalkerValue::Undefined);
                    }
                }

                ASTBlockStmt::VarAssign(v) => {
                    let assign = self.run_expr(&v.source, env);
                    env.locals.insert(v.target.clone(), assign);
                }
                ASTBlockStmt::FnCall(f) => {
                    let args = f.args.iter().map(|m| self.run_expr(m, env)).collect();
                    self.run_fn(f.function.clone(), args);
                }
                ASTBlockStmt::IfStmt(i) => {
                    let case = self.run_expr(&i.expr, env);
                    if case.get_bool().unwrap() {
                        if let Some(ret) = self.run_block(&i.block, env) {
                            return Some(ret);
                        }
                    }
                }
                ASTBlockStmt::RetStmt(r) => {
                    let ret = self.run_expr(&r.expr, env);
                    return Some(ret);
                }
            }
        }
        None
    }

    fn run_expr(&mut self, expr: &ASTExpr, env: &mut WalkerEnv) -> WalkerValue {
        match expr {
            ASTExpr::Variable(v) => env.locals.get(v).unwrap().clone(),
            ASTExpr::Literal(l) => match l {
                Literal::Number(n) => WalkerValue::Number(*n),
                Literal::String(s) => WalkerValue::String(s.clone()),
            },
            ASTExpr::InnerExpr(e) => self.run_inner_expr(e, env),
            ASTExpr::FnCall(f) => {
                let args = f.args.iter().map(|m| self.run_expr(m, env)).collect();
                self.run_fn(f.function.clone(), args).unwrap()
            }
        }
    }

    fn run_inner_expr(&mut self, expr: &ASTInnerExpr, env: &mut WalkerEnv) -> WalkerValue {
        let lhs = self.run_expr(expr.lhs.as_ref(), env);
        let rhs = self.run_expr(expr.rhs.as_ref(), env);
        match expr.op {
            BinOp::Add => lhs + rhs,
            BinOp::Sub => lhs - rhs,
            BinOp::Mul => lhs * rhs,

            BinOp::Div => lhs / rhs,

            BinOp::Mod => lhs % rhs,

            BinOp::Eq => WalkerValue::Bool(lhs == rhs),

            BinOp::Neq => WalkerValue::Bool(lhs != rhs),

            BinOp::Gt => WalkerValue::Bool(lhs > rhs),

            BinOp::Lt => WalkerValue::Bool(lhs < rhs),
            BinOp::Gte => WalkerValue::Bool(lhs >= rhs),
            BinOp::Lte => WalkerValue::Bool(lhs <= rhs),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum WalkerValue {
    String(String),
    Number(f64),
    Bool(bool),
    #[allow(unused)]
    Function(String),
    Undefined,
}

impl Display for WalkerValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            WalkerValue::Undefined => write!(f, "undefined")?,
            WalkerValue::Function(fs) => write!(f, "{fs}")?,
            WalkerValue::Number(n) => write!(f, "{n:?}")?,
            WalkerValue::Bool(b) => write!(f, "{b:?}")?,
            WalkerValue::String(s) => write!(f, "{s}")?,
        }
        Ok(())
    }
}

impl WalkerValue {
    #[allow(unused)]
    pub fn get_string(self) -> Option<String> {
        match self {
            Self::String(s) => Some(s),
            _ => None,
        }
    }
    #[allow(unused)]
    pub fn get_number(self) -> Option<f64> {
        match self {
            Self::Number(n) => Some(n),
            _ => None,
        }
    }
    #[allow(unused)]
    pub fn get_bool(self) -> Option<bool> {
        match self {
            Self::Bool(b) => Some(b),
            _ => None,
        }
    }
    #[allow(unused)]
    pub fn get_function(self) -> Option<String> {
        match self {
            Self::Function(f) => Some(f),
            _ => None,
        }
    }
    #[allow(unused)]
    pub fn get_undefined(self) -> Option<()> {
        match self {
            Self::Undefined => Some(()),
            _ => None,
        }
    }
}

impl std::ops::Add for WalkerValue {
    type Output = WalkerValue;
    fn add(self, rhs: Self) -> Self::Output {
        match self {
            WalkerValue::Undefined | WalkerValue::Function(_) | WalkerValue::Bool(_) => {
                panic!("it's only possible to add strings and numbers")
            }

            WalkerValue::Number(n) => {
                if let WalkerValue::Number(n1) = rhs {
                    WalkerValue::Number(n + n1)
                } else {
                    panic!("it's only possible to add strings and numbers")
                }
            }
            WalkerValue::String(s) => {
                let rhs = match rhs {
                    WalkerValue::String(s) => s,
                    WalkerValue::Number(n) => n.to_string(),
                    WalkerValue::Bool(b) => b.to_string(),
                    WalkerValue::Function(_) => panic!("cannot add a function"),
                    WalkerValue::Undefined => "undefined".into(),
                };
                WalkerValue::String(format!("{s}{rhs}"))
            }
        }
    }
}

impl std::ops::Sub for WalkerValue {
    type Output = WalkerValue;
    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            WalkerValue::Undefined
            | WalkerValue::Function(_)
            | WalkerValue::Bool(_)
            | WalkerValue::String(_) => {
                panic!("it's only possible to sub numbers")
            }

            WalkerValue::Number(n) => {
                if let WalkerValue::Number(n1) = rhs {
                    WalkerValue::Number(n - n1)
                } else {
                    panic!("it's only possible to sub numbers")
                }
            }
        }
    }
}

impl std::ops::Mul for WalkerValue {
    type Output = WalkerValue;
    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            WalkerValue::Undefined
            | WalkerValue::Function(_)
            | WalkerValue::Bool(_)
            | WalkerValue::String(_) => {
                panic!("it's only possible to mul numbers")
            }

            WalkerValue::Number(n) => {
                if let WalkerValue::Number(n1) = rhs {
                    WalkerValue::Number(n * n1)
                } else {
                    panic!("it's only possible to mul numbers")
                }
            }
        }
    }
}

impl std::ops::Div for WalkerValue {
    type Output = WalkerValue;
    fn div(self, rhs: Self) -> Self::Output {
        match self {
            WalkerValue::Undefined
            | WalkerValue::Function(_)
            | WalkerValue::Bool(_)
            | WalkerValue::String(_) => {
                panic!("it's only possible to div numbers")
            }

            WalkerValue::Number(n) => {
                if let WalkerValue::Number(n1) = rhs {
                    WalkerValue::Number(n / n1)
                } else {
                    panic!("it's only possible to mul numbers")
                }
            }
        }
    }
}

impl std::ops::Rem for WalkerValue {
    type Output = WalkerValue;
    fn rem(self, rhs: Self) -> Self::Output {
        match self {
            WalkerValue::Undefined
            | WalkerValue::Function(_)
            | WalkerValue::Bool(_)
            | WalkerValue::String(_) => {
                panic!("it's only possible to mod numbers")
            }

            WalkerValue::Number(n) => {
                if let WalkerValue::Number(n1) = rhs {
                    WalkerValue::Number(n % n1)
                } else {
                    panic!("it's only possible to mod numbers")
                }
            }
        }
    }
}

impl std::cmp::PartialOrd for WalkerValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            Self::Number(n) => match other {
                Self::Number(n2) => n.partial_cmp(n2),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct WalkerEnv {
    locals: HashMap<String, WalkerValue>,
}
