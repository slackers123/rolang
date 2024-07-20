use std::collections::HashMap;

use crate::{
    ast::{
        ASTBlock, ASTBlockStmt, ASTExpr, ASTFnCall, ASTFnDef, ASTInnerExpr, ASTOuterStmt,
        ASTTypedVar, Ast,
    },
    bytecode::instructions::LdCmpType,
    ty::{BinOp, Literal},
};

use super::{
    instructions::{Instruction, RelBcIndex, Source, Target},
    interpreter::{ConstId, FunctionIndex, RegId},
    BcFunction, BcValue,
};

pub fn generate_from_ast(ast: Ast) -> (Option<String>, HashMap<FunctionIndex, BcFunction>) {
    let mut entry = None;
    let mut funcs = HashMap::new();
    for outer_stmt in ast.outer_stmts {
        match outer_stmt {
            ASTOuterStmt::Entry(e) => entry = Some(e.fn_name),
            ASTOuterStmt::Use(_u) => {}
            ASTOuterStmt::FnDef(f) => {
                let mut gen = FunctionGenerator::new(f.args);
                gen.generate_block(f.block);
                funcs.insert(f.fn_name, gen.get_bc_func());
            }
        }
    }
    (entry, funcs)
}

pub struct FunctionGenerator {
    instructions: Vec<Instruction>,
    consts: Vec<BcValue>,
    registers: Vec<RegId>,
    vars: HashMap<String, RegId>,
}

impl FunctionGenerator {
    pub fn new(args: Vec<ASTTypedVar>) -> Self {
        let args: Vec<String> = args.into_iter().map(|s| s.var).collect();
        let mut new = Self {
            instructions: vec![],
            consts: vec![],
            registers: vec![],
            vars: HashMap::new(),
        };
        for arg in args {
            new.alloc_var(arg);
        }

        new
    }
    pub fn get_bc_func(self) -> BcFunction {
        BcFunction {
            instructions: self.instructions,
            consts: self.consts,
        }
    }

    pub fn generate_block(&mut self, source_block: ASTBlock) -> usize {
        let mut len = 0;
        for stmt in source_block.stmts {
            match stmt {
                ASTBlockStmt::VarAssign(a) => {
                    let reg = self.get_var_reg_id(&a.target);
                    len += self.generate_expr(a.source, Target::Register(reg));
                }
                ASTBlockStmt::VarDef(d) => {
                    self.alloc_var(d.name.clone());
                    if let Some(assign) = d.assign {
                        let reg = self.get_var_reg_id(&d.name);
                        len += self.generate_expr(assign, Target::Register(reg));
                    }
                }
                ASTBlockStmt::FnCall(c) => {
                    len += self.generate_function_call(c, None);
                }
                ASTBlockStmt::IfStmt(i) => {
                    let res_reg = self.alloc_reg();
                    len += self.generate_expr(i.expr, Target::Register(res_reg));
                    let true_const_id = self.get_const_id(BcValue::Bool(true));
                    self.instructions.push(Instruction::Cmp {
                        lhs: Source::Register(res_reg),
                        rhs: Source::Constant(true_const_id),
                    });
                    len += 1;
                    let jmp_idx = self.instructions.len();
                    self.instructions.push(Instruction::Jne { goal: 0 });
                    len += 1;

                    let block_len = self.generate_block(i.block);
                    len += block_len;
                    if let Instruction::Jne { goal } = &mut self.instructions[jmp_idx] {
                        *goal = block_len as RelBcIndex + 1;
                    } else {
                        panic!("failed to update jump instruction");
                    }
                }
                ASTBlockStmt::RetStmt(r) => {
                    let ret_reg = self.alloc_reg();
                    len += self.generate_expr(r.expr, Target::Register(ret_reg));
                    self.instructions.push(Instruction::Return {
                        val: Source::Register(ret_reg),
                    });
                    len += 1;
                }
            }
        }
        len
    }

    fn generate_expr(&mut self, source_expr: ASTExpr, target: Target) -> usize {
        match source_expr {
            ASTExpr::Literal(l) => self.generate_literal_load(l, target),
            ASTExpr::Variable(v) => self.generate_variable_load(v, target),
            ASTExpr::InnerExpr(e) => self.generate_inner_expr(e, target),
            ASTExpr::FnCall(f) => self.generate_function_call(f, Some(target)),
        }
    }

    fn generate_function_call(&mut self, source_call: ASTFnCall, target: Option<Target>) -> usize {
        let mut len = 0;
        for (i, arg) in source_call.args.into_iter().enumerate() {
            len += self.generate_expr(arg, Target::CallRegister(i as u8));
        }
        self.instructions.push(Instruction::Call {
            target,
            function: source_call.function,
        });
        len += 1;

        len
    }

    fn generate_literal_load(&mut self, source_literal: Literal, target: Target) -> usize {
        let val = BcValue::from_literal(source_literal);
        let const_id = self.get_const_id(val);

        self.instructions.push(Instruction::Mov {
            target,
            source: Source::Constant(const_id),
        });

        1
    }

    fn generate_variable_load(&mut self, source_variable: String, target: Target) -> usize {
        let reg_id = self.get_var_reg_id(&source_variable);
        self.instructions.push(Instruction::Mov {
            target,
            source: Source::Register(reg_id),
        });

        1
    }

    fn generate_inner_expr(&mut self, source_expr: ASTInnerExpr, target: Target) -> usize {
        let mut len = 0;
        let lhs_reg = self.alloc_reg();
        len += self.generate_expr(*source_expr.lhs, Target::Register(lhs_reg));
        let rhs_reg = self.alloc_reg();
        len += self.generate_expr(*source_expr.rhs, Target::Register(rhs_reg));

        let lhs = Source::Register(lhs_reg);
        let rhs = Source::Register(rhs_reg);

        match source_expr.op {
            BinOp::Eq | BinOp::Gt | BinOp::Lt | BinOp::Gte | BinOp::Lte | BinOp::Neq => {
                self.instructions.push(Instruction::Cmp { lhs, rhs });
                self.instructions.push(Instruction::LdCmp {
                    target,
                    ty: LdCmpType::from_binop(source_expr.op),
                });
                len += 1;
            }
            BinOp::Add => self
                .instructions
                .push(Instruction::Add { target, lhs, rhs }),
            BinOp::Sub => self
                .instructions
                .push(Instruction::Sub { target, lhs, rhs }),
            BinOp::Mul => self
                .instructions
                .push(Instruction::Mul { target, lhs, rhs }),
            BinOp::Div => self
                .instructions
                .push(Instruction::Div { target, lhs, rhs }),
            BinOp::Mod => self
                .instructions
                .push(Instruction::Mod { target, lhs, rhs }),
        }
        len += 1;
        self.release_reg(lhs_reg);
        self.release_reg(rhs_reg);
        len
    }

    fn alloc_var(&mut self, name: String) {
        let new_reg = self.alloc_reg();
        self.vars.insert(name, new_reg);
    }

    fn alloc_reg(&mut self) -> RegId {
        let mut reg = None;
        let mut i = 0;
        while reg.is_none() {
            if self.registers.contains(&i) {
                if i == 255 {
                    panic!("out of registers");
                }
                i += 1;
            } else {
                reg = Some(i);
            }
        }
        let reg = reg.unwrap();

        self.registers.push(reg);
        reg
    }

    fn release_reg(&mut self, reg: RegId) {
        for (i, r) in self.registers.iter().enumerate() {
            if *r == reg {
                self.registers.remove(i);
                return;
            }
        }
    }

    fn get_var_reg_id(&mut self, var_name: &String) -> RegId {
        *self
            .vars
            .get(var_name)
            .expect(&format!("variable {:?} not found", var_name))
    }

    fn get_const_id(&mut self, val: BcValue) -> ConstId {
        self.consts
            .iter()
            .enumerate()
            .find(|v| *v.1 == val)
            .map(|v| v.0)
            .unwrap_or_else(|| {
                self.consts.push(val);
                self.consts.len() - 1
            }) as ConstId
    }
}
