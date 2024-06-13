use crate::ast::Ast;
use ast::gen_ast;
use parser::RolangParser;
use pest::Parser;
use thiserror::Error;

mod ast;
pub mod bytecode;
mod parser;
mod ty;
mod walker;

#[derive(Debug, Error)]
pub enum ROCError {
    #[error("{0:?}")]
    IoError(#[from] std::io::Error),

    #[error("{0:?}")]
    ParserError(#[from] Box<pest::error::Error<parser::Rule>>),

    #[error("{0:?}")]
    ASTError(#[from] ast::ASTError),
}

pub type ROCResult<T> = Result<T, ROCError>;

pub fn build_file(file: &str) -> ROCResult<Ast> {
    let parse = RolangParser::parse(parser::Rule::File, file).map_err(Box::new)?;
    let ast = gen_ast(parse)?;
    Ok(ast)
}

pub fn run_ast(ast: Ast) {
    let mut walker = walker::FileWalker::new(ast);
    walker.run();
}
