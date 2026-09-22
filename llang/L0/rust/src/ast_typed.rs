use std::rc::Rc;

use crate::symbols::Symbol;
use crate::ast::{BinOp, UnOp};
use crate::ast::{Type, TypeData};


pub type AstTyped = Rc<AstTypedData>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstTypedData {
    Num(i64),
    Bool(bool),
    BinOp(BinOp, AstTyped, AstTyped, Type),
    UnOp(UnOp, AstTyped, Type),
}

impl AstTypedData {
    pub fn type_of(&self) -> Type {
        match self {
            AstTypedData::Num(_) => Rc::new(TypeData::Int),
            AstTypedData::Bool(_) => Rc::new(TypeData::Bool),
            AstTypedData::BinOp(_, _, _, t) => t.clone(),
            AstTypedData::UnOp(_, _, t) => t.clone(),
        }
    }
}