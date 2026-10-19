use std::rc::Rc;

use crate::ast::{BinOp, UnOp};
use crate::ast::{Type, TypeData};
use crate::symbols::Symbol;

pub type AstTyped = Rc<AstTypedData>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstTypedData {
    Num(i64),
    Bool(bool),
    BinOp(BinOp, AstTyped, AstTyped, Type),
    UnOp(UnOp, AstTyped, Type),
    If(AstTyped, AstTyped, AstTyped, Type),
    Let(Symbol, AstTyped, AstTyped, Type),
    Id(Symbol, Type),
    Abs(Symbol, Type, AstTyped, Type),
}

impl AstTypedData {
    pub fn type_of(&self) -> Type {
        match self {
            AstTypedData::Num(_) => Rc::new(TypeData::Int),
            AstTypedData::Bool(_) => Rc::new(TypeData::Bool),
            AstTypedData::BinOp(_, _, _, t) => t.clone(),
            AstTypedData::UnOp(_, _, t) => t.clone(),
            AstTypedData::If(_, _, _, t) => t.clone(),
            AstTypedData::Let(_, _, _, t) => t.clone(),
            AstTypedData::Id(_, t) => t.clone(),
            AstTypedData::Abs(_, _, _, t) => t.clone(),
        }
    }
}
