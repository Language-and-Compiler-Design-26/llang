
use std::rc::Rc;
use crate::symbols::Symbol;
use crate::ast::{Ast, AstData, BinOp, UnOp, Type, TypeData};
use crate::environment::Env;
use crate::ast_typed::{AstTyped, AstTypedData};

pub trait TypeCheck {
    fn type_check(&self, env: Rc<Env<Symbol, Type>>) -> Result<AstTyped, String>;
}

impl TypeCheck for Ast {
    fn type_check(&self, env: Rc<Env<Symbol, Type>>) -> Result<AstTyped, String> {
        match self.as_ref() {
            AstData::Num(n) => Ok(Rc::new(AstTypedData::Num(*n))),

            AstData::Bool(b) => Ok(Rc::new(AstTypedData::Bool(*b))),

            AstData::UnOp(op, e) => {
                let t_e = e.type_check(env)?;
                let t = t_e.type_of();
                match (op, t.as_ref()) {
                    (UnOp::Neg, TypeData::Int) => Ok(Rc::new(AstTypedData::UnOp(UnOp::Neg, t_e, Rc::new(TypeData::Int)))),
                    (UnOp::Neg, _) => Err("Type error: expected Int".into()),
                    (UnOp::Not, TypeData::Bool) => Ok(Rc::new(AstTypedData::UnOp(UnOp::Not, t_e, Rc::new(TypeData::Bool)))),
                    (UnOp::Not, _) => Err("Type error: expected Bool".into()),
                }
            },

            AstData::BinOp(op, e1, e2) => {
                let t_e1 = e1.type_check(env.clone())?;
                let t_e2 = e2.type_check(env)?;
                let t_1 = t_e1.type_of();
                let t_2 = t_e2.type_of();
                match (op, t_1.as_ref(), t_2.as_ref()) {
                    (BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div, TypeData::Int, TypeData::Int)=>
                            Ok(Rc::new(AstTypedData::BinOp(op.clone(), t_e1, t_e2, Rc::new(TypeData::Int)))),
                    (BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div,_,_) =>
                            Err("Type error: expected Int".into()),
                    (BinOp::And | BinOp::Or, TypeData::Bool, TypeData::Bool) =>
                            Ok(Rc::new(AstTypedData::BinOp(op.clone(), t_e1, t_e2, Rc::new(TypeData::Bool)))),
                    (BinOp::And | BinOp::Or, _, _) =>
                            Err("Type error: expected Bool".into()),
                    (BinOp::Eq | BinOp::Lt | BinOp::Gt | BinOp::Ne | BinOp::Le | BinOp::Ge, _, _) => {
                        if *t_1 == *t_2 {
                            Ok(Rc::new(AstTypedData::BinOp(op.clone(), t_e1, t_e2, Rc::new(TypeData::Bool))))
                        } else {
                            Err("Type error: expected same type for comparison".into())
                        }
                    },
                }
            },
        }
    }
}