use crate::ast::{Ast, AstData, BinOp, Type, TypeData, UnOp};
use crate::ast_typed::{AstTyped, AstTypedData};
use crate::environment::Env;
use crate::symbols::Symbol;
use std::rc::Rc;

pub trait TypeCheck {
    fn type_check(&self, env: Rc<Env<Symbol, Type>>) -> Result<AstTyped, String>;
}

impl TypeCheck for Ast {
    fn type_check(&self, env: Rc<Env<Symbol, Type>>) -> Result<AstTyped, String> {
        match self.as_ref() {
            AstData::Num(n) => Ok(Rc::new(AstTypedData::Num(*n))),

            AstData::Bool(b) => Ok(Rc::new(AstTypedData::Bool(*b))),

            AstData::Id(x) => Ok(Rc::new(AstTypedData::Id(*x, env.find(*x)?))),

            AstData::UnOp(op, e) => {
                let t_e = e.type_check(env)?;
                let t = t_e.type_of();
                match (op, t.as_ref()) {
                    (UnOp::Neg, TypeData::Int) => Ok(Rc::new(AstTypedData::UnOp(
                        UnOp::Neg,
                        t_e,
                        Rc::new(TypeData::Int),
                    ))),
                    (UnOp::Neg, _) => Err("Type error: expected Int".into()),
                    (UnOp::Not, TypeData::Bool) => Ok(Rc::new(AstTypedData::UnOp(
                        UnOp::Not,
                        t_e,
                        Rc::new(TypeData::Bool),
                    ))),
                    (UnOp::Not, _) => Err("Type error: expected Bool".into()),
                }
            }

            AstData::Abs(x, t1, e) => {
                let new_env = env.push(*x, t1.clone());
                let t_e2 = e.type_check(Rc::new(new_env))?;
                let t_2 = t_e2.type_of();
                Ok(Rc::new(AstTypedData::Abs(
                    *x,
                    t1.clone(),
                    t_e2,
                    Rc::new(TypeData::Arrow(t1.clone(), t_2)),
                )))
            }

            AstData::BinOp(op, e1, e2) => {
                let t_e1 = e1.type_check(env.clone())?;
                let t_e2 = e2.type_check(env)?;
                let t_1 = t_e1.type_of();
                let t_2 = t_e2.type_of();
                match (op, t_1.as_ref(), t_2.as_ref()) {
                    (
                        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div,
                        TypeData::Int,
                        TypeData::Int,
                    ) => Ok(Rc::new(AstTypedData::BinOp(
                        op.clone(),
                        t_e1,
                        t_e2,
                        Rc::new(TypeData::Int),
                    ))),
                    (BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div, _, _) => {
                        Err("Type error: expected Int".into())
                    }
                    (BinOp::And | BinOp::Or, TypeData::Bool, TypeData::Bool) => Ok(Rc::new(
                        AstTypedData::BinOp(op.clone(), t_e1, t_e2, Rc::new(TypeData::Bool)),
                    )),
                    (BinOp::And | BinOp::Or, _, _) => Err("Type error: expected Bool".into()),
                    (
                        BinOp::Eq | BinOp::Lt | BinOp::Gt | BinOp::Ne | BinOp::Le | BinOp::Ge,
                        _,
                        _,
                    ) => {
                        if *t_1 == *t_2 {
                            Ok(Rc::new(AstTypedData::BinOp(
                                op.clone(),
                                t_e1,
                                t_e2,
                                Rc::new(TypeData::Bool),
                            )))
                        } else {
                            Err("Type error: expected same type for comparison".into())
                        }
                    }
                    (BinOp::App, TypeData::Arrow(t_arg, t_ret), _) => {
                        if t_arg == &t_2 {
                            Ok(Rc::new(AstTypedData::BinOp(
                                op.clone(),
                                t_e1,
                                t_e2,
                                t_ret.clone(),
                            )))
                        } else {
                            Err("Type error: argument type mismatch".into())
                        }
                    }
                    (BinOp::App, _, _) => Err("Type error: expected function type".into()),
                }
            }

            AstData::If(cond, then_e, else_e) => {
                let t_cond = cond.type_check(env.clone())?;
                let t_then = then_e.type_check(env.clone())?;
                let t_else = else_e.type_check(env)?;
                if *t_cond.type_of() == TypeData::Bool {
                    let t1 = t_then.type_of();
                    let t2 = t_else.type_of();
                    if *t1 == *t2 {
                        Ok(Rc::new(AstTypedData::If(
                            t_cond,
                            t_then,
                            t_else,
                            t1.clone(),
                        )))
                    } else {
                        Err(
                            "Type error: expecting equal types on both branches of condition"
                                .into(),
                        )
                    }
                } else {
                    Err("Type error: expecting a boolean in a condition".into())
                }
            }

            AstData::Let(x, e1, e2) => {
                let t_e1 = e1.type_check(env.clone())?;
                let t1 = t_e1.type_of();
                let new_env = env.push(*x, t1);
                let t_e2 = e2.type_check(Rc::new(new_env))?;
                let t2 = t_e2.type_of();
                Ok(Rc::new(AstTypedData::Let(*x, t_e1, t_e2, t2)))
            }
        }
    }
}
