use crate::ast::{Ast, AstData, BinOp};
use crate::environment::Env;
use crate::symbols::Symbol;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Closure(Symbol, Ast, Rc<Env<Symbol, Value>>),
}

pub trait Eval {
    fn eval(&self, env: Rc<Env<Symbol, Value>>) -> Result<Value, String>;
}

impl Eval for Ast {
    fn eval(&self, env: Rc<Env<Symbol, Value>>) -> Result<Value, String> {
        match self.as_ref() {
            AstData::Num(x) => Ok(Value::Int(*x)),

            AstData::Bool(b) => Ok(Value::Bool(*b)),

            AstData::Id(x) => env.find(*x),

            AstData::Abs(x, _, e) => Ok(Value::Closure(x.clone(), e.clone(), env)),

            AstData::BinOp(op, e1, e2) => {
                let v1 = e1.eval(env.clone())?;
                let v2 = e2.eval(env)?;
                match op {
                    BinOp::Add => match (v1, v2) {
                        (Value::Int(n), Value::Int(m)) => Ok(Value::Int(n + m)),
                        _ => Err("Expecting integers".into()),
                    },
                    BinOp::Sub => match (v1, v2) {
                        (Value::Int(n), Value::Int(m)) => Ok(Value::Int(n - m)),
                        _ => Err("Expecting integers".into()),
                    },
                    BinOp::Mul => match (v1, v2) {
                        (Value::Int(n), Value::Int(m)) => Ok(Value::Int(n * m)),
                        _ => Err("Expecting integers".into()),
                    },
                    BinOp::Div => match (v1, v2) {
                        (Value::Int(n), Value::Int(m)) => {
                            if m == 0 {
                                Err("Division by zero".into())
                            } else {
                                Ok(Value::Int(n / m))
                            }
                        }
                        _ => Err("Expecting integers".into()),
                    },
                    BinOp::App => match v1 {
                        Value::Closure(param, body, closure_env) => {
                            let new_env = closure_env.push(param, v2);
                            body.eval(Rc::new(new_env))
                        }
                        _ => Err("Expected a function".into()),
                    },
                    BinOp::Eq => Ok(Value::Bool(v1 == v2)),
                    BinOp::Ne => Ok(Value::Bool(v1 != v2)),
                    BinOp::Lt => match (v1, v2) {
                        (Value::Int(n), Value::Int(m)) => Ok(Value::Bool(n < m)),
                        _ => Err("Expecting integers".into()),
                    },
                    BinOp::Gt => match (v1, v2) {
                        (Value::Int(n), Value::Int(m)) => Ok(Value::Bool(n > m)),
                        _ => Err("Expecting integers".into()),
                    },
                    BinOp::Le => match (v1, v2) {
                        (Value::Int(n), Value::Int(m)) => Ok(Value::Bool(n <= m)),
                        _ => Err("Expecting integers".into()),
                    },
                    BinOp::Ge => match (v1, v2) {
                        (Value::Int(n), Value::Int(m)) => Ok(Value::Bool(n >= m)),
                        _ => Err("Expecting integers".into()),
                    },

                    _ => Err("Internal Error, this should not happen".into()),
                }
            }

            AstData::UnOp(op, e) => {
                let v = e.eval(env)?;
                match op {
                    crate::ast::UnOp::Neg => match v {
                        Value::Int(n) => Ok(Value::Int(-n)),
                        _ => Err("Expecting an integer".into()),
                    },
                    crate::ast::UnOp::Not => match v {
                        Value::Bool(b) => Ok(Value::Bool(!b)),
                        _ => Err("Expecting a boolean".into()),
                    },
                }
            }

            AstData::Let(x, e1, e2) => {
                let v1 = e1.eval(env.clone())?;
                let new_env = env.push(*x, v1);
                e2.eval(Rc::new(new_env))
            }

            AstData::If(cond, then_e, else_e) => {
                let b = cond.eval(env.clone())?;
                if b == Value::Bool(true) {
                    then_e.eval(env)
                } else {
                    else_e.eval(env)
                }
            } // _ => Err("Unsupported AST node".into())
        }
    }
}
