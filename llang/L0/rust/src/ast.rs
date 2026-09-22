use std::rc::Rc;

pub type Type = Rc<TypeData>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeData {
    Int,
    Bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Eq,
    Lt,
    Gt,
    Ne,
    Le,
    Ge,
}

pub type Symbol = i64;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnOp {
    Neg,
    Not
}

pub type Ast = Rc<AstData>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstData {
    Num(i64),
    Bool(bool),
    BinOp(BinOp, Ast, Ast),
    UnOp(UnOp, Ast),
}

