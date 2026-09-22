use std::rc::Rc;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Env<K,V> {
    Empty,
    Node(K, V, Rc<Env<K,V>>),
}

impl<K:Display + Clone + PartialEq, V:Clone> Env<K,V> {
    pub fn new() -> Env<K,V> {
        Env::Empty
    }

    pub fn push(&self, var: K, val: V) -> Env<K,V> {
        Env::Node(var.clone(), val, Rc::new(self.clone()))
    }

    pub fn find(&self, var: K) -> Result<V,String> {
        match self {
            Env::Empty => Err(format!("Variable not found {}",var)),
            Env::Node(x  ,val,next) => 
                if *x == var { Ok(val.clone()) } 
                else { next.find(var) }
        }
    }
}