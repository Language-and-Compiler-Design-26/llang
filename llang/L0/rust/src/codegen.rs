
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use crate::environment::Env;
use crate::symbols::{Symbol, new_symbol, string_of_symbol};
use crate::ast_typed::{AstTyped, AstTypedData};
use crate::ast::{BinOp, UnOp, Type, TypeData};

pub type Register = u32;
pub type Label = u32;
pub type FnName = Symbol;
pub type StructName = Symbol;

#[derive(Debug, Clone, PartialEq)]
pub enum IType {
    I32,
    I1,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CmpOp {
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge
}

#[derive(Debug, Clone, PartialEq)]
pub enum LlvmResult {
    IConst(i64, IType),
    Register(Register),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
  Addi32(Register, LlvmResult, LlvmResult),
  Subi32(Register, LlvmResult, LlvmResult),
  Muli32(Register, LlvmResult, LlvmResult),
  Divi32(Register, LlvmResult, LlvmResult),
  Cmp(Register, CmpOp, LlvmResult, LlvmResult, IType),
  Xor(Register, LlvmResult, LlvmResult), // Only for i1
}

pub struct Block {
    pub label: Label,
    pub instructions: Vec<Instr>,
}

pub struct Struct {
    pub name: StructName,
    pub fields: Vec<(Symbol,IType)>,
}

pub struct CompilationUnit {
    pub result: LlvmResult,
    pub label: Label,
    pub instructions: Vec<Instr>,
    pub blocks: Vec<Block>,
}


impl CompilationUnit {
    pub fn new(result: LlvmResult, label: Label, instructions: Vec<Instr>, blocks: Vec<Block>) -> Self {
        CompilationUnit { result, label, instructions, blocks: blocks}
    }
}

thread_local! {
    static STACK: RefCell<Vec<(Cell<Register>, Cell<Label>)>> = RefCell::new(vec![(Cell::new(0), Cell::new(0))]);
}

fn new_reg() -> Register {
    STACK.with(|s| {
        let stack = s.borrow();
        let (reg_count, _) = stack.last().expect("register stack is empty");
        let reg = reg_count.get();
        reg_count.set(reg+1);
        reg
    })
}

fn new_label() -> Label {
    STACK.with(|s| {
        let stack = s.borrow();
        let (_, label_count) = stack.last().expect("register stack is empty");
        let label = label_count.get();
        label_count.set(label+1);
        label
    })
}

fn reg_begin_scope() {
    STACK.with(|s| s.borrow_mut().push((Cell::new(0), Cell::new(0))));
}

fn reg_end_scope() {
    STACK.with(|s| { s.borrow_mut().pop(); });
}

fn llvm_type_of(t:&Type) -> IType {
    match t.as_ref() {
        TypeData::Int => IType::I32,
        TypeData::Bool => IType::I1,
    }
}

pub trait Compile {
    fn compile(&self) -> Result<CompilationUnit, String>;
}

impl Compile for AstTyped {
    // This function compiles the root of the AST producing the code of the main function, 
    // and the list of functions and structs that are defined in the program.
    fn compile(&self) -> Result<CompilationUnit, String> {
        let cu = self.compile_internal(0, Vec::new(), Rc::new(Env::new()), Vec::new())?;
        Ok(cu)
    }
}
trait CompileInternal {
    fn compile_internal(
        &self, 
        l0:Label, 
        b0:Vec<Instr>, 
        env: Rc<Env<Symbol, (LlvmResult, Type)>>, 
        blocks:Vec<Block>) -> Result<CompilationUnit, String>;

    fn compile_bin_op(
        &self,
        l0:Label, 
        b0:Vec<Instr>, 
        env: Rc<Env<Symbol, (LlvmResult, Type)>>, 
        blocks:Vec<Block>) -> Result<CompilationUnit, String>;
}

fn generate_instr(op:&BinOp, ret:Register, r1: LlvmResult, r2: LlvmResult) -> Result<Instr, String> {
    match op {
        BinOp::Add => Ok(Instr::Addi32(ret, r1, r2)),
        BinOp::Sub => Ok(Instr::Subi32(ret, r1, r2)),
        BinOp::Mul => Ok(Instr::Muli32(ret, r1, r2)),
        BinOp::Div => Ok(Instr::Divi32(ret, r1, r2)),
        BinOp::Eq => Ok(Instr::Cmp(ret, CmpOp::Eq, r1, r2, IType::I32)), // These may compare other types 
        BinOp::Ne => Ok(Instr::Cmp(ret, CmpOp::Ne, r1, r2, IType::I32)),
        BinOp::Lt => Ok(Instr::Cmp(ret, CmpOp::Lt, r1, r2, IType::I32)),
        BinOp::Le => Ok(Instr::Cmp(ret, CmpOp::Le, r1, r2, IType::I32)),
        BinOp::Gt => Ok(Instr::Cmp(ret, CmpOp::Gt, r1, r2, IType::I32)),
        BinOp::Ge => Ok(Instr::Cmp(ret, CmpOp::Ge, r1, r2, IType::I32)),
        _ => Err("Not a binary operation that maps to one LLVM instruction".into())
    }
}

impl CompileInternal for AstTyped {
    fn compile_internal(
        &self, 
        l0:Label, 
        b0:Vec<Instr>, 
        env: Rc<Env<Symbol, (LlvmResult,Type)>>, 
        blocks:Vec<Block>) -> Result<CompilationUnit, String> {

        match self.as_ref() {
            AstTypedData::Num(n) =>
                Ok(CompilationUnit::new(
                    LlvmResult::IConst(*n, IType::I32),
                    l0,
                    b0,
                    blocks,
                )),

            AstTypedData::Bool(b) =>
                Ok(CompilationUnit::new(
                    LlvmResult::IConst(if *b {1} else {0}, IType::I1),
                    l0,
                    b0,
                    blocks,
                )),

            AstTypedData::UnOp(op, e, _) => {
                let cu: CompilationUnit = e.compile_internal(l0, b0, env, blocks)?;
                let ret = new_reg();
                let mut instructions = cu.instructions;
                match op {
                    UnOp::Neg => {
                        let instr = Instr::Muli32(ret, cu.result, LlvmResult::IConst(-1,IType::I32));
                        instructions.push(instr);
                        Ok(CompilationUnit::new(
                            LlvmResult::Register(ret),
                            cu.label,
                            instructions,
                            cu.blocks,
                        ))
                    },
                    UnOp::Not => {
                        let instr = Instr::Xor(ret, cu.result, LlvmResult::IConst(1,IType::I1));
                        instructions.push(instr);
                        Ok(CompilationUnit::new(
                            LlvmResult::Register(ret),
                            cu.label,
                            instructions,
                            cu.blocks,
                        ))
                    },
                }
            },

            AstTypedData::BinOp(..) => {
                self.compile_bin_op(l0, b0, env, blocks)
            },
        }
    }

    fn compile_bin_op(&self,l0:Label, b0:Vec<Instr>, env: Rc<Env<Symbol, (LlvmResult,Type)>>, blocks:Vec<Block>) -> Result<CompilationUnit, String> {
        if let AstTypedData::BinOp(op, e1, e2, _t) = self.as_ref() {
            match op {
                _ => { // Operations that are obtained by a single instruction by compiling both blocks
                    let cu_1 = e1.compile_internal(l0, b0, env.clone(), blocks)?;
                    let mut cu_2 = e2.compile_internal(cu_1.label, cu_1.instructions, env, cu_1.blocks)?;
                    let ret = new_reg();
                    let instr = generate_instr(op, ret, cu_1.result, cu_2.result)?;
                    cu_2.instructions.push(instr);
                    Ok(CompilationUnit::new(
                        LlvmResult::Register(ret),
                        cu_2.label,
                        cu_2.instructions,
                        cu_2.blocks,
                    ))
                }
            }
        } else {
            Err("Compile internal error: This should not happen.".into())
        }
    }
}
