use crate::codegen::{CompilationUnit, CmpOp, IType, Instr, Label, LlvmResult, Register};
use crate::symbols::string_of_symbol;

fn string_of_register(r: Register) -> String {
    format!("%{}", r)
}

pub fn string_of_label(l: Label) -> String {
    format!("label_{}", l)
}

fn string_of_type(t: &IType) -> String {
    match t {
        IType::I32 => "i32".into(),
        IType::I1 => "i1".into(),
    }
}

fn string_of_cmp_op(op: CmpOp) -> &'static str {
    match op {
        CmpOp::Eq => "eq",
        CmpOp::Ne => "ne",
        CmpOp::Lt => "slt",
        CmpOp::Le => "sle",
        CmpOp::Gt => "sgt",
        CmpOp::Ge => "sge",
    }
}

fn string_of_result(r: &LlvmResult) -> String {
    match r {
        LlvmResult::IConst(n, _) => n.to_string(),
        LlvmResult::Register(reg) => string_of_register(*reg),
    }
}

pub fn string_of_instr(instr: &Instr) -> String {
    match instr {
        Instr::Addi32(r, p1, p2) =>
            format!("{} = add nsw i32 {}, {}", string_of_register(*r), string_of_result(p1), string_of_result(p2)),
        Instr::Subi32(r, p1, p2) =>
            format!("{} = sub nsw i32 {}, {}", string_of_register(*r), string_of_result(p1), string_of_result(p2)),
        Instr::Muli32(r, p1, p2) =>
            format!("{} = mul nsw i32 {}, {}", string_of_register(*r), string_of_result(p1), string_of_result(p2)),
        Instr::Divi32(r, p1, p2) =>
            format!("{} = sdiv i32 {}, {}", string_of_register(*r), string_of_result(p1), string_of_result(p2)),
        Instr::Cmp(r, op, p1, p2, t) =>
            format!("{} = icmp {} {} {}, {}", string_of_register(*r), string_of_cmp_op(*op), string_of_type(t), string_of_result(p1), string_of_result(p2)),
        Instr::Xor(r, p1, p2) =>
            format!("{} = xor i1 {}, {}", string_of_register(*r), string_of_result(p1), string_of_result(p2)),
    }
}

fn print_block(label: Label, instructions: &[Instr]) {
    println!("{}:", string_of_label(label));
    for instr in instructions {
        println!("  {}", string_of_instr(instr));
    }
}

// The IType of the register an instruction defines, if any -- comparisons
// and Xor always produce i1 regardless of their own IType field (which
// describes their operands, not their result), Phi carries its own result
// type directly, and the arithmetic instructions are always i32.
fn defined_type(instr: &Instr) -> Option<(Register, IType)> {
    match instr {
        Instr::Addi32(r, ..) | Instr::Subi32(r, ..) | Instr::Muli32(r, ..) | Instr::Divi32(r, ..) =>
            Some((*r, IType::I32)),
        Instr::Xor(r, ..) | Instr::Cmp(r, ..) =>
            Some((*r, IType::I1)),
    }
}

// Finds the IType of a register by locating the instruction that defined
// it. Returns None for a register that isn't defined by any instruction in
// this unit -- e.g. a function parameter, which arrives via the calling
// convention rather than being computed by an Instr.
fn try_type_of_register(cu: &CompilationUnit, reg: Register) -> Option<IType> {
    cu.instructions.iter()
        .chain(cu.blocks.iter().flat_map(|b| &b.instructions))
        .find_map(|instr| defined_type(instr).filter(|(r, _)| *r == reg))
        .map(|(_, t)| t)
}

// Finds the IType of the compilation unit's result, by reading it straight
// off an IConst or, for a register, locating the instruction that defined
// it. None means the result register isn't defined by any instruction in
// this unit -- expected for in-progress codegen (e.g. a closure's result
// register that's allocated but not yet wired to an instruction).
fn try_type_of_result(cu: &CompilationUnit) -> Option<IType> {
    match &cu.result {
        LlvmResult::IConst(_, t) => Some(t.clone()),
        LlvmResult::Register(reg) => try_type_of_register(cu, *reg),
    }
}

// Prints every finished block followed by the still-open current block,
// without the top-level printf/ret trailer -- shared by the program's main
// body and by each nested function's body, since only main prints its
// result via printf.
fn print_body(cu: &CompilationUnit) {
    for block in &cu.blocks {
        print_block(block.label, &block.instructions);
    }
    print_block(cu.label, &cu.instructions);
}

/// Prints every struct and function the compilation collected, then the
/// main body: every finished block followed by the still-open current
/// block, then the final printf/ret pair that `llvm_generator` emits to
/// show the program's result instead of terminating silently.
pub fn print_compilation_unit(cu: &CompilationUnit) {
    
    println!("define i32 @main() {{");

    print_body(cu);

    let result = string_of_result(&cu.result);
    match try_type_of_result(cu) {
        Some(IType::I1) => {
            println!("  %widen = zext i1 {} to i32", result);
            println!("  call i32 (ptr, ...) @printf(ptr @fmt, i32 %widen)");
            println!("  ret i32 0");
        },
        Some(IType::I32) => {
            println!("  call i32 (ptr, ...) @printf(ptr @fmt, i32 {})", result);
            println!("  ret i32 0");
        },
        None => {
            println!("  ; result {} was never defined by an instruction in this unit", result);
            println!("  ret i32 0");
        },
        // _ => Err("print_compilation_unit: result type not implemented").unwrap(),
    }
    println!("}}");
}
