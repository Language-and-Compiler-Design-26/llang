use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::IntValue;
use inkwell::IntPredicate;
use inkwell::types::IntType;
use inkwell::AddressSpace;

use crate::codegen::{Block, CmpOp, CompilationUnit, IType, Instr, Label, LlvmResult, Register};
use crate::llvm_utils::string_of_label;

fn int_predicate_of(op: CmpOp) -> IntPredicate {
    match op {
        CmpOp::Eq => IntPredicate::EQ,
        CmpOp::Ne => IntPredicate::NE,
        CmpOp::Lt => IntPredicate::SLT,
        CmpOp::Le => IntPredicate::SLE,
        CmpOp::Gt => IntPredicate::SGT,
        CmpOp::Ge => IntPredicate::SGE,
    }
}

fn resolve<'ctx>(
    context: &'ctx Context,
    regs: &HashMap<Register, IntValue<'ctx>>,
    r: &LlvmResult,
) -> IntValue<'ctx> {
    match r {
        LlvmResult::IConst(n, IType::I32) => context.i32_type().const_int(*n as u64, true),
        LlvmResult::IConst(n, IType::I1) => context.bool_type().const_int(*n as u64, false),
        LlvmResult::Register(reg) => *regs
            .get(reg)
            .unwrap_or_else(|| panic!("register %{} used before it was defined", reg)),
        // _ => panic!("resolve: unexpected LlvmResult variant {:?}", r), 
    }
}

trait TypeOf {
    fn type_of(&self,t:&IType) -> IntType<'_>;
}
impl TypeOf for Context {
    fn type_of(&self,t:&IType) -> IntType<'_> {
        match t {
            IType::I1 => self.bool_type(),
            IType::I32 => self.i32_type(),
        }
    }
}

// Emits every finished block, then the still-open trailing block, and
// returns the resulting module. `context` is created by the caller and
// must outlive the returned `Module<'ctx>`.
pub fn emit_llvm_module<'ctx>(context: &'ctx Context, cu: &CompilationUnit) -> Module<'ctx> {
    let module = context.create_module("demo");
    let builder = context.create_builder();
    let i32_t = context.i32_type();

    let main_type = i32_t.fn_type(&[], false);
    let main_fn = module.add_function("main", main_type, None);

    // Create the basic blocks, the last one is in the instructions vector with label cu.label
    // When emiting jumps, we need the reference to the block in memory.
    let mut llvm_blocks: HashMap<Label, BasicBlock> = HashMap::new();
    for b in &cu.blocks {
        let bb = context.append_basic_block(main_fn, &string_of_label(b.label));
        llvm_blocks.insert(b.label, bb);
    }
    let tail_bb = context.append_basic_block(main_fn, &string_of_label(cu.label));
    llvm_blocks.insert(cu.label, tail_bb);

    // emit all blocks
    let mut regs: HashMap<Register, IntValue> = HashMap::new();
    for b in &cu.blocks {
        emit_block(&context, &builder, &llvm_blocks, &mut regs, b);
    }
    builder.position_at_end(llvm_blocks[&cu.label]);
    for instr in &cu.instructions {
        emit_instr(&context, &builder, &llvm_blocks, &mut regs, instr);
    }

    let ret = resolve(context, &regs, &cu.result);
    let ret_i32 = context.i32_type().const_int(0, true);
    print_result(context, &module, &builder, ret);
    builder.build_return(Some(&ret_i32)).unwrap();

    module
}

// Promotes an i1 result to i32; leaves an already-i32 result unchanged.
fn widen_to_i32<'ctx>(context: &'ctx Context, builder: &Builder<'ctx>, value: IntValue<'ctx>) -> IntValue<'ctx> {
    if value.get_type().get_bit_width() < 32 {
        builder.build_int_z_extend(value, context.i32_type(), "widen").unwrap()
    } else {
        value
    }
}

// Prints the program's result with printf before main returns, so running
// the compiled binary shows something instead of terminating silently.
fn print_result<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    result: IntValue<'ctx>,
) {
    let i32_t = context.i32_type();
    let ptr_t = context.ptr_type(AddressSpace::default());
    let printf_type = i32_t.fn_type(&[ptr_t.into()], true);
    let printf_fn = module
        .get_function("printf")
        .unwrap_or_else(|| module.add_function("printf", printf_type, None));

    let fmt = builder.build_global_string_ptr("%d\n", "fmt").unwrap();
    let arg = widen_to_i32(context, builder, result);

    builder
        .build_call(printf_fn, &[fmt.as_pointer_value().into(), arg.into()], "printf_call")
        .unwrap();
}

// Builds the module fresh and writes it as LLVM IR text to `filename`.
pub fn write_llvm_text_file(cu: &CompilationUnit, filename: &str) -> std::io::Result<()> {
    let context = Context::create();
    let module = emit_llvm_module(&context, cu);
    std::fs::write(filename, module.print_to_string().to_string())
}

// Builds the module fresh and writes its bitcode to `filename`.
pub fn write_llvm_bitcode_to_file(cu: &CompilationUnit, filename: &str) -> std::io::Result<()> {
    let context = Context::create();
    let module = emit_llvm_module(&context, cu);
    if module.write_bitcode_to_path(std::path::Path::new(filename)) {
        Ok(())
    } else {
        Err(std::io::Error::new(std::io::ErrorKind::Other, "Failed to export module's bytecode"))
    }
}

pub fn llvm_as_string(cu: &CompilationUnit) -> std::io::Result<String> {
    let context = Context::create();
    let module = emit_llvm_module(&context, cu);
    Ok(module.print_to_string().to_string())
}

fn emit_block<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    blocks: &HashMap<Label, BasicBlock<'ctx>>,
    regs: &mut HashMap<Register, IntValue<'ctx>>,
    b: &Block,
) {
    builder.position_at_end(blocks[&b.label]);
    for instr in &b.instructions {
        emit_instr(context, builder, blocks, regs, instr);
    }
}

fn emit_instr<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    _blocks: &HashMap<Label, BasicBlock<'ctx>>,
    regs: &mut HashMap<Register, IntValue<'ctx>>,
    instr: &Instr,
) {
    match instr {
        Instr::Addi32(ret, r1, r2) => {
            let lhs = resolve(context, regs, r1);
            let rhs = resolve(context, regs, r2);
            let result = builder.build_int_add(lhs, rhs, "addtmp").unwrap();
            regs.insert(*ret, result);
        },
        Instr::Subi32(ret, r1, r2) => {
            let lhs = resolve(context, regs, r1);
            let rhs = resolve(context, regs, r2);
            let result = builder.build_int_sub(lhs, rhs, "subtmp").unwrap();
            regs.insert(*ret, result);
        },
        Instr::Muli32(ret, r1, r2) => {
            let lhs = resolve(context, regs, r1);
            let rhs = resolve(context, regs, r2);
            let result = builder.build_int_mul(lhs, rhs, "multmp").unwrap();
            regs.insert(*ret, result);
        },
        Instr::Divi32(ret, r1, r2) => {
            let lhs = resolve(context, regs, r1);
            let rhs = resolve(context, regs, r2);
            let result = builder.build_int_signed_div(lhs, rhs, "divtmp").unwrap();
            regs.insert(*ret, result);
        },
        Instr::Xor(ret, r1, r2) => {
            let lhs = resolve(context, regs, r1);
            let rhs = resolve(context, regs, r2);
            let result = builder.build_xor(lhs, rhs, "xortmp").unwrap();
            regs.insert(*ret, result);
        },
        Instr::Cmp(ret, op, r1, r2, _t) => {
            let lhs = resolve(context, regs, r1);
            let rhs = resolve(context, regs, r2);
            let result = builder
                .build_int_compare(int_predicate_of(*op), lhs, rhs, "cmptmp")
                .unwrap();
            regs.insert(*ret, result);
        },
    }
}
