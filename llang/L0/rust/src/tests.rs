
mod tests {
    use std::rc::Rc;
    use crate::ast::{self, Ast, Type, TypeData};
    use crate::grammar;
    use crate::interpreter::{Eval, Value};
    use crate::environment::Env;
    use crate::typing::TypeCheck;
    use crate::ast_typed::AstTypedData;
    use crate::codegen::{Compile, LlvmResult, Instr, IType};
    use inkwell::context::Context;

    fn parse(s: &str) -> Ast {
        grammar::CondParser::new().parse(s).unwrap()
    }

    fn type_check(s: &str) -> Result<Type, String> {
        parse(s).type_check(Rc::new(Env::new())).map(|t| t.type_of())
    }

    fn int_t() -> Type {
        Rc::new(TypeData::Int)
    }

    fn bool_t() -> Type {
        Rc::new(TypeData::Bool)
    }

    fn eval(s: &str) -> Result<Value, String> {
        parse(s).eval(Rc::new(Env::new()))
    }

    fn eval_int(s: &str) -> i64 {
        match eval(s).unwrap() {
            Value::Int(n) => n,
            v => panic!("expected Int, got {:?}", v),
        }
    }

    fn eval_bool(s: &str) -> bool {
        match eval(s).unwrap() {
            Value::Bool(b) => b,
            v => panic!("expected Bool, got {:?}", v),
        }
    }

    // ---- parsing ----

    #[test]
    fn parses_a_single_number() {
        assert_eq!(parse("3"), std::rc::Rc::new(ast::AstData::Num(3)));
    }

    #[test]
    fn parses_addition() {
        assert_eq!(
            parse("3 + 4"),
            std::rc::Rc::new(ast::AstData::BinOp(
                ast::BinOp::Add,
                std::rc::Rc::new(ast::AstData::Num(3)),
                std::rc::Rc::new(ast::AstData::Num(4))
            ))
        );
    }

    #[test]
    fn respects_precedence() {
        assert_eq!(eval_int("3 + 4 * 2"), 11);
    }

    #[test]
    fn respects_parentheses() {
        assert_eq!(eval_int("(3 + 4) * 2"), 14);
    }

    #[test]
    fn rejects_invalid_syntax() {
        assert!(grammar::CondParser::new().parse("3 +").is_err());
    }

    // ---- arithmetic operators ----

    #[test]
    fn evaluates_addition() {
        assert_eq!(eval_int("3 + 4"), 7);
    }

    #[test]
    fn evaluates_subtraction() {
        assert_eq!(eval_int("10 - 4"), 6);
    }

    #[test]
    fn evaluates_subtraction_and_division() {
        assert_eq!(eval_int("(10 - 4) / 2"), 3);
    }

    #[test]
    fn evaluates_multiplication() {
        assert_eq!(eval_int("6 * 7"), 42);
    }

    #[test]
    fn evaluates_division() {
        assert_eq!(eval_int("20 / 4"), 5);
    }

    #[test]
    fn integer_division_truncates() {
        assert_eq!(eval_int("7 / 2"), 3);
    }

    #[test]
    fn division_by_zero_errors() {
        let err = eval("5 / 0").unwrap_err();
        assert_eq!(err, "Division by zero");
    }

    #[test]
    fn evaluates_unary_negation() {
        assert_eq!(eval_int("-5"), -5);
        assert_eq!(eval_int("-(3 + 4)"), -7);
    }

    #[test]
    fn negation_on_non_integer_errors() {
        assert!(eval("- (1 < 2)").is_err());
    }

    // ---- comparison operators ----

    #[test]
    fn evaluates_lt() {
        assert_eq!(eval_bool("3 < 4"), true);
        assert_eq!(eval_bool("4 < 3"), false);
    }

    #[test]
    fn evaluates_gt() {
        assert_eq!(eval_bool("4 > 3"), true);
        assert_eq!(eval_bool("3 > 4"), false);
    }

    #[test]
    fn evaluates_le() {
        assert_eq!(eval_bool("3 <= 3"), true);
        assert_eq!(eval_bool("4 <= 3"), false);
    }

    #[test]
    fn evaluates_ge() {
        assert_eq!(eval_bool("3 >= 3"), true);
        assert_eq!(eval_bool("3 >= 4"), false);
    }

    #[test]
    fn evaluates_eq_on_integers() {
        assert_eq!(eval_bool("3 = 3"), true);
        assert_eq!(eval_bool("3 = 4"), false);
    }

    #[test]
    fn evaluates_ne_on_integers() {
        assert_eq!(eval_bool("3 <> 4"), true);
        assert_eq!(eval_bool("3 <> 3"), false);
    }

    #[test]
    fn evaluates_eq_on_booleans() {
        assert_eq!(eval_bool("(1 < 2) = (3 < 4)"), true);
        assert_eq!(eval_bool("(1 < 2) = (4 < 3)"), false);
    }

    #[test]
    fn comparison_on_non_integers_errors() {
        assert!(eval("(1 < 2) < (3 < 4)").is_err());
    }

    // ---- boolean / logical operators ----

    #[test]
    fn evaluates_not() {
        assert_eq!(eval_bool("not (1 < 2)"), false);
        assert_eq!(eval_bool("not (2 < 1)"), true);
    }

    #[test]
    fn not_on_non_boolean_errors() {
        assert!(eval("not 3").is_err());
    }

    // ---- type checking: literals & operators ----

    #[test]
    fn types_a_number_as_int() {
        assert_eq!(type_check("3").unwrap(), int_t());
    }

    #[test]
    fn types_a_boolean() {
        assert_eq!(type_check("true").unwrap(), bool_t());
        assert_eq!(type_check("false").unwrap(), bool_t());
    }

    #[test]
    fn types_arithmetic_as_int() {
        assert_eq!(type_check("3 + 4").unwrap(), int_t());
        assert_eq!(type_check("(10 - 4) / 2 * 6").unwrap(), int_t());
    }

    #[test]
    fn arithmetic_on_non_int_errors() {
        assert!(type_check("3 + true").is_err());
        assert!(type_check("(1 < 2) - 1").is_err());
    }

    #[test]
    fn types_negation_as_int() {
        assert_eq!(type_check("-5").unwrap(), int_t());
    }

    #[test]
    fn negation_on_non_int_errors() {
        assert!(type_check("- (1 < 2)").is_err());
    }

    #[test]
    fn types_comparisons_as_bool() {
        assert_eq!(type_check("3 < 4").unwrap(), bool_t());
        assert_eq!(type_check("3 = 4").unwrap(), bool_t());
        assert_eq!(type_check("(1 < 2) = (3 < 4)").unwrap(), bool_t());
    }

    #[test]
    fn comparison_between_mismatched_types_errors() {
        assert!(type_check("(1 < 2) < 3").is_err());
    }

    #[test]
    fn types_and_or_as_bool() {
        assert_eq!(type_check("(1 < 2) && (3 < 4)").unwrap(), bool_t());
        assert_eq!(type_check("(1 < 2) || (3 < 4)").unwrap(), bool_t());
    }

    #[test]
    fn and_or_on_non_bool_errors() {
        assert!(type_check("3 && (1 < 2)").is_err());
        assert!(type_check("(1 < 2) && 3").is_err());
        assert!(type_check("3 || true").is_err());
    }

    #[test]
    fn types_not_as_bool() {
        assert_eq!(type_check("not (1 < 2)").unwrap(), bool_t());
    }

    #[test]
    fn not_on_non_bool_errors() {
        assert!(type_check("not 3").is_err());
    }

    // ---- compilation ----

    fn compile_ir(s: &str) -> String {
        let typed = parse(s).type_check(Rc::new(Env::new())).unwrap();
        let cu = typed.compile().unwrap();
        let context = Context::create();
        let module = crate::llvm_generator::emit_llvm_module(&context, &cu);
        module.print_to_string().to_string()
    }

    #[test]
    fn compiles_addition_of_two_numbers() {
        let one = Rc::new(AstTypedData::Num(1));
        let two = Rc::new(AstTypedData::Num(2));
        let expr = Rc::new(AstTypedData::BinOp(ast::BinOp::Add, one, two, int_t()));

        let cu = expr.compile().unwrap();

        // addition doesn't branch, so it stays on the entry label/block
        assert_eq!(cu.label, 0);
        assert!(cu.blocks.is_empty());

        let reg = match cu.result {
            LlvmResult::Register(r) => r,
            other => panic!("expected a register result, got {:?}", other),
        };

        assert_eq!(
            cu.instructions,
            vec![Instr::Addi32(
                reg,
                LlvmResult::IConst(1, IType::I32),
                LlvmResult::IConst(2, IType::I32),
            )]
        );
    }

    #[test]
    fn emits_llvm_ir_for_addition() {
        let ir = compile_ir("1 + 2");

        assert!(ir.contains("define i32 @main()"), "{}", ir);
        // LLVM constant-folds `add` on two literal operands at build time,
        // so 1 + 2 shows up directly as the folded constant in the printf
        // call, not an `add` -- main itself always returns 0
        assert!(ir.contains("i32 3"), "{}", ir);
        assert!(ir.contains("ret i32 0"), "{}", ir);
    }

    // ---- printing the result ----

    #[test]
    fn prints_the_int_result_before_returning() {
        let ir = compile_ir("1 + 2");

        assert!(ir.contains("declare i32 @printf(ptr, ...)"), "{}", ir);
        // printed before main returns, though main always exits with 0
        assert!(ir.contains("call i32 (ptr, ...) @printf(ptr @fmt, i32 3)"), "{}", ir);
        assert!(ir.contains("ret i32 0"), "{}", ir);
    }

}
