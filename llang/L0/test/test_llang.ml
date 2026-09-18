open OUnit2
open Llang.Ast
open Llang.Symbols
open Llang.Parser_utils
open Llang.Interpreter

(* Mainly AI generated tests *)

let test_parses_a_single_number _ =
  assert_equal (Num 3) (parse_string "3")

let test_parses_addition _ =
  assert_equal (BinOp (Add, Num 3, Num 4)) (parse_string "3 + 4")

let test_parses_precedence _ =
  assert_equal
    (BinOp (Add, Num 3, BinOp (Mul, Num 4, Num 2)))
    (parse_string "3 + 4 * 2")

let test_parses_parentheses _ =
  assert_equal
    (BinOp (Mul, BinOp (Add, Num 3, Num 4), Num 2))
    (parse_string "(3 + 4) * 2")

let test_parses_unary_minus _ =
  assert_equal (UnOp (Neg, Num 3)) (parse_string "-3")

let test_parses_subtraction_and_division _ =
  assert_equal
    (BinOp (Div, BinOp (Sub, Num 10, Num 4), Num 2))
    (parse_string "(10 - 4) / 2")

let test_unparses_back_to_a_readable_expression _ = (* The current unparsing function is not correct *)
  assert_equal "3 + 4 * (2 - 1)" (unparse_ast (parse_string "3 + 4 * (2 - 1)"))

let test_raises_on_invalid_syntax _ =
  assert_raises (Failure "Parse error at line 1, column 3") (fun () ->
      parse_string "3 +")

let test_evaluates_a_single_number _ =
  assert_equal (Int 3) (eval (Num 3))

let test_evaluates_addition _ =
  assert_equal (Int 7) (eval (parse_string "3 + 4"))

let test_evaluates_precedence _ =
  assert_equal (Int 11) (eval (parse_string "3 + 4 * 2"))

let test_evaluates_parentheses _ =
  assert_equal (Int 14) (eval (parse_string "(3 + 4) * 2"))

let test_evaluates_unary_minus _ =
  assert_equal (Int (-3)) (eval (parse_string "-3"))

let test_evaluates_subtraction_and_division _ =
  assert_equal (Int 3) (eval (parse_string "(10 - 4) / 2"))

let test_evaluates_nested_unary_minus _ =
  assert_equal (Int 3) (eval (parse_string "- -3"))

let test_evaluates_a_boolean_literal _ =
  assert_equal (Bool true) (eval (parse_string "true"))

let test_evaluates_logical_and _ =
  assert_equal (Bool false) (eval (parse_string "true && false"))

let test_evaluates_logical_or _ =
  assert_equal (Bool true) (eval (parse_string "false || true"))

let test_evaluates_logical_not _ =
  assert_equal (Bool false) (eval (parse_string "not true"))

let test_evaluates_an_if_expression _ =
  assert_equal (Int 3) (eval (parse_string "if true then 3 else 4"))

let test_parses_equality _ =
  assert_equal (BinOp (Eq, Num 3, Num 4)) (parse_string "3 = 4")

let test_parses_inequality _ =
  assert_equal (BinOp (Ne, Num 3, Num 4)) (parse_string "3 <> 4")

let test_parses_less_than _ =
  assert_equal (BinOp (Lt, Num 3, Num 4)) (parse_string "3 < 4")

let test_parses_less_than_or_equal _ =
  assert_equal (BinOp (Le, Num 3, Num 4)) (parse_string "3 <= 4")

let test_parses_greater_than _ =
  assert_equal (BinOp (Gt, Num 3, Num 4)) (parse_string "3 > 4")

let test_parses_greater_than_or_equal _ =
  assert_equal (BinOp (Ge, Num 3, Num 4)) (parse_string "3 >= 4")


let test_evaluates_equality_true _ =
  assert_equal (Bool true) (eval (parse_string "3 = 3"))

let test_evaluates_equality_false _ =
  assert_equal (Bool false) (eval (parse_string "3 = 4"))

let test_evaluates_inequality _ =
  assert_equal (Bool true) (eval (parse_string "3 <> 4"))

let test_evaluates_less_than _ =
  assert_equal (Bool true) (eval (parse_string "3 < 4"))

let test_evaluates_less_than_or_equal _ =
  assert_equal (Bool true) (eval (parse_string "4 <= 4"))

let test_evaluates_greater_than _ =
  assert_equal (Bool true) (eval (parse_string "5 > 4"))

let test_evaluates_greater_than_or_equal _ =
  assert_equal (Bool true) (eval (parse_string "4 >= 4"))

let test_evaluates_boolean_comparisons _ =
  assert_equal (Bool true) (eval (parse_string "false < true"))

let test_evaluates_comparison_in_condition _ =
  assert_equal (Int 1) (eval (parse_string "if 3 < 4 then 1 else 2"))

let test_evaluates_a_let_binding _ =
  assert_equal (Int 3) (eval (parse_string "let x = 3 in x"))

let test_evaluates_a_let_binding_used_in_expression _ =
  assert_equal (Int 8) (eval (parse_string "let x = 3 in x + 5"))

let test_evaluates_nested_let_bindings _ =
  assert_equal (Int 7) (eval (parse_string "let x = 3 in let y = 4 in x + y"))

let test_evaluates_shadowed_let_binding _ =
  assert_equal (Int 5) (eval (parse_string "let x = 3 in let x = 5 in x"))

let test_evaluates_let_binding_not_visible_outside_body _ =
  assert_equal (Int 8)
    (eval (parse_string "(let x = 3 in x) + (let x = 5 in x)"))

let test_evaluates_let_bound_value_in_if_condition _ =
  assert_equal (Int 1)
    (eval (parse_string "let x = true in if x then 1 else 2"))

let test_unparses_a_comparison _ =
  assert_equal "3 < 4" (unparse_ast (parse_string "3 < 4"))

let test_unparses_a_let_binding _ =
  assert_equal "let x = 3 in x" (unparse_ast (parse_string "let x = 3 in x"))

let contains_substring haystack needle =
  let hlen = String.length haystack and nlen = String.length needle in
  let rec go i =
    i + nlen <= hlen && (String.sub haystack i nlen = needle || go (i + 1))
  in
  nlen = 0 || go 0

let suite =
  "llang parser suite"
  >::: [
         "parses a single number" >:: test_parses_a_single_number;
         "parses addition" >:: test_parses_addition;
         "parses multiplication before addition (precedence)"
         >:: test_parses_precedence;
         "parses parentheses" >:: test_parses_parentheses;
         "parses unary minus" >:: test_parses_unary_minus;
         "parses subtraction and division"
         >:: test_parses_subtraction_and_division;
         "unparses back to a readable expression"
         >:: test_unparses_back_to_a_readable_expression;
         "raises on invalid syntax" >:: test_raises_on_invalid_syntax;
         "evaluates a single number" >:: test_evaluates_a_single_number;
         "evaluates addition" >:: test_evaluates_addition;
         "evaluates multiplication before addition (precedence)"
         >:: test_evaluates_precedence;
         "evaluates parentheses" >:: test_evaluates_parentheses;
         "evaluates unary minus" >:: test_evaluates_unary_minus;
         "evaluates subtraction and division"
         >:: test_evaluates_subtraction_and_division;
         "evaluates nested unary minus" >:: test_evaluates_nested_unary_minus;
         "evaluates a boolean literal" >:: test_evaluates_a_boolean_literal;
         "evaluates logical and" >:: test_evaluates_logical_and;
         "evaluates logical or" >:: test_evaluates_logical_or;
         "evaluates logical not" >:: test_evaluates_logical_not;
         "evaluates an if expression" >:: test_evaluates_an_if_expression;
         "parses equality" >:: test_parses_equality;
         "parses inequality" >:: test_parses_inequality;
         "parses less than" >:: test_parses_less_than;
         "parses less than or equal" >:: test_parses_less_than_or_equal;
         "parses greater than" >:: test_parses_greater_than;
         "parses greater than or equal" >:: test_parses_greater_than_or_equal;
         "evaluates equality (true)" >:: test_evaluates_equality_true;
         "evaluates equality (false)" >:: test_evaluates_equality_false;
         "evaluates inequality" >:: test_evaluates_inequality;
         "evaluates less than" >:: test_evaluates_less_than;
         "evaluates less than or equal" >:: test_evaluates_less_than_or_equal;
         "evaluates greater than" >:: test_evaluates_greater_than;
         "evaluates greater than or equal"
         >:: test_evaluates_greater_than_or_equal;
         "evaluates boolean comparisons" >:: test_evaluates_boolean_comparisons;
         "evaluates comparison in if condition"
         >:: test_evaluates_comparison_in_condition;
         "evaluates a let binding" >:: test_evaluates_a_let_binding;
         "evaluates a let binding used in expression"
         >:: test_evaluates_a_let_binding_used_in_expression;
         "evaluates nested let bindings" >:: test_evaluates_nested_let_bindings;
         "evaluates shadowed let binding"
         >:: test_evaluates_shadowed_let_binding;
         "evaluates let binding not visible outside body"
         >:: test_evaluates_let_binding_not_visible_outside_body;
         "evaluates let bound value in if condition"
         >:: test_evaluates_let_bound_value_in_if_condition;
         "unparses a comparison" >:: test_unparses_a_comparison;
         "unparses a let binding" >:: test_unparses_a_let_binding;
       ]

let () = run_test_tt_main suite
