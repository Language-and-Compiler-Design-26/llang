open Compiler_llvm

(* How to transform LLVM to *)

let unparse_result = function 
  | IConst (x, _) -> string_of_int x
  | Register x -> string_of_register x

let string_of_op = function
  | Eq -> "eq"
  | Ne -> "ne"
  | Lt -> "slt"
  | Le -> "sle"
  | Gt -> "sgt"
  | Ge -> "sge"

let string_of_llvm_type = function
  | I32 -> "i32"
  | I1 -> "i1"

let unparse_llvm_i = function 
  | Addi32 (r,p1,p2) -> string_of_register r^" = add nsw i32 "^unparse_result p1^", "^unparse_result p2
  | Subi32 (r,p1,p2) -> string_of_register r^" = sub nsw i32 "^unparse_result p1^", "^unparse_result p2
  | Muli32 (r,p1,p2) -> string_of_register r^" = mul nsw i32 "^unparse_result p1^", "^unparse_result p2
  | Divi32 (r,p1,p2) -> string_of_register r^" = sdiv i32 "^unparse_result p1^", "^unparse_result p2
  | Cmp (r, op, p1, p2, t) ->
    string_of_register r^" = icmp "^string_of_op op^" "^string_of_llvm_type t ^" "^unparse_result p1^", "^unparse_result p2

let preamble = 
  "; ModuleID = 'llang.c'\n"^
  "source_filename = \"llang.c\"\n"^
  "target datalayout = \"e-m:o-i64:64-i128:128-n32:64-S128-Fn32\"\n"^
  "target triple = \"arm64-apple-macosx26.0.0\"\n\n"^
  "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n\n"^
  "; Function Attrs: noinline nounwind optnone ssp uwtable(sync)\n"^
  "define i32 @main() #0 {\n"

let epilogue ret t = 
  let final = new_reg () in 
  "  "^string_of_register final^" = call i32 (ptr, ...) @printf(ptr noundef @.str, "^string_of_llvm_type t^" noundef "^unparse_result ret^")"^
  "\n  ret i32 0\n"^
  "}\n\n"^
  "declare i32 @printf(ptr noundef, ...) #1\n"

let string_of_program ast = 
  let ret, label, instrs, blocks = compile_program 0 [] ast in
  let blocks_text = 
    List.fold_left (fun acc (l, b) -> 
      acc^string_of_label l^":\n"^
      (List.fold_left (fun acc i -> acc^"  "^unparse_llvm_i i^"\n") "" b)
    ) "" (blocks@[(label, instrs)])
  in
  preamble ^blocks_text^ epilogue ret (Ast_typed.type_of ast |> llvm_type_of)


let write_llvm_text_file ast filename =
  let text = string_of_program ast in
  let oc = open_out filename in
  output_string oc text;
  close_out oc
