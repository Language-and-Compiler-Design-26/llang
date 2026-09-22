(* This module compiles the AST to LLVM IR *)

open Ast_typed
open Environment

type register = int

type label = int

let string_of_register n = "%"^string_of_int n

let string_of_label n = "label_"^string_of_int n

type i_type = 
  | I32
  | I1

type cmp_op = 
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

type result = 
  | IConst of int * i_type
  | Register of register

type llvm = 
  | Addi32 of register * result * result 
  | Subi32 of register * result * result 
  | Muli32 of register * result * result 
  | Divi32 of register * result * result 
  | Cmp of register * cmp_op * result * result * i_type

let llvm_type_of = function
  | Ast.TInt -> I32
  | Ast.TBool -> I1

let count = ref 0
let new_reg = fun () -> count := !count + 1; !count

let empty_basic_block = []

let build_op op ret r1 r2 t = 
  match op, t with 
  | Ast.Add, I32 -> Addi32 (ret,r1,r2)
  | Ast.Sub, I32 -> Subi32 (ret,r1,r2)
  | Ast.Mul, I32 -> Muli32 (ret,r1,r2)
  | Ast.Div, I32 -> Divi32 (ret,r1,r2) 
  | Ast.Eq, _ -> Cmp (ret,Eq,r1,r2,t)
  | Ast.Ne, _ -> Cmp (ret,Ne,r1,r2,t)
  | Ast.Lt, _ -> Cmp (ret,Lt,r1,r2,t)
  | Ast.Le, _ -> Cmp (ret,Le,r1,r2,t)
  | Ast.Gt, _ -> Cmp (ret,Gt,r1,r2,t)
  | Ast.Ge, _ -> Cmp (ret,Ge,r1,r2,t)
  | _ -> failwith "Unsupported binary operator in LLVM compilation"

let rec compile_program env l0 b0 = function 
  | Num x -> IConst (x,I32), l0, b0, []

  | Bool b -> IConst ((if b then 1 else 0), I1), l0, b0, []

  | BinOp (op, e1, e2, t) -> compile_binop env l0 b0 e1 e2 t op

  | UnOp (op, e, t) ->
    let r1, l1, b1, bs1 = compile_program env l0 b0 e in
    let r2 = 
      match op with 
      | Ast.Neg -> IConst (-1,I32) 
      | _ -> failwith "Unsupported unary operator in LLVM compilation"
    in
    let ret = new_reg() in
    (Register ret, l1, b1@[build_op Mul ret r1 r2 I32], bs1)



and 
  compile_binop env l0 b0 e1 e2 t = function
      | op -> 
          let r1,l1,b1,bs1 = compile_program env l0 b0 e1 in
          let r2,l2,b2,bs2 = compile_program env l1 b1 e2 in
          let ret = new_reg() in
          (Register ret, l2, b2@[build_op op ret r1 r2 (llvm_type_of t)], bs1@bs2)

let compile_program:(label -> llvm list -> Ast_typed.ast_typed -> result * label * llvm list * (label * llvm list) list) = compile_program empty
(* overrides the previous declaration*)