open Ast
open Ast_typed
open Environment

let type_bin_op op t1 t2 =
  match op, t1, t2 with
  | (Add | Sub | Mul | Div), TInt, TInt -> TInt
  | (And | Or), TBool, TBool -> TBool
  | (Eq | Ne), TInt, TInt -> TBool
  | (Eq | Ne), TBool, TBool -> TBool
  | (Lt | Le | Gt | Ge), TInt, TInt -> TBool
  | _ -> failwith "Type error in binary operation"

let type_un_op op t =
  match op, t with
  | Neg, TInt -> TInt
  | Not, TBool -> TBool
  | _ -> failwith "Type error in unary operation"

let rec type_synth env = function
  | Ast.Num n -> Ast_typed.Num n

  | Ast.Bool b -> Ast_typed.Bool b

  | Ast.BinOp (op, e1, e2) -> 
      let et1 = type_synth env e1 in 
      let et2 = type_synth env e2 in 
      Ast_typed.BinOp (op, et1, et2, type_bin_op op (type_of et1) (type_of et2))

  | Ast.UnOp (op, e) -> let e = type_synth env e in Ast_typed.UnOp (op, e, type_un_op op (type_of e))

let type_synth ast = type_synth empty ast (* overrides the previous definition *)