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
  | App, TArrow (t1, t2), t1' when t1 = t1' -> t2
  | App, _, _ -> failwith "Type error in function application"
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

  | Ast.If (cond, then_branch, else_branch) ->
      begin match type_of (type_synth env cond) with
      | TBool -> 
        begin match type_synth env then_branch, type_synth env else_branch with
        | t1, t2 when type_of t1 = type_of t2 -> Ast_typed.If (type_synth env cond, t1, t2, type_of t1)
        | _ -> failwith "Branches of if expression must have the same type"
        end
      | _ -> failwith "Condition in if expression must be a boolean"
      end

  | Ast.Let (id, e1, e2) ->
      let e1 = type_synth env e1 in
      let new_env = env |> begin_scope |> add_binding id (type_of e1) in
      let e2 = type_synth new_env e2 in
      Ast_typed.Let (id, e1, e2, type_of e2)

  | Ast.Id id -> Id(id, lookup id env)

  | Ast.Abs (id, t, e) -> 
      let new_env = env |> begin_scope |> add_binding id t in
      let e_typed = type_synth new_env e in
      Ast_typed.Abs (id, t, e_typed, TArrow (t, type_of e_typed))

let type_synth ast = type_synth empty ast (* overrides the previous definition *)