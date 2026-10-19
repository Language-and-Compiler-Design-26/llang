open Ast
open Symbols
open Environment
open Parser_utils

type value = 
  | Int of int
  | Bool of bool
  | Closure of symbol * ast * value env

let value_from_int n = Int n
let value_from_bool b = Bool b

let int_from_value = function 
  | Int n -> n
  | _ -> failwith "Expected an integer value"

let bool_from_value = function
  | Bool b -> b
  | _ -> failwith "Expected a boolean value"

let string_of_value = function
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Closure (id,e,_) -> "fun "^string_of_symbol id^" -> "^unparse_ast e


let native_eval_un_op op v =
  match op with
  | Neg -> value_from_int (- (int_from_value v))
  | Not -> value_from_bool (not (bool_from_value v))

let rec eval env = function
  | Num n -> value_from_int n

  | Bool b -> value_from_bool b

  | BinOp (op, e1, e2) -> native_eval_bin_op op (eval env e1) (eval env e2)

  | UnOp (op, e) -> native_eval_un_op op (eval env e)
  
    | If (cond, then_branch, else_branch) ->
      if bool_from_value (eval env cond) 
        then eval env then_branch else eval env else_branch
  
  | Let (id, e1, e2) ->
      let v1 = eval env e1 in 
      let new_env = env |>  begin_scope |> add_binding id v1 in
      eval new_env e2 (* no need for, but new_env |> end_scope *)

  | Id id -> Environment.lookup id env

  | Abs (id,_,e) -> Closure(id,e,env)

and native_eval_bin_op op v1 v2 =
  match op with
  | Add -> value_from_int (int_from_value v1 + int_from_value v2)
  | Sub -> value_from_int (int_from_value v1 - int_from_value v2)
  | Mul -> value_from_int (int_from_value v1 * int_from_value v2)
  | Div -> value_from_int (int_from_value v1 / int_from_value v2)
  | And -> value_from_bool (bool_from_value v1 && bool_from_value v2)
  | Or -> value_from_bool (bool_from_value v1 || bool_from_value v2)
  | Eq -> value_from_bool (v1 = v2)
  | Ne -> value_from_bool (v1 <> v2)
  | (Lt|Le|Gt|Ge) -> begin match v1, v2 with
    | Int n1, Int n2 -> (match op with
      | Lt -> value_from_bool (n1 < n2)
      | Le -> value_from_bool (n1 <= n2)
      | Gt -> value_from_bool (n1 > n2)
      | Ge -> value_from_bool (n1 >= n2)
      | _ -> assert false)
    | Bool b1, Bool b2 -> (match op with
      | Lt -> value_from_bool (b1 < b2)
      | Le -> value_from_bool (b1 <= b2)
      | Gt -> value_from_bool (b1 > b2)
      | Ge -> value_from_bool (b1 >= b2)
      | _ -> assert false)
    | _ -> failwith "Unsupported binary operator for non matching types"
  end
  | App -> begin match v1 with
    | Closure(id,e,env_closure) -> 
        let new_env = env_closure |> begin_scope |> add_binding id v2 in
        eval new_env e

    | _ -> failwith("Value not a function, cannot be called.")
  end

let eval = eval Environment.empty
