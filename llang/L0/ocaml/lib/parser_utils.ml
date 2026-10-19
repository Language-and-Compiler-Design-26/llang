(* This module contains the unparser functions for the language *)

open Symbols
open Ast

(* Parser utilities *)

let parse_lexbuf lb =
  try Parser.main Lexer.read lb with
  | Parsing.Parse_error ->
      let pos = lb.Lexing.lex_curr_p in
      let col = pos.pos_cnum - pos.pos_bol in
      failwith (Printf.sprintf "Parse error at line %d, column %d" pos.pos_lnum col)

let parse_string s =
  Lexing.from_string s |> parse_lexbuf

(* Unparser functions *)

let unparse_bin_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | And -> "&&"
  | Or -> "||"
  | Eq -> "="
  | Ne -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | App -> " "


let unparse_un_op = function
  | Neg -> "-"
  | Not -> "not "

let rec string_of_btype = function
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (t1,t2) -> string_of_btype t1 ^" -> "^ string_of_btype t2


let rec unparse_ast = function
  | Num x -> string_of_int x

  | Bool b -> string_of_bool b

  | BinOp (op, e1, e2) -> (unparse_ast e1 ^ " " ^ unparse_bin_op op ^ " " ^ unparse_ast e2)

  | UnOp (op, e1) -> (unparse_un_op op ^ unparse_ast e1)

  | If (cond, then_branch, else_branch) ->
      "if " ^ unparse_ast cond ^ " then " ^ unparse_ast then_branch ^ " else " ^ unparse_ast else_branch

  | Let (id, e1, e2) ->
      "let " ^ (string_of_symbol id) ^ " = " ^ unparse_ast e1 ^ " in " ^ unparse_ast e2

  | Id id -> string_of_symbol id

  | Abs (id, t, e1) -> "fun ("^string_of_symbol id^":"^string_of_btype t^ ") -> "^unparse_ast e1
