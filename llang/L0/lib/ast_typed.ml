(* This module contains the typed abstract syntax tree for the language *)

open Symbols
open Ast

type ast_typed = 
  | Num of int
  | Bool of bool
  | BinOp of Ast.bin_op * ast_typed * ast_typed * l_type
  | UnOp of Ast.un_op * ast_typed * l_type

let type_of = function
  | Num _ -> TInt
  | Bool _ -> TBool
  | BinOp (_, _, _, t) -> t
  | UnOp (_, _, t) -> t





