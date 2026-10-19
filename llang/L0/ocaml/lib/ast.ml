(* This module contains the abstract syntax tree for the language *)

open Symbols

type l_type = 
  | TInt
  | TBool
  | TArrow of l_type * l_type

type bin_op =   
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | App

type un_op = 
  | Neg
  | Not

type ast = 
  | Num of int
  | Bool of bool
  | BinOp of bin_op * ast * ast
  | UnOp of un_op * ast
  | If of ast * ast * ast
  | Let of symbol * ast * ast  
  | Id of symbol
  | Abs of symbol * l_type *ast



