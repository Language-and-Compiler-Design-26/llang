(* This module contains the abstract syntax tree for the language *)

open Symbols

type l_type = 
  | TInt
  | TBool

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

type un_op = 
  | Neg
  | Not

type ast = 
  | Num of int
  | Bool of bool
  | BinOp of bin_op * ast * ast
  | UnOp of un_op * ast



