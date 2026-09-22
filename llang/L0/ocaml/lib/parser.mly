%{
open Ast
open Symbols

(* To see parser errors, run with: ocamlyacc -v -b /tmp/parser_debug parser.mly so that files are generated to /tmp *)

%}

%token <int> INT
%token PLUS MINUS TIMES DIV LPAREN RPAREN EOF
%token TRUE FALSE AND OR NOT 
%token EQ NE LT LE GT GE
%token IF THEN ELSE
%token <string> ID
%token LET IN
%token FUN ARROW
%token TINT TBOOL COLON

%start main
%type <Ast.ast> main

%%
main:
  cond EOF                  { $1 }

cond:
  | disj                         { $1 }

disj:
  | disj OR conj                 { BinOp (Or, $1, $3) }
  | conj                         { $1 }

conj:
  | conj AND bterm          { BinOp (And, $1, $3) }
  | bterm                   { $1 }

bterm:
  | NOT bterm               { UnOp (Not, $2) }
  | comp                    { $1 }

comp:
  | comp EQ  expr           { BinOp (Eq, $1, $3) }
  | comp NE  expr           { BinOp (Ne, $1, $3) }
  | comp LT  expr           { BinOp (Lt, $1, $3) }
  | comp LE  expr           { BinOp (Le, $1, $3) }
  | comp GT  expr           { BinOp (Gt, $1, $3) }
  | comp GE  expr           { BinOp (Ge, $1, $3) }
  | expr                    { $1 }

expr:
  | expr PLUS  term         { BinOp (Add, $1, $3) }
  | expr MINUS term         { BinOp (Sub, $1, $3) }
  | term                    { $1 }

term:
  | term TIMES call         { BinOp (Mul, $1, $3) }
  | term DIV   call         { BinOp (Div, $1, $3) }
  | call                    { $1 }
;

call: 
  | MINUS factor            { UnOp (Neg, $2) }
  | factor                  { $1 }

factor:
  | INT                     { Num $1 }
  | TRUE                    { Bool true }
  | FALSE                   { Bool false }
  | LPAREN cond RPAREN      { $2 }

