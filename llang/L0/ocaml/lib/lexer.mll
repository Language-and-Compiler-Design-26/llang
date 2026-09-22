{
  open Parser
  exception Lexing_error of string
}

rule read = parse
  | [' ' '\t' '\r' '\n']     { read lexbuf }       (* skip whitespace *)
  | ['0'-'9']+ as i          { INT (int_of_string i) }
  | '+'                      { PLUS }
  | '-'                      { MINUS }
  | '*'                      { TIMES }
  | '/'                      { DIV }
  | '('                      { LPAREN }
  | ')'                      { RPAREN }

  | "true"                   { TRUE }
  | "false"                  { FALSE }
  | "&&"                     { AND }
  | "||"                     { OR }
  | "not "                   { NOT }

  | "if"                     { IF }
  | "then"                   { THEN }
  | "else"                   { ELSE }

  | "="                      { EQ }
  | "<"                      { LT }
  | ">"                      { GT }
  | "<="                     { LE }
  | ">="                     { GE }
  | "<>"                     { NE }

  | "let"                    { LET }
  | "in"                     { IN }

  | "fun"                    { FUN }
  | "->"                     { ARROW }

  | "int"                    { TINT }
  | "bool"                   { TBOOL }
  | ":"                      { COLON }

  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']* as id { ID id }

  | eof                      { EOF }
  | _ as c                   { raise (Lexing_error (Printf.sprintf "Unexpected char: %c" c)) }