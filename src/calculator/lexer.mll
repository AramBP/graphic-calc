{
  open Lexing
  open Parser
  exception SyntaxError of string
}

let digit = ['0'-'9']
let int = digit digit*
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white   { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | int     { NUM (float_of_string (Lexing.lexeme lexbuf)) }
  | float   { NUM (float_of_string (Lexing.lexeme lexbuf)) }
  | '*'     { TIMES }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '/'     { DIVISION }
  | '^'     { POW }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '='     { EQUAL }
  | "pi"    { PI }
  | 'e'     { EULERNUM }
  | ','     { COMMA }
  | "sin"   { SIN }
  | "cos"   { COS }
  | "tan"   { TAN }
  | "exp"   { EXP }
  | "log"   { LOG }
  | id      { ID (Lexing.lexeme lexbuf) }
  | _       { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof     { EOF }
