open Core
open Lexer
open Lexing
open Eval

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg -> fprintf stderr "syntax error: %a: %s\n" print_position lexbuf msg; None
  | Parser.Error -> fprintf stderr "%a: syntax error\n" print_position lexbuf; None

let eval_with_error env l =
  try Some (eval env l) with
  | UnboundVariable msg -> fprintf stderr "error: %s\n" msg; None
  | DivisionByZero msg -> fprintf stderr "error: %s\n" msg; None
  | OperatorOperandMismatch msg -> fprintf stderr "error: %s\n" msg; None
  | UndefinedFunction msg -> fprintf stderr "error: %s\n" msg; None
  | WrongNumberOfArguments msg -> fprintf stderr "error: %s\n" msg; None

