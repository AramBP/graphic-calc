open Core
open Lexer
open Lexing
open Eval
open Ast

let rec expr_to_string e env =
  match e with
  | Num x -> Float.to_string x
  | Var id -> 
      (match Env.find_opt id env.vars with
      | Some v -> Float.to_string v
      | None -> raise (UnboundVariable ("Unbound Variable : " ^ id)))
  | Binop (bop, e1, e2) -> (expr_to_string e1 env) ^ (bin_op_to_string bop) ^ (expr_to_string e2 env)
  | Unop (uop, e) -> (un_op_to_string uop) ^ (expr_to_string e env)
  | Call (id, expr_list) ->
      let expr_strings = List.mapi ~f:(fun i e ->
          (expr_to_string e env) ^ (if i > 0 then ", " else "")  
        ) expr_list
      in
      id ^ "(" ^ (String.concat ~sep:", " expr_strings) ^ ")" 

let line_to_string env ln =
  match ln with
  | Assign (id, e) -> id ^ " = " ^ expr_to_string e env
  | Expr e -> expr_to_string e env
  | FunDef (id, params, e) -> 
      id ^ "(" ^ (String.concat ~sep:", " params) ^ ")" ^ " = " ^ expr_to_string e env
  
let get_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.asprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Ok (Parser.prog Lexer.read lexbuf) with
  | SyntaxError msg -> 
      let err = Format.asprintf "syntax error: %s: %s\n" (get_position lexbuf) msg in
      Error err
  | Parser.Error -> 
      let err = Format.asprintf "%s: syntax error\n" (get_position lexbuf) in
      Error err

let eval_with_error env ln =
  try Ok (eval env ln) with
  | UnboundVariable msg -> 
      let err = Format.asprintf "error: %s\n" msg in
      Error err
  | DivisionByZero msg -> 
      let err = Format.asprintf "error: %s\n" msg in
      Error err
  | OperatorOperandMismatch msg -> 
      let err = Format.asprintf "error: %s\n" msg in
      Error err
  | UndefinedFunction msg -> 
      let err = Format.asprintf "error: %s\n" msg in
      Error err
  | WrongNumberOfArguments msg ->
      let err = Format.asprintf "error: %s\n" msg in
      Error err

let interp env input =
  let lexbuf = Lexing.from_string input in
  match parse_with_error lexbuf with
  | Ok (Some ln) -> (match eval_with_error env ln with
    | Ok (env', v_opt) -> (match v_opt with
      | Some v -> (Float.to_string v, env')
      | None -> ("", env')
    )
    | Error err -> (err, env)
  )
  | Error err -> (err, env)
  | _ -> (input, env)

