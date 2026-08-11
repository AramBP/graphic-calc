open Ast

exception UnboundVariable of string
exception DivisionByZero of string
exception OperatorOperandMismatch of string
exception UndefinedFunction of string
exception WrongNumberOfArguments of string

module Env = Map.Make(String)
type env_t = { vars : float Env.t ; funcs : (string list * expr) Env.t }

let is_num = function
  | Num _ -> true
  | Binop _ | Unop _ | Var _ | Call _ -> false

let get_vars e =
  let rec aux acc = function
  | Num _ -> acc
  | Var id -> id::acc
  | Binop (_, e1, e2) -> aux (aux acc e1) e2
  | Unop (_, e) -> aux acc e
  | Call (_, l) -> List.fold_left aux acc l 
  in
  List.sort_uniq String.compare (aux [] e)


let rec eval (env : env_t) (l : line) : (env_t * float option) = 
  match l with
  | Expr e -> (env, eval_expr env e)
  | Assign (id, e) -> eval_assign env id e
  | FunDef (id, params, e) -> 
      List.iter (fun var_id -> 
        if not (Env.mem var_id env.vars) && not (List.mem var_id params) 
        then raise (UnboundVariable ("Unbound Variable : " ^ var_id))) (get_vars e);
      ({ env with funcs = Env.add id (params, e) env.funcs}, None)

and eval_expr env e =
  match e with 
  | Num x -> Some x
  | Var id -> eval_var env id
  | Binop (bop, e1, e2) -> eval_bin_op env bop e1 e2
  | Unop (uop, e) -> eval_un_op env uop e
  | Call (id, expr_list) -> eval_call env id expr_list

and eval_var env id =
  match Env.find_opt id env.vars with
  | Some v -> Some v
  | None -> raise (UnboundVariable ("Unbound Variable : " ^ id))

and eval_bin_op env bop e1 e2 =
  match bop, (eval_expr env e1), (eval_expr env e2) with
  | _, _, None | _, None, _ -> 
      raise (OperatorOperandMismatch ("Operator and Operand mismatch"))
  | Add, Some x, Some y -> Some (x +. y)
  | Sub, Some x, Some y -> Some (x -. y)
  | Mult, Some x, Some y -> Some (x *. y)
  | Div, Some x, Some y -> 
      if Int.equal (Float.to_int y) 0 then raise (DivisionByZero ("Division By Zero"))
      else Some (x /. y)
  | Pow, Some x, Some y -> Some (Float.pow x y)

and eval_un_op env uop e =
  match uop, (eval_expr env e) with
  | _, None -> raise (OperatorOperandMismatch ("Operator and Operand mismatch"))
  | Neg, Some x -> Some (-1. *. x)
  | Sin, Some x -> Some (Float.sin x)
  | Cos, Some x -> Some (Float.cos x)
  | Tan, Some x -> Some (Float.tan x)
  | Exp, Some x -> Some (Float.exp x)
  | Log, Some x -> Some (Float.log x)

and eval_assign env id e = 
  let v = eval_expr env e in
  ({ env with vars = Env.add id (Option.get v) env.vars }, v)

and eval_call env id expr_list =
  let param_list, e = 
    match Env.find_opt id env.funcs with
    | Some x -> x
    | None -> raise (UndefinedFunction ("Function " ^ id ^ " is not defined"))
  in
  
  if List.(length param_list != length expr_list) then 
    raise (WrongNumberOfArguments ( "Function " ^ id ^ " accepts " 
    ^ Int.to_string (List.length param_list) 
    ^ " but is applied " ^ Int.to_string (List.length expr_list) ^ " arguments."));
  
  let vars = 
    List.fold_left2 
      (fun m id v -> Env.add id v m)
      env.vars
      param_list 
      (List.map (fun expr -> Option.get(eval_expr env expr)) expr_list)
  in
  
  eval_expr { env with vars = vars } e

