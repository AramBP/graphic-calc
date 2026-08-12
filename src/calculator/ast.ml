type bin_op =
  | Add
  | Mult
  | Sub
  | Div
  | Pow

type un_op =
  | Neg
  | Sin
  | Cos
  | Tan
  | Exp
  | Log

type expr = 
  | Num of float
  | Var of string
  | Binop of bin_op * expr * expr
  | Unop of un_op * expr
  | Call of string * expr list

type line = 
  | Assign of string * expr
  | Expr of expr
  | FunDef of string * string list * expr option

let bin_op_to_string = function
  | Add -> "+"
  | Mult -> "*"
  | Sub -> "-"
  | Div -> "/"
  | Pow -> "^"

let un_op_to_string = function
  | Neg -> "-"
  | Sin -> "sin"
  | Cos -> "cos"
  | Tan -> "tan"
  | Exp -> "log"
  | Log -> "log"


