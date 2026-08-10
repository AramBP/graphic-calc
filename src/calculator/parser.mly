%{
let exprs_to_params exprs =
  List.map (fun e -> 
    match e with
    | Ast.Var id -> id
    | _ -> failwith "Invalid identifier"
  ) exprs
%}

%token <float> NUM
%token <string> ID
%token PLUS
%token TIMES
%token DIVISION
%token MINUS
%token POW 
%token LPAREN
%token RPAREN
%token EQUAL
%token PI
%token EULERNUM
%token COMMA
%token SIN
%token COS
%token TAN
%token EXP
%token LOG
%token EOF

%left PLUS MINUS
%left TIMES DIVISION
%nonassoc UMINUS
%right POW

%start <Ast.line option> prog

%%

prog: 
  | EOF           { None }
  | l=line ; EOF  { Some l } ;

line:
  | id=ID; EQUAL; e=expr                                  { Assign (id, e) }
  | id=ID; LPAREN; args=expr_list; RPAREN; EQUAL; e=expr  { FunDef (id, exprs_to_params args, e) }
  | e=expr                                                { Expr e } ;

expr:
  | LPAREN; e=expr; RPAREN                  { e }
  | x = NUM                                 { Num x }
  | EULERNUM                                { Num 2.71828 }                       
  | PI                                      { Num Float.pi }
  | id=ID; LPAREN; exprs=expr_list; RPAREN  { Call (id, exprs) }
  | id = ID                                 { Var id }             
  | e1=expr; PLUS; e2=expr                  { Binop (Add, e1, e2) }
  | e1=expr; MINUS; e2=expr                 { Binop (Sub, e1, e2) }
  | e1=expr; TIMES; e2=expr                 { Binop (Mult, e1, e2) }
  | e1=expr; DIVISION; e2=expr              { Binop (Div, e1, e2) }  
  | e1=expr; POW; e2=expr                   { Binop (Pow, e1, e2) }
  | e1=expr; LPAREN; e2=expr; RPAREN        { Binop (Mult, e1, e2) }
  | LPAREN; e1=expr; RPAREN; e2=expr        { Binop (Mult, e1, e2) }
  | x=NUM; e=expr                           { Binop (Mult, Num x, e) }
  | e=expr; x=NUM                           { Binop (Mult, e, Num x) }
  | MINUS; e=expr %prec UMINUS              { Unop (Neg, e) } 
  | SIN; LPAREN; e=expr; RPAREN             { Unop (Sin, e)}
  | COS; LPAREN; e=expr; RPAREN             { Unop (Cos, e)}
  | TAN; LPAREN; e=expr; RPAREN             { Unop (Tan, e)}
  | EXP; LPAREN; e=expr; RPAREN             { Unop (Exp, e)}
  | LOG; LPAREN; e=expr; RPAREN             { Unop (Log, e)} ;

expr_list : exprs = separated_list(COMMA, expr) { exprs } ;
