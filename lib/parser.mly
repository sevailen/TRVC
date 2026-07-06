%{
    open Ast
    let parse_error _ = failwith "Parse error"
    let () = ignore parse_error
%}

(* --- Tokens --- *)
%token INT VOID CONST
%token IF ELSE WHILE BREAK CONTINUE RETURN
%token PLUS MINUS STAR SLASH PERCENT
%token LT GT LE GE EQ NE AND OR NOT
%token ASSIGN
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA
%token <int> NUM
%token <string> ID
%token EOF

(* --- Precedence & associativity --- *)
%left OR
%left AND
%left EQ NE
%left LT GT LE GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%right NOT
%nonassoc NOELSE
%nonassoc ELSE

(* --- Start symbol --- *)
%start program
%type <Ast.program> program

%%

program:
  | top_decls EOF { $1 }
;

top_decls:
  | { [] }
  | top_decls top_decl { $1 @ [$2] }
;

top_decl:
  | const_decl { $1 }
  | var_decl { $1 }
  | func_def { GFuncDef $1 }
;

(* --- Declarations --- *)
const_decl:
  | CONST INT ID ASSIGN expr SEMICOLON { GConstDecl ($3, $5) }
;

var_decl:
  | INT ID ASSIGN expr SEMICOLON { GVarDecl ($2, $4) }
;

(* --- Function definition --- *)
func_def:
  | INT ID LPAREN params_opt RPAREN block
    { let body = match $6 with Block ss -> ss | _ -> [$6] in
      { fname = $2; fty = Int; params = $4; body } }
  | VOID ID LPAREN params_opt RPAREN block
    { let body = match $6 with Block ss -> ss | _ -> [$6] in
      { fname = $2; fty = Void; params = $4; body } }
;

params_opt:
  | { [] }
  | params { $1 }
;

params:
  | param { [$1] }
  | params COMMA param { $1 @ [$3] }
;

param:
  | INT ID { { pname = $2 } }
;

(* --- Statements --- *)
block:
  | LBRACE stmts RBRACE { Block $2 }
;

stmts:
  | { [] }
  | stmts stmt { $1 @ [$2] }
;

stmt:
  | block { $1 }
  | SEMICOLON { Empty }
  | expr SEMICOLON { ExprStmt $1 }
  | ID ASSIGN expr SEMICOLON { ExprStmt (Assign ($1, $3)) }
  | const_decl_no_global { $1 }
  | var_decl_no_global { $1 }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If ($3, $5, None) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If ($3, $5, Some $7) }
  | WHILE LPAREN expr RPAREN stmt { While ($3, $5) }
  | BREAK SEMICOLON { Break }
  | CONTINUE SEMICOLON { Continue }
  | RETURN SEMICOLON { Return None }
  | RETURN expr SEMICOLON { Return (Some $2) }
;

(* Local const/var decls — wrapped as stmt for use inside functions *)
const_decl_no_global:
  | CONST INT ID ASSIGN expr SEMICOLON { ConstDecl ($3, $5) }
;

var_decl_no_global:
  | INT ID ASSIGN expr SEMICOLON { VarDecl ($2, $4) }
;

(* --- Expressions --- *)
expr:
  | l_or_expr { $1 }
;

l_or_expr:
  | l_and_expr { $1 }
  | l_or_expr OR l_and_expr { Binop (Or, $1, $3) }
;

l_and_expr:
  | rel_expr { $1 }
  | l_and_expr AND rel_expr { Binop (And, $1, $3) }
;

rel_expr:
  | add_expr { $1 }
  | rel_expr LT add_expr { Binop (Lt, $1, $3) }
  | rel_expr GT add_expr { Binop (Gt, $1, $3) }
  | rel_expr LE add_expr { Binop (Le, $1, $3) }
  | rel_expr GE add_expr { Binop (Ge, $1, $3) }
  | rel_expr EQ add_expr { Binop (Eq, $1, $3) }
  | rel_expr NE add_expr { Binop (Ne, $1, $3) }
;

add_expr:
  | mul_expr { $1 }
  | add_expr PLUS mul_expr { Binop (Add, $1, $3) }
  | add_expr MINUS mul_expr { Binop (Sub, $1, $3) }
;

mul_expr:
  | unary_expr { $1 }
  | mul_expr STAR unary_expr { Binop (Mul, $1, $3) }
  | mul_expr SLASH unary_expr { Binop (Div, $1, $3) }
  | mul_expr PERCENT unary_expr { Binop (Mod, $1, $3) }
;

unary_expr:
  | primary_expr { $1 }
  | PLUS unary_expr { Unop (Pos, $2) }
  | MINUS unary_expr { Unop (Neg, $2) }
  | NOT unary_expr { Unop (Not, $2) }
;

primary_expr:
  | NUM { IntLit $1 }
  | ID { Var $1 }
  | LPAREN expr RPAREN { $2 }
  | ID LPAREN args_opt RPAREN { Call ($1, $3) }
;

args_opt:
  | { [] }
  | args { $1 }
;

args:
  | expr { [$1] }
  | args COMMA expr { $1 @ [$3] }
;
