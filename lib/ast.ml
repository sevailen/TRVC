(** ToyC Abstract Syntax Tree *)

type unop = Neg | Not | Pos

type binop =
  | Add | Sub | Mul | Div | Mod
  | Lt | Gt | Le | Ge | Eq | Ne
  | And | Or

type ty = Int | Void

type expr =
  | IntLit of int
  | Var of string
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Call of string * expr list
  | Assign of string * expr

type stmt =
  | Block of stmt list
  | Empty
  | ExprStmt of expr
  | VarDecl of string * expr
  | ConstDecl of string * expr
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Break
  | Continue
  | Return of expr option

type param = { pname : string }

type func_def = {
  fname : string;
  fty : ty;
  params : param list;
  body : stmt list;
}

type top_decl =
  | GVarDecl of string * expr
  | GConstDecl of string * expr
  | GFuncDef of func_def

type program = top_decl list

(** String representations *)

let string_of_unop = function
  | Neg -> "-" | Not -> "!" | Pos -> "+"

let string_of_binop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">="
  | Eq -> "==" | Ne -> "!="
  | And -> "&&" | Or -> "||"

let string_of_ty = function Int -> "int" | Void -> "void"

let rec string_of_expr = function
  | IntLit n -> string_of_int n
  | Var x -> x
  | Unop (op, e) -> Printf.sprintf "(%s%s)" (string_of_unop op) (string_of_expr e)
  | Binop (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_binop op) (string_of_expr e2)
  | Call (f, args) ->
    Printf.sprintf "%s(%s)" f (String.concat ", " (List.map string_of_expr args))
  | Assign (x, e) -> Printf.sprintf "%s = %s" x (string_of_expr e)
