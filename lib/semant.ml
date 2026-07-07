(** Semantic analysis for ToyC *)

open Ast

(* ------------------------------------------------------------------ *)
(*  Error reporting                                                   *)
(* ------------------------------------------------------------------ *)

let semant_error msg = failwith ("Semantic error: " ^ msg)

(* ------------------------------------------------------------------ *)
(*  Constant expression evaluation (compile-time)                     *)
(* ------------------------------------------------------------------ *)

let rec eval_const (st : Symbol.t) (e : expr) : int =
  match e with
  | IntLit n -> n
  | Var x ->
    (match Symbol.lookup st x with
     | Some (ConstVal n) -> n
     | Some _ -> semant_error ("non-const variable '" ^ x ^ "' in const expression")
     | None -> semant_error ("undefined variable '" ^ x ^ "'"))
  | Unop (Neg, e1) -> - (eval_const st e1)
  | Unop (Not, e1) -> if eval_const st e1 = 0 then 1 else 0
  | Unop (Pos, e1) -> eval_const st e1
  | Binop (op, e1, e2) ->
    let v1 = eval_const st e1 in
    let v2 = eval_const st e2 in
    eval_const_binop op v1 v2
  | _ -> semant_error "non-constant expression in const context"

and eval_const_binop (op : binop) (a : int) (b : int) : int =
  match op with
  | Add -> Dag.wrap32 (a + b)
  | Sub -> Dag.wrap32 (a - b)
  | Mul -> Dag.wrap32 (a * b)
  | Div -> if b = 0 then semant_error "division by zero" else Dag.wrap32 (Dag.c_div a b)
  | Mod -> if b = 0 then semant_error "modulo by zero" else Dag.wrap32 (Dag.c_mod a b)
  | Lt -> if a < b then 1 else 0
  | Gt -> if a > b then 1 else 0
  | Le -> if a <= b then 1 else 0
  | Ge -> if a >= b then 1 else 0
  | Eq -> if a = b then 1 else 0
  | Ne -> if a <> b then 1 else 0
  | And -> (if a <> 0 then 1 else 0) land (if b <> 0 then 1 else 0)
  | Or -> (if a <> 0 || b <> 0 then 1 else 0)

(** Check if an expression can be evaluated at compile time. *)
let rec is_constexpr (st : Symbol.t) (e : expr) : bool =
  match e with
  | IntLit _ -> true
  | Var x ->
    (match Symbol.lookup st x with Some (ConstVal _) -> true | _ -> false)
  | Unop (_, e1) -> is_constexpr st e1
  | Binop (_, e1, e2) -> is_constexpr st e1 && is_constexpr st e2
  | _ -> false

(* ------------------------------------------------------------------ *)
(*  L-value checking                                                   *)
(* ------------------------------------------------------------------ *)

let check_lvalue (st : Symbol.t) (name : string) : unit =
  match Symbol.lookup st name with
  | Some (Var Int) -> ()
  | Some (ConstVal _) -> semant_error ("cannot assign to const '" ^ name ^ "'")
  | Some (Func _) -> semant_error ("cannot assign to function '" ^ name ^ "'")
  | Some _ -> semant_error ("invalid lvalue '" ^ name ^ "'")
  | None -> semant_error ("undefined variable '" ^ name ^ "'")

(* ------------------------------------------------------------------ *)
(*  Type checking for expressions                                     *)
(* ------------------------------------------------------------------ *)

(** Returns the type of an expression. *)
let rec type_of (st : Symbol.t) (e : expr) : ty =
  match e with
  | IntLit _ -> Int
  | Var x ->
    (match Symbol.lookup st x with
     | Some (Var t) -> t
     | Some (ConstVal _) -> Int
     | Some (Func _) -> semant_error ("function '" ^ x ^ "' used as value")
     | None -> semant_error ("undefined variable '" ^ x ^ "'"))
  | Unop (_, e1) ->
    let _ = type_of st e1 in Int
  | Binop (And, e1, e2) | Binop (Or, e1, e2) ->
    let _ = type_of st e1 in let _ = type_of st e2 in Int
  | Binop (_, e1, e2) ->
    let _ = type_of st e1 in let _ = type_of st e2 in Int
  | Call (f, args) ->
    (match Symbol.lookup st f with
     | Some (Func (ret, params)) ->
       if List.length args <> List.length params then
         semant_error (Printf.sprintf "function '%s' expects %d arguments, got %d"
                         f (List.length params) (List.length args));
       List.iter (fun a -> let _ = type_of st a in ()) args;
       ret
     | Some _ -> semant_error ("'" ^ f ^ "' is not a function")
     | None -> semant_error ("undefined function '" ^ f ^ "'"))
  | Assign (x, e) ->
    check_lvalue st x;
    let _ = type_of st e in Int

(* ------------------------------------------------------------------ *)
(*  Check statements                                                  *)
(* ------------------------------------------------------------------ *)

(** Check a statement list for correctness.
    [has_return] tracks whether we've seen a return on all paths. *)
let rec check_stmts (st : Symbol.t) (ret_ty : ty) (ss : stmt list) : unit =
  List.iter (fun s -> check_stmt st ret_ty s) ss

and check_stmt (st : Symbol.t) (ret_ty : ty) (s : stmt) : unit =
  match s with
  | Block ss ->
    Symbol.push_scope st;
    check_stmts st ret_ty ss;
    Symbol.pop_scope st

  | Empty -> ()

  | ExprStmt e ->
    let _ = type_of st e in ()

  | VarDecl (x, e) ->
    if Symbol.lookup_current st x <> None then
      semant_error ("redeclaration of '" ^ x ^ "'");
    let _ = type_of st e in
    Symbol.add st x (Var Int)

  | ConstDecl (x, e) ->
    if Symbol.lookup_current st x <> None then
      semant_error ("redeclaration of '" ^ x ^ "'");
    if not (is_constexpr st e) then
      semant_error ("const '" ^ x ^ "' initializer is not a constant expression");
    let v = eval_const st e in
    Symbol.add st x (ConstVal v)

  | If (cond, then_s, else_s) ->
    let _ = type_of st cond in
    check_stmt st ret_ty then_s;
    Option.iter (fun es -> check_stmt st ret_ty es) else_s

  | While (cond, body) ->
    let _ = type_of st cond in
    Symbol.enter_loop st;
    check_stmt st ret_ty body;
    Symbol.leave_loop st

  | Break | Continue ->
    if not (Symbol.in_loop st) then
      semant_error "break/continue outside of loop"

  | Return e_opt ->
    (match ret_ty, e_opt with
     | Int, None -> semant_error "int function must return a value"
     | Void, Some _ -> semant_error "void function cannot return a value"
     | Int, Some e -> let _ = type_of st e in ()
     | Void, None -> ())

(* ------------------------------------------------------------------ *)
(*  Check top-level declarations                                      *)
(* ------------------------------------------------------------------ *)

let check (prog : program) : unit =
  let st = Symbol.create () in

  (* First pass: register all function signatures (allow forward refs) *)
  List.iter (function
    | GFuncDef fd ->
      if Symbol.lookup_current st fd.fname <> None then
        semant_error ("duplicate function '" ^ fd.fname ^ "'");
      Symbol.add st fd.fname (Func (fd.fty, fd.params))
    | GVarDecl (x, _) ->
      Symbol.add st x (Var Int)
    | GConstDecl (x, e) ->
      if not (is_constexpr st e) then
        semant_error ("global const '" ^ x ^ "' must be constant");
      let v = eval_const st e in
      Symbol.add st x (ConstVal v)
  ) prog;

  (* Second pass: check function bodies *)
  List.iter (function
    | GFuncDef fd ->
      Symbol.push_scope st;
      (* Add parameters to function scope *)
      List.iter (fun p ->
        Symbol.add st p.pname (Var Int)
      ) fd.params;
      check_stmts st fd.fty fd.body;
      Symbol.pop_scope st
    | GVarDecl (_, e) ->
      let _ = type_of st e in ()
    | GConstDecl _ -> ()
  ) prog;

  (* Ensure main function exists *)
  match Symbol.lookup st "main" with
  | Some (Func (Int, [])) -> ()
  | Some (Func (_, _)) ->
    semant_error "main must be 'int main()'"
  | _ -> semant_error "missing 'int main()' function"
