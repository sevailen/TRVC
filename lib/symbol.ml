(** Symbol table with scope stack *)

open Ast

type symbol_kind =
  | Var of ty
  | ConstVal of int
  | Func of ty * param list

type scope = (string, symbol_kind) Hashtbl.t

type t = {
  mutable scopes : scope list;
  mutable loop_depth : int;
}

let create () : t =
  { scopes = [Hashtbl.create 32]; loop_depth = 0 }

let push_scope (st : t) : unit =
  st.scopes <- Hashtbl.create 16 :: st.scopes

let pop_scope (st : t) : unit =
  match st.scopes with
  | _ :: rest -> st.scopes <- rest
  | [] -> failwith "pop_scope: no scope to pop"

let add (st : t) (name : string) (kind : symbol_kind) : unit =
  match st.scopes with
  | cur :: _ -> Hashtbl.replace cur name kind
  | [] -> failwith "add: no scope"

let lookup (st : t) (name : string) : symbol_kind option =
  let rec search = function
    | [] -> None
    | scope :: rest ->
      match Hashtbl.find_opt scope name with
      | Some k -> Some k
      | None -> search rest
  in
  search st.scopes

let lookup_current (st : t) (name : string) : symbol_kind option =
  match st.scopes with
  | cur :: _ -> Hashtbl.find_opt cur name
  | [] -> None

let is_global (st : t) : bool =
  List.length st.scopes = 1

let enter_loop (st : t) : unit =
  st.loop_depth <- st.loop_depth + 1

let leave_loop (st : t) : unit =
  st.loop_depth <- st.loop_depth - 1

let in_loop (st : t) : bool =
  st.loop_depth > 0
