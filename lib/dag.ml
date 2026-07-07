(** DAG IR for ToyC pure expressions — hash-consing, folding, flattening,
    algebraic simplification, strength reduction *)
open Ast

type id = int
type node = DConst of int | DVar of string | DBinop of binop * id * id

type node_key = KConst of int | KVar of string | KBinop of binop * id * id

let key_of_node n = match n with
  | DConst i -> KConst i | DVar s -> KVar s
  | DBinop(op,l,r) -> KBinop(op,l,r)

type t = {
  mutable nodes : node array;
  mutable count : int;
  hashcons : (node_key, id) Hashtbl.t;
}

let empty () = { nodes = Array.make 64 (DConst 0); count = 0;
                 hashcons = Hashtbl.create 256 }

let intern dag n =
  let key = key_of_node n in
  match Hashtbl.find_opt dag.hashcons key with
  | Some e -> e
  | None ->
    let fresh = dag.count in dag.count <- fresh + 1;
    if fresh >= Array.length dag.nodes then
      dag.nodes <- Array.append dag.nodes
        (Array.make (Array.length dag.nodes) (DConst 0));
    dag.nodes.(fresh) <- n; Hashtbl.add dag.hashcons key fresh; fresh

let get dag i = dag.nodes.(i)
let count dag = dag.count

(** Truncate OCaml int to 32-bit signed (C/RV32 semantics). *)
let wrap32 n =
  let n = n land 0xFFFFFFFF in
  if n >= 0x80000000 then n - 0x100000000 else n

(** C-style truncate-toward-zero division (OCaml / rounds toward -inf). *)
let c_div a b =
  let q = a / b in
  let r = a mod b in
  if r = 0 then q
  else if (a >= 0) = (b >= 0) then q
  else q + 1

(** C-style truncated remainder. *)
let c_mod a b =
  let r = a mod b in
  if r = 0 then 0
  else if (a >= 0) = (b >= 0) then r
  else r - b

(** Eval constant binop with 32-bit signed semantics. *)
let eval_const_binop op a b = match op with
  | Add -> wrap32 (a + b)
  | Sub -> wrap32 (a - b)
  | Mul -> wrap32 (a * b)
  | Div -> if b=0 then failwith "div0" else wrap32 (c_div a b)
  | Mod -> if b=0 then failwith "mod0" else wrap32 (c_mod a b)
  | Lt -> if a<b then 1 else 0 | Gt -> if a>b then 1 else 0
  | Le -> if a<=b then 1 else 0 | Ge -> if a>=b then 1 else 0
  | Eq -> if a=b then 1 else 0 | Ne -> if a<>b then 1 else 0
  | And -> (if a<>0 then 1 else 0) land (if b<>0 then 1 else 0)
  | Or -> if a<>0 || b<>0 then 1 else 0

(** Algebraic simplification: try to simplify a binop before creating node.
    Returns None if no simplification applies. *)
let try_simplify (dag : t) (op : binop) (l : id) (r : id) : id option =
  let is_const i n = match get dag i with DConst m when m = n -> true | _ -> false in
  let get_const i = match get dag i with DConst n -> Some n | _ -> None in
  (* Identity: x+0, 0+x, x-0, x*1, 1*x, x/1, x%1 → x *)
  (match op with
   | Add when is_const r 0 || is_const l 0 ->
     if is_const r 0 then Some l else Some r
   | Sub when is_const r 0 -> Some l
   | Mul ->
     if is_const r 0 || is_const l 0 then
       Some (intern dag (DConst 0))       (* x*0 → 0 *)
     else if is_const r 1 then Some l     (* x*1 → x *)
     else if is_const l 1 then Some r     (* 1*x → x *)
     else if is_const r 2 then            (* x*2 → x+x *)
       Some (intern dag (DBinop(Add,l,l)))
     else if is_const l 2 then            (* 2*x → x+x *)
       Some (intern dag (DBinop(Add,r,r)))
     else None
   | Div when is_const r 1 -> Some l      (* x/1 → x *)
   | Mod when is_const r 1 ->             (* x%1 → 0 *)
     Some (intern dag (DConst 0))
   (* Logical: x&&0→0, x||1→1 *)
   | And when is_const r 0 || is_const l 0 ->
     Some (intern dag (DConst 0))
   | And when is_const r 1 -> Some l      (* x&&1 → x (non-zero check) *)
   | Or when is_const r 1 || is_const l 1 ->
     Some (intern dag (DConst 1))
   | Or when is_const r 0 -> Some l       (* x||0 → x *)
   (* Eq/Ne with self → 1/0 *)
   | Eq when l = r -> Some (intern dag (DConst 1))
   | Ne when l = r -> Some (intern dag (DConst 0))
   (* Sub self → 0 *)
   | Sub when l = r -> Some (intern dag (DConst 0))
   (* Le/Ge self → 1 *)
   | Le when l = r -> Some (intern dag (DConst 1))
   | Ge when l = r -> Some (intern dag (DConst 1))
   (* Lt/Gt self → 0 *)
   | Lt when l = r -> Some (intern dag (DConst 0))
   | Gt when l = r -> Some (intern dag (DConst 0))
   (* +0/+1 patterns *)
   | Add ->
     (match get_const r with
      | Some n when n > 0 && n <= 8 ->
        (* Try to merge: (x+n)+m → x+(n+m) *)
        (match get dag l with
         | DBinop(Add, ll, lr) ->
           (match get_const lr with
            | Some m -> Some (intern dag (DBinop(Add, ll, intern dag (DConst (m+n)))))
            | None -> None)
         | _ -> None)
      | _ -> None)
   | _ -> None)

(** Convert ToyC pure expr to DAG with simplifications. *)
let rec of_pure_expr dag e = match e with
  | IntLit n -> intern dag (DConst n)
  | Var x -> intern dag (DVar x)
  | Unop (Neg, e1) ->
    let l = of_pure_expr dag e1 in
    intern dag (DBinop(Sub, intern dag (DConst 0), l))
  | Unop (Not, e1) ->
    let l = of_pure_expr dag e1 in
    intern dag (DBinop(Eq, l, intern dag (DConst 0)))
  | Unop (Pos, e1) -> of_pure_expr dag e1
  | Binop(op,e1,e2) ->
    let l = of_pure_expr dag e1 in
    let r = of_pure_expr dag e2 in
    (match try_simplify dag op l r with
     | Some id -> id
     | None -> intern dag (DBinop(op,l,r)))
  | _ -> failwith "DAG: non-pure expression"

(** Constant folding *)
let fold_constants old_dag =
  let nd = empty () in
  let m = Array.make old_dag.count 0 in
  for i = 0 to old_dag.count - 1 do
    m.(i) <- (match old_dag.nodes.(i) with
      | DConst n -> intern nd (DConst n)
      | DVar x -> intern nd (DVar x)
      | DBinop(op,l,r) ->
        let nl = m.(l) in let nr = m.(r) in
        (match try_simplify nd op nl nr with
         | Some id -> id
         | None ->
           match get nd nl, get nd nr with
           | DConst a, DConst b -> intern nd (DConst (eval_const_binop op a b))
           | _ -> intern nd (DBinop(op,nl,nr))))
  done; nd

(** Mark which DAG nodes are reachable from a set of root ids.
    Returns a set of live node ids. *)
let mark_live (dag : t) (roots : id list) : (id, unit) Hashtbl.t =
  let live = Hashtbl.create 64 in
  let rec visit id =
    if not (Hashtbl.mem live id) then begin
      Hashtbl.add live id ();
      match get dag id with
      | DBinop(_, l, r) -> visit l; visit r
      | _ -> ()
    end
  in
  List.iter visit roots;
  live

type tac = TConst of int*int | TBinop of int*binop*int*int | TCopy of int*int

(** Flatten single root *)
let flatten dag env root =
  let memo = Hashtbl.create 64 in
  let rec go nid = match Hashtbl.find_opt memo nid with
    | Some r -> ([], r)
    | None ->
      let instrs, r = match get dag nid with
        | DConst n -> let r = Util.fresh_reg() in ([TConst(r,n)], r)
        | DVar x -> (match List.assoc_opt x env with
                    | Some r -> ([], r)
                    | None -> failwith ("Unbound: "^x))
        | DBinop(op,l,r) ->
          let li,lr = go l in let ri,rr = go r in
          let r' = Util.fresh_reg() in
          (li @ ri @ [TBinop(r',op,lr,rr)], r')
      in Hashtbl.add memo nid r; (instrs, r)
  in go root

(** Flatten multiple roots with shared memo.
    Only nodes reachable from the roots are emitted (local DCE). *)
let flatten_roots dag roots =
  (* Compute live nodes from all roots *)
  let root_ids = List.map (fun (id,_,_) -> id) roots in
  let live = mark_live dag root_ids in
  let memo = Hashtbl.create 64 in
  let root_reg = Hashtbl.create 16 in
  List.iter (fun (id,r,_) -> Hashtbl.add root_reg id r) roots;
  let rec go nid env = match Hashtbl.find_opt memo nid with
    | Some r -> ([], r)
    | None ->
      (* Dead node: skip with a dummy reg *)
      if not (Hashtbl.mem live nid) then
        let dummy = Util.fresh_reg() in
        Hashtbl.add memo nid dummy; ([], dummy)
      else
      let instrs, r = match get dag nid with
        | DConst n ->
          let r = (match Hashtbl.find_opt root_reg nid with
                   | Some p -> p | None -> Util.fresh_reg()) in
          ([TConst(r,n)], r)
        | DVar x ->
          (match List.assoc_opt x env with
           | Some r -> Hashtbl.add memo nid r; ([], r)
           | None -> failwith ("Unbound: "^x))
        | DBinop(op,l,r) ->
          let li,lr = go l env in let ri,rr = go r env in
          let r' = (match Hashtbl.find_opt root_reg nid with
                    | Some p -> p | None -> Util.fresh_reg()) in
          (li @ ri @ [TBinop(r',op,lr,rr)], r')
      in Hashtbl.add memo nid r; (instrs, r)
  in
  List.concat_map (fun (rid, expected, env) ->
    let instrs, actual = go rid env in
    if actual = expected then instrs
    else instrs @ [TCopy(expected, actual)]) roots
