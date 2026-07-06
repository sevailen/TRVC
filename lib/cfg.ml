(** ToyC CFG IR: basic blocks + three-address code, AST→CFG, optimizations *)

open Ast

type reg = int
type label = string

(* --- TAC instructions --- *)
type tac =
  | TConst of reg * int
  | TBinop of reg * binop * reg * reg
  | TUnop of reg * unop * reg
  | TCopy of reg * reg
  | TLoad of reg * reg * int
  | TStore of reg * int * reg
  | TLa of reg * string
  | TCall of reg * string * reg list
  | TCallVoid of string * reg list

type terminator =
  | TReturn of reg option
  | TJump of label
  | TBranch of reg * label * label

type block = { label : label; body : tac list; term : terminator }
type func_cfg = { name : string; ret_ty : ty; entry : label;
                  blocks : block list; num_slots : int; num_params : int }
type program_cfg = { functions : func_cfg list; globals : (string * int) list }

type block_segment =
  | PureSeg of { reg : reg; expr : Ast.expr; env : (string * reg) list }
  | TacSeg of tac

type loop_ctx = { break_lbl : label; continue_lbl : label }

(* fp-relative offset of local/param slot k (locals start right below saved ra/fp) *)
let slot_off (k : int) : int = -(12 + 4 * k)

(* ================================================================= *)
(*  DAG processing (must come before builder that calls them)        *)
(* ================================================================= *)

let of_dag_tac (dt : Dag.tac) : tac = match dt with
  | Dag.TConst(r,n) -> TConst(r,n)
  | Dag.TBinop(r,op,a,b) -> TBinop(r,op,a,b)
  | Dag.TCopy(r,rs) -> TCopy(r,rs)

let env_subset sub sup =
  List.for_all (fun (x,r) ->
    match List.assoc_opt x sup with Some r' -> r=r' | None -> false) sub

let process_one_group (items : (reg * expr * (string * reg) list) list) : tac list =
  if items = [] then [] else
  let dag = Dag.empty () in
  let roots = List.map (fun (reg,e,env) ->
    (Dag.of_pure_expr dag e, reg, env)) items in
  let mapping = Array.make (Dag.count dag) 0 in
  let folded = Dag.empty () in
  for i = 0 to Dag.count dag - 1 do
    mapping.(i) <- (match Dag.get dag i with
      | Dag.DConst n -> Dag.intern folded (Dag.DConst n)
      | Dag.DVar x -> Dag.intern folded (Dag.DVar x)
      | Dag.DBinop (op,l,r) ->
        let nl = mapping.(l) in let nr = mapping.(r) in
        (match Dag.get folded nl, Dag.get folded nr with
         | Dag.DConst a, Dag.DConst b ->
           let v = Dag.eval_const_binop op a b in
           Dag.intern folded (Dag.DConst v)
         | _ -> Dag.intern folded (Dag.DBinop(op,nl,nr))))
  done;
  let folded_roots = List.map (fun (id,reg,env) -> (mapping.(id), reg, env)) roots in
  List.map of_dag_tac (Dag.flatten_roots folded folded_roots)

let process_segments segments =
  let rec go acc cur_env cur_pures = function
    | [] -> let tail = process_one_group (List.rev cur_pures) in List.rev_append acc tail
    | TacSeg t :: rest ->
      let acc' = if cur_pures=[] then acc
        else List.rev_append (process_one_group (List.rev cur_pures)) acc in
      go (t::acc') cur_env [] rest
    | PureSeg {reg;expr;env} :: rest ->
      if cur_env=[] || env_subset cur_env env
      then go acc (if cur_env=[] then env else cur_env) ((reg,expr,env)::cur_pures) rest
      else
        let acc' = if cur_pures=[] then acc
          else List.rev_append (process_one_group (List.rev cur_pures)) acc in
        go acc' env [(reg,expr,env)] rest
  in go [] [] [] segments

(* ================================================================= *)
(*  Builder state                                                    *)
(* ================================================================= *)

(* Variable binding kinds resolved by the CFG builder *)
type binding =
  | Slot of int        (* local var / param: fp-relative stack slot index *)
  | ConstB of int      (* compile-time constant value *)
  | GlobalB of string  (* global variable: addressed by symbol name *)

type builder_state = {
  mutable cur_label : label;
  mutable cur_body : block_segment list;
  mutable blocks : block list;
  mutable block_open : bool;                (* is cur block awaiting a terminator? *)
  mutable functions : func_cfg list;
  mutable loop_stack : loop_ctx list;
  mutable scopes : (string, binding) Hashtbl.t list;  (* innermost first *)
  mutable next_slot : int;
  mutable max_slots : int;
}

let fresh_label (_st : builder_state) = Util.fresh_label "L"

(* --- scope management --- *)
let push_scope (st : builder_state) = st.scopes <- Hashtbl.create 16 :: st.scopes
let pop_scope (st : builder_state) =
  match st.scopes with _ :: rest -> st.scopes <- rest | [] -> ()
let bind (st : builder_state) name b =
  match st.scopes with cur :: _ -> Hashtbl.replace cur name b | [] -> ()
let rec resolve_in name = function
  | [] -> None
  | sc :: rest -> (match Hashtbl.find_opt sc name with Some b -> Some b | None -> resolve_in name rest)
let resolve (st : builder_state) name = resolve_in name st.scopes

let alloc_stack_slot (st : builder_state) (name : string) : int =
  let slot = st.next_slot in
  st.next_slot <- slot + 1;
  if st.next_slot > st.max_slots then st.max_slots <- st.next_slot;
  bind st name (Slot slot); slot

(* --- block emission --- *)
let ensure_open (st : builder_state) =
  if not st.block_open then begin
    st.cur_label <- fresh_label st; st.cur_body <- []; st.block_open <- true
  end

let emit_tac st t = ensure_open st; st.cur_body <- st.cur_body @ [TacSeg t]

let emit_pure (st : builder_state) (env : (string * reg) list) (e : expr) : reg =
  ensure_open st;
  let r = Util.fresh_reg () in
  st.cur_body <- st.cur_body @ [PureSeg {reg=r; expr=e; env}]; r

let finish_block (st : builder_state) (term : terminator) : unit =
  if st.block_open then begin
    let body = process_segments st.cur_body in
    st.blocks <- {label=st.cur_label; body; term} :: st.blocks;
    st.cur_body <- []; st.block_open <- false
  end

let start_new_block (st : builder_state) (lbl : label) : unit =
  st.cur_label <- lbl; st.cur_body <- []; st.block_open <- true

(* ================================================================= *)
(*  Compile-time constant evaluation (local consts, over builder scope) *)
(* ================================================================= *)

let rec eval_const_b (st : builder_state) (e : expr) : int =
  match e with
  | IntLit n -> n
  | Var x ->
    (match resolve st x with
     | Some (ConstB n) -> n
     | _ -> failwith ("Semantic error: undefined variable '" ^ x ^ "'"))
  | Unop (Neg, e1) -> - (eval_const_b st e1)
  | Unop (Not, e1) -> if eval_const_b st e1 = 0 then 1 else 0
  | Unop (Pos, e1) -> eval_const_b st e1
  | Binop (op, e1, e2) -> Dag.eval_const_binop op (eval_const_b st e1) (eval_const_b st e2)
  | _ -> failwith "non-constant expression in const context"

(* ================================================================= *)
(*  Named-value load/store (slot / const / global) — all expr paths   *)
(* ================================================================= *)

let load_named (st : builder_state) (name : string) : reg =
  match resolve st name with
  | Some (Slot k) ->
    let r = Util.fresh_reg () in emit_tac st (TLoad (r, 8, slot_off k)); r
  | Some (ConstB n) ->
    let r = Util.fresh_reg () in emit_tac st (TConst (r, n)); r
  | Some (GlobalB g) ->
    let r = Util.fresh_reg () in
    emit_tac st (TLa (r, g)); emit_tac st (TLoad (r, r, 0)); r
  | None -> failwith ("no stack slot for " ^ name)

let store_named (st : builder_state) (name : string) (r : reg) : unit =
  match resolve st name with
  | Some (Slot k) -> emit_tac st (TStore (8, slot_off k, r))
  | Some (GlobalB g) ->
    let tmp = Util.fresh_reg () in
    emit_tac st (TLa (tmp, g)); emit_tac st (TStore (tmp, 0, r))
  | Some (ConstB _) -> failwith ("cannot assign to const '" ^ name ^ "'")
  | None -> failwith ("no stack slot for " ^ name)

(* ================================================================= *)
(*  Expression builder                                               *)
(* ================================================================= *)

(** Check if expression is pure: no calls AND no short-circuit ops.
    &&/|| must never be folded into the DAG or they lose short-circuit semantics
    (e.g. `x!=0 && 100/x>5` would divide by zero when x=0). *)
let rec is_pure = function
  | Call _ | Assign _ -> false
  | Binop (And, _, _) | Binop (Or, _, _) -> false
  | Binop (_, e1, e2) -> is_pure e1 && is_pure e2
  | Unop (_, e1) -> is_pure e1
  | _ -> true

(* Collect variable names referenced in a pure expr and preload each into a reg. *)
let preload_vars (st : builder_state) (e : expr) : (string * reg) list =
  let names = ref [] in
  let rec find = function
    | Var x -> if not (List.mem x !names) then names := x :: !names
    | Unop (_, e1) -> find e1
    | Binop (_, e1, e2) -> find e1; find e2
    | _ -> () in
  find e;
  List.fold_left (fun acc v ->
    if List.mem_assoc v acc then acc else (v, load_named st v) :: acc) [] !names

let rec build_expr (st : builder_state) (e : expr) : reg =
  match e with
  | IntLit _ -> emit_pure st [] e
  | Var x -> load_named st x
  | Unop (op, e1) ->
    if is_pure e then
      let env = preload_vars st e in emit_pure st env e
    else begin
      let r1 = build_expr st e1 in
      let r = Util.fresh_reg () in
      emit_tac st (TUnop (r, op, r1)); r
    end
  | Binop (And, e1, e2) ->
    let r1 = build_expr st e1 in
    let r = Util.fresh_reg () in
    let l_e2 = fresh_label st in
    let l_false = fresh_label st in
    let l_merge = fresh_label st in
    finish_block st (TBranch (r1, l_e2, l_false));
    (* evaluate e2 *)
    start_new_block st l_e2;
    let r2 = build_expr st e2 in
    let z = Util.fresh_reg () in
    emit_tac st (TConst (z, 0));
    emit_tac st (TBinop (r, Ne, r2, z));
    finish_block st (TJump l_merge);
    (* short-circuit false *)
    start_new_block st l_false;
    emit_tac st (TConst (r, 0));
    finish_block st (TJump l_merge);
    start_new_block st l_merge; r
  | Binop (Or, e1, e2) ->
    let r1 = build_expr st e1 in
    let r = Util.fresh_reg () in
    let l_e2 = fresh_label st in
    let l_true = fresh_label st in
    let l_merge = fresh_label st in
    finish_block st (TBranch (r1, l_true, l_e2));
    (* evaluate e2 *)
    start_new_block st l_e2;
    let r2 = build_expr st e2 in
    let z = Util.fresh_reg () in
    emit_tac st (TConst (z, 0));
    emit_tac st (TBinop (r, Ne, r2, z));
    finish_block st (TJump l_merge);
    (* short-circuit true *)
    start_new_block st l_true;
    emit_tac st (TConst (r, 1));
    finish_block st (TJump l_merge);
    start_new_block st l_merge; r
  | Binop (op, e1, e2) ->
    if not (is_pure e1) || not (is_pure e2) then begin
      let r1 = build_expr st e1 in
      let r2 = build_expr st e2 in
      let r = Util.fresh_reg () in
      emit_tac st (TBinop (r, op, r1, r2)); r
    end else begin
      let env = preload_vars st e in emit_pure st env e
    end
  | Call (f, args) ->
    let arg_regs = List.map (build_expr st) args in
    let r = Util.fresh_reg () in
    emit_tac st (TCall (r, f, arg_regs)); r
  | Assign _ -> failwith "assign in expr not supported"

(* ================================================================= *)
(*  Statement builder                                                *)
(* ================================================================= *)

let rec build_stmt (st : builder_state) (s : stmt) : unit =
  match s with
  | Block ss ->
    push_scope st;
    let saved = st.next_slot in
    List.iter (build_stmt st) ss;
    st.next_slot <- saved;      (* reclaim block-local slots on scope exit *)
    pop_scope st

  | Empty -> ()

  | ExprStmt (Assign (x, e)) ->
    let r = build_expr st e in
    store_named st x r

  | ExprStmt (Call (f, args)) ->
    let arg_regs = List.map (build_expr st) args in
    emit_tac st (TCallVoid (f, arg_regs))

  | ExprStmt e ->
    let _ = build_expr st e in ()

  | VarDecl (x, e) ->
    let r = build_expr st e in
    let slot = alloc_stack_slot st x in
    emit_tac st (TStore (8, slot_off slot, r))

  | ConstDecl (x, e) ->
    let v = eval_const_b st e in
    bind st x (ConstB v)

  | If (cond, then_s, else_s) ->
    let rc = build_expr st cond in
    let then_lbl = fresh_label st in
    let else_lbl = fresh_label st in
    let end_lbl = fresh_label st in
    finish_block st (TBranch (rc, then_lbl, else_lbl));
    start_new_block st then_lbl;
    build_stmt st then_s;
    if st.block_open then finish_block st (TJump end_lbl);
    start_new_block st else_lbl;
    Option.iter (build_stmt st) else_s;
    if st.block_open then finish_block st (TJump end_lbl);
    start_new_block st end_lbl

  | While (cond, body) ->
    let cond_lbl = fresh_label st in
    let body_lbl = fresh_label st in
    let end_lbl = fresh_label st in
    finish_block st (TJump cond_lbl);
    start_new_block st cond_lbl;
    let rc = build_expr st cond in
    finish_block st (TBranch (rc, body_lbl, end_lbl));
    start_new_block st body_lbl;
    st.loop_stack <- {break_lbl=end_lbl; continue_lbl=cond_lbl} :: st.loop_stack;
    build_stmt st body;
    st.loop_stack <- List.tl st.loop_stack;
    if st.block_open then finish_block st (TJump cond_lbl);
    start_new_block st end_lbl

  | Break ->
    (match st.loop_stack with
     | ctx :: _ -> finish_block st (TJump ctx.break_lbl)
     | [] -> failwith "break outside loop")

  | Continue ->
    (match st.loop_stack with
     | ctx :: _ -> finish_block st (TJump ctx.continue_lbl)
     | [] -> failwith "continue outside loop")

  | Return e_opt ->
    let r_opt = Option.map (build_expr st) e_opt in
    finish_block st (TReturn r_opt)

(* ================================================================= *)
(*  Build whole program                                              *)
(* ================================================================= *)

let build_program (prog : Ast.program) : program_cfg =
  Util.reset_reg (); Util.reset_label ();
  let global_scope = Hashtbl.create 32 in
  let st = {
    cur_label = "entry"; cur_body = []; blocks = []; block_open = false;
    functions = []; loop_stack = [];
    scopes = [global_scope];
    next_slot = 0; max_slots = 0;
  } in
  let globals = ref [] in

  (* Pre-register globals in source order so const chains resolve. *)
  List.iter (function
    | GVarDecl (x, e) ->
      let v = (try eval_const_b st e with _ -> 0) in
      Hashtbl.replace global_scope x (GlobalB x);
      globals := (x, v) :: !globals
    | GConstDecl (x, e) ->
      let v = eval_const_b st e in
      Hashtbl.replace global_scope x (ConstB v)
    | GFuncDef _ -> ()
  ) prog;

  (* Build each function *)
  List.iter (function
    | GFuncDef fd ->
      st.cur_body <- []; st.blocks <- [];
      st.cur_label <- fd.fname ^ "_entry";
      st.block_open <- true;
      st.next_slot <- 0; st.max_slots <- 0;
      st.loop_stack <- [];
      push_scope st;
      (* params occupy the first slots, in order; codegen saves a0..a7 there *)
      List.iter (fun p -> let _ = alloc_stack_slot st p.pname in ()) fd.params;
      List.iter (build_stmt st) fd.body;
      (* close any open trailing block (void fallthrough / unreachable merge) *)
      if st.block_open then finish_block st (TReturn None);
      pop_scope st;
      st.functions <- {name=fd.fname; ret_ty=fd.fty;
                       entry=fd.fname ^ "_entry";
                       blocks=List.rev st.blocks;
                       num_slots=st.max_slots;
                       num_params=List.length fd.params} :: st.functions
    | GVarDecl _ | GConstDecl _ -> ()
  ) prog;

  { functions = List.rev st.functions; globals = List.rev !globals }

(* ================================================================= *)
(*  Optimizations                                                    *)
(* ================================================================= *)

let norm l = List.sort_uniq compare l

let term_live_out live_in_map term = match term with
  | TReturn (Some r) -> [r]
  | TReturn None -> []
  | TBranch (r, l1, l2) ->
    let l1in = try Hashtbl.find live_in_map l1 with _ -> [] in
    let l2in = try Hashtbl.find live_in_map l2 with _ -> [] in
    norm (r :: (l1in @ l2in))
  | TJump lbl -> (try Hashtbl.find live_in_map lbl with _ -> [])

let dce_block blk live_out =
  let live = ref live_out in let kept = ref [] in
  let add r = if not (List.mem r !live) then live := r :: !live in
  List.iter (fun instr -> match instr with
    | TConst(r,_) | TUnop(r,_,_) ->
      if List.mem r !live then (kept:=instr::!kept; live:=List.filter((<>)r)!live)
    | TBinop(r,_,r1,r2) ->
      if List.mem r !live then
        (kept:=instr::!kept; live:=List.filter((<>)r)!live; add r1; add r2)
    | TCopy(r,rs) ->
      if List.mem r !live then (kept:=instr::!kept; live:=List.filter((<>)r)!live; add rs)
    | TLoad(r,rb,_) ->
      if List.mem r !live then (kept:=instr::!kept; live:=List.filter((<>)r)!live; add rb)
    | TStore(rb,_,rs) -> kept:=instr::!kept; add rb; add rs
    | TCall(r,_,args) ->
      kept:=instr::!kept; live:=List.filter((<>)r)!live; List.iter add args
    | TCallVoid(_,args) -> kept:=instr::!kept; List.iter add args
    | TLa(r,_) ->
      if List.mem r !live then (kept:=instr::!kept; live:=List.filter((<>)r)!live)
  ) (List.rev blk.body);
  ({ blk with body = !kept }, norm !live)

let compute_live_in blk live_out = snd (dce_block blk live_out)

let compute_liveness blocks =
  let live_in_map = Hashtbl.create 16 in
  List.iter (fun b -> Hashtbl.replace live_in_map b.label []) blocks;
  let changed = ref true in
  while !changed do changed:=false;
    List.iter (fun b ->
      let live_out = term_live_out live_in_map b.term in
      let live_in = compute_live_in b live_out in
      let old = try Hashtbl.find live_in_map b.label with _ -> [] in
      if live_in <> old then changed:=true;
      Hashtbl.replace live_in_map b.label live_in) blocks
  done; live_in_map

let eliminate_dead blocks live_in_map =
  List.map (fun b ->
    fst (dce_block b (term_live_out live_in_map b.term))) blocks

let dce_function blocks =
  eliminate_dead blocks (compute_liveness blocks)

let const_prop_block blk =
  let consts = Hashtbl.create 16 in
  let lookup r = match Hashtbl.find_opt consts r with Some v->v | None->None in
  let body = List.map (fun instr -> match instr with
    | TConst(r,n) -> Hashtbl.replace consts r (Some n); instr
    | TCopy(r,rs) -> Hashtbl.replace consts r (lookup rs); instr
    | TBinop(r,op,r1,r2) ->
      (match lookup r1, lookup r2 with
       | Some a, Some b ->
         let v = Dag.eval_const_binop op a b in
         Hashtbl.replace consts r (Some v); TConst(r,v)
       | _ -> Hashtbl.replace consts r None; instr)
    | TUnop(r,op,r1) ->
      (match lookup r1 with
       | Some a ->
         let v = match op with Neg-> -a | Not-> if a=0 then 1 else 0 | Pos->a in
         Hashtbl.replace consts r (Some v); TConst(r,v)
       | _ -> Hashtbl.replace consts r None; instr)
    | TLoad(r,_,_) -> Hashtbl.replace consts r None; instr
    | TCall(r,_,_) -> Hashtbl.replace consts r None; instr
    | TLa(r,_) -> Hashtbl.replace consts r None; instr
    | _ -> instr) blk.body in
  let term = match blk.term with
    | TBranch(r,l1,l2) ->
      (match lookup r with Some 0 -> TJump l2 | Some _ -> TJump l1 | None -> blk.term)
    | _ -> blk.term in
  {blk with body; term}

let copy_prop_block blk =
  let copies = Hashtbl.create 16 in
  let body = List.map (fun instr ->
    let subst r = match Hashtbl.find_opt copies r with Some r'->r' | None->r in
    match instr with
    | TCopy(r,rs) -> Hashtbl.replace copies r (subst rs); instr
    | _ ->
      (match instr with
       | TConst(r,_)|TUnop(r,_,_)|TBinop(r,_,_,_)
       | TLoad(r,_,_)|TCall(r,_,_)|TLa(r,_) -> Hashtbl.remove copies r | _ -> ());
      match instr with
      | TBinop(r,op,r1,r2) -> TBinop(r,op,subst r1,subst r2)
      | TUnop(r,op,r1) -> TUnop(r,op,subst r1)
      | TStore(rb,off,rs) -> TStore(subst rb,off,subst rs)
      | TCall(r,f,args) -> TCall(r,f,List.map subst args)
      | TCallVoid(f,args) -> TCallVoid(f,List.map subst args)
      | _ -> instr) blk.body in
  let term = match blk.term with
    | TBranch(r,l1,l2) -> TBranch((try Hashtbl.find copies r with _->r),l1,l2)
    | TReturn(Some r) -> TReturn(Some (try Hashtbl.find copies r with _->r))
    | _ -> blk.term in
  {blk with body; term}

(** Local optimization: DCE + const_prop + copy_prop *)
let optimize_func (fn : func_cfg) : func_cfg =
  let blks = ref fn.blocks in
  blks := dce_function !blks;
  blks := List.map const_prop_block !blks;
  blks := List.map copy_prop_block !blks;
  blks := dce_function !blks;
  { fn with blocks = !blks }

let optimize_program (prog : program_cfg) : program_cfg =
  { prog with functions = List.map optimize_func prog.functions }
