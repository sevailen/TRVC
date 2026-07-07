(** ToyC CFG IR: basic blocks + three-address code, AST→CFG, optimizations *)

open Ast

type reg = int
type label = string

(* --- TAC instructions --- *)
type tac =
  | TConst of reg * int
  | TBinop of reg * binop * reg * reg
  | TBinopImm of reg * binop * reg * int   (* r := r1 op imm (Add/Lt only) *)
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
        (match Dag.try_simplify folded op nl nr with
         | Some id -> id
         | None ->
           match Dag.get folded nl, Dag.get folded nr with
           | Dag.DConst a, Dag.DConst b ->
             let v = Dag.eval_const_binop op a b in
             Dag.intern folded (Dag.DConst v)
           | _ -> Dag.intern folded (Dag.DBinop(op,nl,nr))))
        (match Dag.try_simplify folded op nl nr with
         | Some id -> id
         | None ->
           match Dag.get folded nl, Dag.get folded nr with
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
  | RegB of int        (* local var / param promoted to a dedicated vreg (mem2reg) *)
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
  | Unop (Neg, e1) -> Dag.wrap32 (- (eval_const_b st e1))
  | Unop (Neg, e1) -> Dag.wrap32 (- (eval_const_b st e1))
  | Unop (Not, e1) -> if eval_const_b st e1 = 0 then 1 else 0
  | Unop (Pos, e1) -> eval_const_b st e1
  | Binop (op, e1, e2) -> Dag.eval_const_binop op (eval_const_b st e1) (eval_const_b st e2)
  | _ -> failwith "non-constant expression in const context"

(* ================================================================= *)
(*  Named-value load/store (slot / const / global) — all expr paths   *)
(* ================================================================= *)

let load_named (st : builder_state) (name : string) : reg =
  match resolve st name with
  | Some (RegB v) -> v
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
  | Some (RegB v) -> emit_tac st (TCopy (v, r))
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
    let v = Util.fresh_reg () in
    emit_tac st (TCopy (v, r));
    bind st x (RegB v)

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
      (* mem2reg: each param gets a dedicated vreg, initialized at entry from
         the ABI arg register (a0..a7 = vregs 10..17) or the caller's stack. *)
      List.iteri (fun i p ->
        let v = Util.fresh_reg () in
        (if i < 8 then emit_tac st (TCopy (v, 10 + i))
         else emit_tac st (TLoad (v, 8, (i - 8) * 4)));
        bind st p.pname (RegB v)) fd.params;
      List.iter (build_stmt st) fd.body;
      (* close any open trailing block (void fallthrough / unreachable merge) *)
      if st.block_open then finish_block st (TReturn None);
      pop_scope st;
      st.functions <- {name=fd.fname; ret_ty=fd.fty;
                       entry=fd.fname ^ "_entry";
                       blocks=List.rev st.blocks;
                       num_slots=0;
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
    | TBinopImm(r,_,r1,_) ->
      if List.mem r !live then
        (kept:=instr::!kept; live:=List.filter((<>)r)!live; add r1)
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
    | TBinopImm(r,op,r1,n) ->
      (match lookup r1 with
       | Some a -> let v = Dag.eval_const_binop op a n in
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
       | TConst(r,_)|TUnop(r,_,_)|TBinop(r,_,_,_)|TBinopImm(r,_,_,_)
       | TLoad(r,_,_)|TCall(r,_,_)|TLa(r,_) -> Hashtbl.remove copies r | _ -> ());
      match instr with
      | TBinop(r,op,r1,r2) -> TBinop(r,op,subst r1,subst r2)
      | TBinopImm(r,op,r1,n) -> TBinopImm(r,op,subst r1,n)
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

(* def/uses accessors for coalescing *)
let tac_def = function
  | TConst(r,_)|TBinop(r,_,_,_)|TBinopImm(r,_,_,_)|TUnop(r,_,_)|TCopy(r,_)
  | TLoad(r,_,_)|TLa(r,_)|TCall(r,_,_) -> Some r
  | TStore _|TCallVoid _ -> None

let tac_uses = function
  | TBinop(_,_,a,b) -> [a;b]
  | TBinopImm(_,_,a,_) -> [a]
  | TUnop(_,_,a) | TCopy(_,a) | TLoad(_,a,_) -> [a]
  | TStore(b,_,s) -> [b;s]
  | TCall(_,_,args) | TCallVoid(_,args) -> args
  | TConst _ | TLa _ -> []

let retarget_dest instr v = match instr with
  | TConst(_,n) -> TConst(v,n) | TBinop(_,op,a,b) -> TBinop(v,op,a,b)
  | TBinopImm(_,op,a,n) -> TBinopImm(v,op,a,n)
  | TUnop(_,op,a) -> TUnop(v,op,a) | TCopy(_,s) -> TCopy(v,s)
  | TLoad(_,b,o) -> TLoad(v,b,o) | TLa(_,l) -> TLa(v,l)
  | TCall(_,f,args) -> TCall(v,f,args) | x -> x

(** Copy coalescing: `DEF t; TCopy(v,t)` with t dead afterwards → `DEF v`.
    Removes the redundant moves mem2reg introduces on every var update. *)
let coalesce_func blocks =
  let live_in = compute_liveness blocks in
  List.map (fun b ->
    let live_out = term_live_out live_in b.term in
    let arr = Array.of_list b.body in
    let n = Array.length arr in
    let used_from k r =
      let rec go i =
        if i >= n then List.mem r live_out
        else if List.mem r (tac_uses arr.(i)) then true else go (i+1)
      in go k in
    let out = ref [] and i = ref 0 in
    while !i < n do
      (match (if !i+1 < n then Some arr.(!i+1) else None), tac_def arr.(!i) with
       | Some (TCopy (v, t')), Some t
         when t = t' && t >= 18 && v >= 18 && v <> t && not (used_from (!i+2) t) ->
         out := retarget_dest arr.(!i) v :: !out; i := !i + 2
       | _ -> out := arr.(!i) :: !out; incr i)
    done;
    { b with body = List.rev !out }
  ) blocks

(** Jump threading: bypass empty blocks whose only content is `TJump t`.
    Redirect every terminator edge through such forwarders, then drop the
    now-unreferenced forwarder blocks (keeping the entry). Safe, reduces the
    many trivial `Lx: j Ly` blocks produced by if/while/short-circuit. *)
let thread_jumps (fn : func_cfg) : func_cfg =
  let fwd = Hashtbl.create 32 in
  List.iter (fun b -> match b.body, b.term with
    | [], TJump t when t <> b.label -> Hashtbl.replace fwd b.label t
    | _ -> ()) fn.blocks;
  let rec resolve l seen =
    if List.mem l seen then l
    else match Hashtbl.find_opt fwd l with
      | Some t -> resolve t (l :: seen)
      | None -> l in
  let fix l = resolve l [] in
  let blocks = List.map (fun b ->
    let term = match b.term with
      | TJump l -> TJump (fix l)
      | TBranch (r, l1, l2) -> TBranch (r, fix l1, fix l2)
      | TReturn _ as t -> t in
    { b with term }) fn.blocks in
  (* keep entry and any non-forwarder block; forwarders are now bypassed *)
  let blocks = List.filter (fun b ->
    b.label = fn.entry || not (Hashtbl.mem fwd b.label)) blocks in
  { fn with blocks }

(* ================================================================= *)
(*  Global dataflow: predecessors                                     *)
(* ================================================================= *)

(** Build predecessor map for blocks. *)
let compute_preds blocks =
  let preds = Hashtbl.create 16 in
  List.iter (fun b -> Hashtbl.replace preds b.label []) blocks;
  List.iter (fun b ->
    let add l =
      let ps = Hashtbl.find preds l in
      if not (List.mem b.label ps) then Hashtbl.replace preds l (b.label :: ps)
    in
    match b.term with
    | TJump l -> add l
    | TBranch (_, l1, l2) -> add l1; add l2
    | TReturn _ -> ()
  ) blocks;
  (fun lbl -> try Hashtbl.find preds lbl with _ -> [])

(* ================================================================= *)
(*  Global constant propagation (iterative dataflow)                  *)
(* ================================================================= *)

(** Constant propagate a block, accepting an initial seed table (reg -> int).
    Returns updated block and the outgoing constant table. *)
let const_prop_block_seeded blk (consts : (int, int) Hashtbl.t) =
  let lookup r = Hashtbl.find_opt consts r in
  let body = List.map (fun instr -> match instr with
    | TConst(r,n) -> Hashtbl.replace consts r n; instr
    | TCopy(r,rs) ->
      (match lookup rs with Some v -> Hashtbl.replace consts r v | None -> Hashtbl.remove consts r); instr
    | TBinop(r,op,r1,r2) ->
      (match lookup r1, lookup r2 with
       | Some a, Some b ->
         (try let v = Dag.eval_const_binop op a b in
          Hashtbl.replace consts r v; TConst(r,v)
          with Failure _ -> Hashtbl.remove consts r; instr)
       | _ -> Hashtbl.remove consts r; instr)
    | TUnop(r,op,r1) ->
      (match lookup r1 with
       | Some a ->
         (try let v = match op with Neg -> Dag.wrap32 (-a) | Not -> if a=0 then 1 else 0 | Pos -> a in
          Hashtbl.replace consts r v; TConst(r,v)
          with Failure _ -> Hashtbl.remove consts r; instr)
       | _ -> Hashtbl.remove consts r; instr)
    | TBinopImm(r,op,r1,n) ->
      (match lookup r1 with
       | Some a ->
         (try let v = Dag.eval_const_binop op a n in
          Hashtbl.replace consts r v; TConst(r,v)
          with Failure _ -> Hashtbl.remove consts r; instr)
       | _ -> Hashtbl.remove consts r; instr)
    | TLoad(r,_,_) -> Hashtbl.remove consts r; instr
    | TCall(r,_,_) -> Hashtbl.remove consts r; instr
    | TLa(r,_) -> Hashtbl.remove consts r; instr
    | _ -> instr) blk.body in
  let term = match blk.term with
    | TBranch(r,l1,l2) ->
      (match lookup r with Some 0 -> TJump l2 | Some _ -> TJump l1 | None -> blk.term)
    | _ -> blk.term in
  ({blk with body; term}, consts)

let global_const_prop blocks =
  let preds = compute_preds blocks in
  (* out_maps: label -> (reg, int) Hashtbl *)
  let out_maps = Hashtbl.create 16 in
  List.iter (fun b ->
    Hashtbl.replace out_maps b.label (Hashtbl.create 16)
  ) blocks;
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun b ->
      (* Compute incoming constants: intersection of all predecessors' out sets *)
      let incoming = Hashtbl.create 16 in
      let pl = preds b.label in
      if pl <> [] then begin
        (* Start with the first predecessor's out map *)
        let first_map = Hashtbl.find out_maps (List.hd pl) in
        Hashtbl.iter (fun k v -> Hashtbl.replace incoming k v) first_map;
        (* Intersect with remaining predecessors *)
        List.iter (fun p ->
          let p_map = Hashtbl.find out_maps p in
          Hashtbl.filter_map_inplace (fun k v ->
            match Hashtbl.find_opt p_map k with
            | Some v' when v' = v -> Some v'
            | _ -> None
          ) incoming
        ) (List.tl pl)
      end;
      let _new_blk, _ = const_prop_block_seeded b incoming in
      let old_map = Hashtbl.find out_maps b.label in
      (* Check if out map changed *)
      let map_changed = ref false in
      Hashtbl.iter (fun k v ->
        match Hashtbl.find_opt old_map k with
        | Some v' when v' = v -> ()
        | _ -> map_changed := true
      ) incoming;
      Hashtbl.iter (fun k _ ->
        if not (Hashtbl.mem incoming k) then map_changed := true
      ) old_map;
      if !map_changed then begin
        changed := true;
        Hashtbl.replace out_maps b.label incoming
      end
    ) blocks
  done;
  (* Re-apply constant propagation with final incoming sets *)
  List.map (fun b ->
    let pl = preds b.label in
    let incoming = Hashtbl.create 16 in
    if pl <> [] then begin
      let first_map = Hashtbl.find out_maps (List.hd pl) in
      Hashtbl.iter (fun k v -> Hashtbl.replace incoming k v) first_map;
      List.iter (fun p ->
        let p_map = Hashtbl.find out_maps p in
        Hashtbl.filter_map_inplace (fun k _ ->
          match Hashtbl.find_opt p_map k with Some v' -> Some v' | None -> None
        ) incoming
      ) (List.tl pl)
    end;
    fst (const_prop_block_seeded b incoming)
  ) blocks

(* ================================================================= *)
(*  Global copy propagation (iterative dataflow)                      *)
(* ================================================================= *)

(** Copy-propagate a block with an initial seed copy table. *)
let copy_prop_block_seeded blk copies =
  let subst r = match Hashtbl.find_opt copies r with Some r'->r' | None->r in
  let body = List.map (fun instr ->
    match instr with
    | TCopy(r,rs) -> Hashtbl.replace copies r (subst rs); instr
    | _ ->
      (match instr with
       | TConst(r,_)|TUnop(r,_,_)|TBinop(r,_,_,_)|TBinopImm(r,_,_,_)
       | TLoad(r,_,_)|TCall(r,_,_)|TLa(r,_) -> Hashtbl.remove copies r | _ -> ());
      match instr with
      | TBinop(r,op,r1,r2) -> TBinop(r,op,subst r1,subst r2)
      | TBinopImm(r,op,r1,n) -> TBinopImm(r,op,subst r1,n)
      | TUnop(r,op,r1) -> TUnop(r,op,subst r1)
      | TStore(rb,off,rs) -> TStore(subst rb,off,subst rs)
      | TCall(r,f,args) -> TCall(r,f,List.map subst args)
      | TCallVoid(f,args) -> TCallVoid(f,List.map subst args)
      | _ -> instr) blk.body in
  let term = match blk.term with
    | TBranch(r,l1,l2) -> TBranch((subst r),l1,l2)
    | TReturn(Some r) -> TReturn(Some (subst r))
    | _ -> blk.term in
  ({blk with body; term}, copies)

let global_copy_prop blocks =
  let preds = compute_preds blocks in
  let out_maps = Hashtbl.create 16 in
  List.iter (fun b ->
    Hashtbl.replace out_maps b.label (Hashtbl.create 16)
  ) blocks;
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun b ->
      let incoming = Hashtbl.create 16 in
      let pl = preds b.label in
      if pl <> [] then begin
        let first_map = Hashtbl.find out_maps (List.hd pl) in
        Hashtbl.iter (fun k v -> Hashtbl.replace incoming k v) first_map;
        List.iter (fun p ->
          let p_map = Hashtbl.find out_maps p in
          Hashtbl.filter_map_inplace (fun k _ ->
            match Hashtbl.find_opt p_map k with Some v' -> Some v' | None -> None
          ) incoming
        ) (List.tl pl)
      end;
      let (_ : block) = fst (copy_prop_block_seeded b incoming) in
      let old_map = Hashtbl.find out_maps b.label in
      let map_changed = ref false in
      Hashtbl.iter (fun k v ->
        match Hashtbl.find_opt old_map k with
        | Some v' when v' = v -> ()
        | _ -> map_changed := true
      ) incoming;
      Hashtbl.iter (fun k _ ->
        if not (Hashtbl.mem incoming k) then map_changed := true
      ) old_map;
      if !map_changed then begin
        changed := true;
        Hashtbl.replace out_maps b.label incoming
      end
    ) blocks
  done;
  List.map (fun b ->
    let pl = preds b.label in
    let incoming = Hashtbl.create 16 in
    if pl <> [] then begin
      let first_map = Hashtbl.find out_maps (List.hd pl) in
      Hashtbl.iter (fun k v -> Hashtbl.replace incoming k v) first_map;
      List.iter (fun p ->
        let p_map = Hashtbl.find out_maps p in
        Hashtbl.filter_map_inplace (fun k _ ->
          match Hashtbl.find_opt p_map k with Some v' -> Some v' | None -> None
        ) incoming
      ) (List.tl pl)
    end;
    fst (copy_prop_block_seeded b incoming)
  ) blocks

(* ================================================================= *)
(*  Global dataflow: predecessors                                     *)
(* ================================================================= *)

(** Build predecessor map for blocks. *)
let compute_preds blocks =
  let preds = Hashtbl.create 16 in
  List.iter (fun b -> Hashtbl.replace preds b.label []) blocks;
  List.iter (fun b ->
    let add l =
      let ps = Hashtbl.find preds l in
      if not (List.mem b.label ps) then Hashtbl.replace preds l (b.label :: ps)
    in
    match b.term with
    | TJump l -> add l
    | TBranch (_, l1, l2) -> add l1; add l2
    | TReturn _ -> ()
  ) blocks;
  (fun lbl -> try Hashtbl.find preds lbl with _ -> [])

(* ================================================================= *)
(*  Global constant propagation (iterative dataflow)                  *)
(* ================================================================= *)

(** Constant propagate a block, accepting an initial seed table (reg -> int).
    Returns updated block and the outgoing constant table. *)
let const_prop_block_seeded blk (consts : (int, int) Hashtbl.t) =
  let lookup r = Hashtbl.find_opt consts r in
  let body = List.map (fun instr -> match instr with
    | TConst(r,n) -> Hashtbl.replace consts r n; instr
    | TCopy(r,rs) ->
      (match lookup rs with Some v -> Hashtbl.replace consts r v | None -> Hashtbl.remove consts r); instr
    | TBinop(r,op,r1,r2) ->
      (match lookup r1, lookup r2 with
       | Some a, Some b ->
         (try let v = Dag.eval_const_binop op a b in
          Hashtbl.replace consts r v; TConst(r,v)
          with Failure _ -> Hashtbl.remove consts r; instr)
       | _ -> Hashtbl.remove consts r; instr)
    | TUnop(r,op,r1) ->
      (match lookup r1 with
       | Some a ->
         (try let v = match op with Neg -> Dag.wrap32 (-a) | Not -> if a=0 then 1 else 0 | Pos -> a in
          Hashtbl.replace consts r v; TConst(r,v)
          with Failure _ -> Hashtbl.remove consts r; instr)
       | _ -> Hashtbl.remove consts r; instr)
    | TBinopImm(r,op,r1,n) ->
      (match lookup r1 with
       | Some a ->
         (try let v = Dag.eval_const_binop op a n in
          Hashtbl.replace consts r v; TConst(r,v)
          with Failure _ -> Hashtbl.remove consts r; instr)
       | _ -> Hashtbl.remove consts r; instr)
    | TLoad(r,_,_) -> Hashtbl.remove consts r; instr
    | TCall(r,_,_) -> Hashtbl.remove consts r; instr
    | TLa(r,_) -> Hashtbl.remove consts r; instr
    | _ -> instr) blk.body in
  let term = match blk.term with
    | TBranch(r,l1,l2) ->
      (match lookup r with Some 0 -> TJump l2 | Some _ -> TJump l1 | None -> blk.term)
    | _ -> blk.term in
  ({blk with body; term}, consts)

let global_const_prop blocks =
  let preds = compute_preds blocks in
  (* out_maps: label -> (reg, int) Hashtbl *)
  let out_maps = Hashtbl.create 16 in
  List.iter (fun b ->
    Hashtbl.replace out_maps b.label (Hashtbl.create 16)
  ) blocks;
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun b ->
      (* Compute incoming constants: intersection of all predecessors' out sets *)
      let incoming = Hashtbl.create 16 in
      let pl = preds b.label in
      if pl <> [] then begin
        (* Start with the first predecessor's out map *)
        let first_map = Hashtbl.find out_maps (List.hd pl) in
        Hashtbl.iter (fun k v -> Hashtbl.replace incoming k v) first_map;
        (* Intersect with remaining predecessors *)
        List.iter (fun p ->
          let p_map = Hashtbl.find out_maps p in
          Hashtbl.filter_map_inplace (fun k v ->
            match Hashtbl.find_opt p_map k with
            | Some v' when v' = v -> Some v'
            | _ -> None
          ) incoming
        ) (List.tl pl)
      end;
      let _new_blk, _ = const_prop_block_seeded b incoming in
      let old_map = Hashtbl.find out_maps b.label in
      (* Check if out map changed *)
      let map_changed = ref false in
      Hashtbl.iter (fun k v ->
        match Hashtbl.find_opt old_map k with
        | Some v' when v' = v -> ()
        | _ -> map_changed := true
      ) incoming;
      Hashtbl.iter (fun k _ ->
        if not (Hashtbl.mem incoming k) then map_changed := true
      ) old_map;
      if !map_changed then begin
        changed := true;
        Hashtbl.replace out_maps b.label incoming
      end
    ) blocks
  done;
  (* Re-apply constant propagation with final incoming sets *)
  List.map (fun b ->
    let pl = preds b.label in
    let incoming = Hashtbl.create 16 in
    if pl <> [] then begin
      let first_map = Hashtbl.find out_maps (List.hd pl) in
      Hashtbl.iter (fun k v -> Hashtbl.replace incoming k v) first_map;
      List.iter (fun p ->
        let p_map = Hashtbl.find out_maps p in
        Hashtbl.filter_map_inplace (fun k _ ->
          match Hashtbl.find_opt p_map k with Some v' -> Some v' | None -> None
        ) incoming
      ) (List.tl pl)
    end;
    fst (const_prop_block_seeded b incoming)
  ) blocks

(* ================================================================= *)
(*  Global copy propagation (iterative dataflow)                      *)
(* ================================================================= *)

(** Copy-propagate a block with an initial seed copy table. *)
let copy_prop_block_seeded blk copies =
  let subst r = match Hashtbl.find_opt copies r with Some r'->r' | None->r in
  let body = List.map (fun instr ->
    match instr with
    | TCopy(r,rs) -> Hashtbl.replace copies r (subst rs); instr
    | _ ->
      (match instr with
       | TConst(r,_)|TUnop(r,_,_)|TBinop(r,_,_,_)|TBinopImm(r,_,_,_)
       | TLoad(r,_,_)|TCall(r,_,_)|TLa(r,_) -> Hashtbl.remove copies r | _ -> ());
      match instr with
      | TBinop(r,op,r1,r2) -> TBinop(r,op,subst r1,subst r2)
      | TBinopImm(r,op,r1,n) -> TBinopImm(r,op,subst r1,n)
      | TUnop(r,op,r1) -> TUnop(r,op,subst r1)
      | TStore(rb,off,rs) -> TStore(subst rb,off,subst rs)
      | TCall(r,f,args) -> TCall(r,f,List.map subst args)
      | TCallVoid(f,args) -> TCallVoid(f,List.map subst args)
      | _ -> instr) blk.body in
  let term = match blk.term with
    | TBranch(r,l1,l2) -> TBranch((subst r),l1,l2)
    | TReturn(Some r) -> TReturn(Some (subst r))
    | _ -> blk.term in
  ({blk with body; term}, copies)

let global_copy_prop blocks =
  let preds = compute_preds blocks in
  let out_maps = Hashtbl.create 16 in
  List.iter (fun b ->
    Hashtbl.replace out_maps b.label (Hashtbl.create 16)
  ) blocks;
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun b ->
      let incoming = Hashtbl.create 16 in
      let pl = preds b.label in
      if pl <> [] then begin
        let first_map = Hashtbl.find out_maps (List.hd pl) in
        Hashtbl.iter (fun k v -> Hashtbl.replace incoming k v) first_map;
        List.iter (fun p ->
          let p_map = Hashtbl.find out_maps p in
          Hashtbl.filter_map_inplace (fun k _ ->
            match Hashtbl.find_opt p_map k with Some v' -> Some v' | None -> None
          ) incoming
        ) (List.tl pl)
      end;
      let (_ : block) = fst (copy_prop_block_seeded b incoming) in
      let old_map = Hashtbl.find out_maps b.label in
      let map_changed = ref false in
      Hashtbl.iter (fun k v ->
        match Hashtbl.find_opt old_map k with
        | Some v' when v' = v -> ()
        | _ -> map_changed := true
      ) incoming;
      Hashtbl.iter (fun k _ ->
        if not (Hashtbl.mem incoming k) then map_changed := true
      ) old_map;
      if !map_changed then begin
        changed := true;
        Hashtbl.replace out_maps b.label incoming
      end
    ) blocks
  done;
  List.map (fun b ->
    let pl = preds b.label in
    let incoming = Hashtbl.create 16 in
    if pl <> [] then begin
      let first_map = Hashtbl.find out_maps (List.hd pl) in
      Hashtbl.iter (fun k v -> Hashtbl.replace incoming k v) first_map;
      List.iter (fun p ->
        let p_map = Hashtbl.find out_maps p in
        Hashtbl.filter_map_inplace (fun k _ ->
          match Hashtbl.find_opt p_map k with Some v' -> Some v' | None -> None
        ) incoming
      ) (List.tl pl)
    end;
    fst (copy_prop_block_seeded b incoming)
  ) blocks

(** Immediate folding: fuse a small constant operand into the op.
    Turns `TConst(c,n); TBinop(r,Add,x,c)` into `TBinopImm(r,Add,x,n)` (later
    lowered to addi/slti), so loop-invariant `li` for the constant is dropped by
    DCE. Position-sensitive forward scan (a temp reg may be reloaded). *)
let in_imm12 n = n >= -2048 && n <= 2047
let fold_imm_block blk =
  let cst = Hashtbl.create 16 in
  let getc r = Hashtbl.find_opt cst r in
  let body = List.map (fun instr ->
    let out = match instr with
      | TBinop (r, Add, a, b) ->
        (match getc b with
         | Some n when in_imm12 n -> TBinopImm (r, Add, a, n)
         | _ -> (match getc a with
                 | Some n when in_imm12 n -> TBinopImm (r, Add, b, n)
                 | _ -> instr))
      | TBinop (r, Sub, a, b) ->
        (match getc b with
         | Some n when in_imm12 (-n) -> TBinopImm (r, Add, a, -n)
         | _ -> instr)
      | TBinop (r, Lt, a, b) ->
        (match getc b with
         | Some n when in_imm12 n -> TBinopImm (r, Lt, a, n)
         | _ -> instr)
      | _ -> instr in
    (match instr with
     | TConst (r, n) -> Hashtbl.replace cst r n
     | _ -> (match tac_def instr with Some d -> Hashtbl.remove cst d | None -> ()));
    out) blk.body in
  { blk with body }

(* ================================================================= *)
(*  Loop Invariant Code Motion (LICM)                                 *)
(* ================================================================= *)

(** Compute Reverse PostOrder numbers for blocks.
    Entry block always has RPO 1. Back edges go from higher RPO to lower RPO. *)
let compute_rpo blocks : (label, int) Hashtbl.t =
  let succs = Hashtbl.create 16 in
  List.iter (fun b ->
    let add l = 
      let prev = try Hashtbl.find succs b.label with _ -> [] in
      Hashtbl.replace succs b.label (l :: prev)
    in
    match b.term with
    | TJump l -> add l
    | TBranch (_, l1, l2) -> add l1; add l2
    | TReturn _ -> ()
  ) blocks;
  (* DFS postorder, then reverse *)
  let visited = Hashtbl.create 16 in
  let postorder = ref [] in
  let rec dfs lbl =
    if Hashtbl.mem visited lbl then () else begin
      Hashtbl.replace visited lbl ();
      List.iter dfs (try Hashtbl.find succs lbl with _ -> []);
      postorder := lbl :: !postorder
    end
  in
  let entry = (List.hd blocks).label in
  dfs entry;
  (* Now assign RPO numbers: reverse postorder = 1, 2, 3, ... *)
  let rpo = Hashtbl.create 16 in
  List.iteri (fun i lbl -> Hashtbl.replace rpo lbl (i + 1)) (List.rev !postorder);
  rpo

(** Find natural loops using Reverse PostOrder.
    A back edge a → h has RPO(h) < RPO(a). Then the loop body is
    the header plus all blocks that can reach the latch through the back edge. *)
let find_natural_loops blocks : (label * label list) list =
  let preds = compute_preds blocks in
  let rpo = compute_rpo blocks in
  let loops = ref [] in
  (* Find back edges: check every edge a → b for b.rpo < a.rpo *)
  List.iter (fun b ->
    let a_lbl = b.label in
    let a_rpo = try Hashtbl.find rpo a_lbl with _ -> 0 in
    List.iter (fun h_lbl ->
      let h_rpo = try Hashtbl.find rpo h_lbl with _ -> 0 in
      if h_rpo < a_rpo then begin
        (* Edge a → h is a back edge, h is the loop header *)
        let body = try List.assoc h_lbl !loops with _ -> [] in
        if not (List.mem a_lbl body) then
          loops := (h_lbl, a_lbl :: body) :: List.remove_assoc h_lbl !loops
      end
    ) (preds a_lbl)
  ) blocks;
  (* Expand loop body: all blocks that can reach a latch without going through header *)
  let expand_loop header body_set =
    let expanded = ref body_set in
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter (fun l ->
        if List.mem l !expanded then
          List.iter (fun p ->
            if p <> header && not (List.mem p !expanded) then begin
              expanded := p :: !expanded; changed := true
            end
          ) (preds l)
      ) !expanded
    done;
    header :: List.filter ((<>) header) !expanded
  in
  List.map (fun (h, body) -> (h, norm (expand_loop h body))) !loops

(** Perform LICM on a function's blocks.
    Moves loop-invariant instructions from loop bodies to pre-headers. *)
let licm_func (fn : func_cfg) : func_cfg =
  let loops = find_natural_loops fn.blocks in
  if loops = [] then fn else
  (* Process each loop *)
  let fn_ref = ref fn in
  List.iter (fun (hdr, body) ->
    let body_set = body in
    let cur_blocks = !fn_ref.blocks in
    (* Collect all regs defined INSIDE the loop (including header, since header's
       param copies re-define registers every iteration). *)
    let inside_defs = Hashtbl.create 16 in
    List.iter (fun l ->
      if List.mem l body_set then
        let b = try List.find (fun blk -> blk.label = l) cur_blocks with _ -> failwith "block not found" in
        List.iter (fun instr ->
          match tac_def instr with Some r -> Hashtbl.replace inside_defs r () | None -> ()
        ) b.body
    ) body_set;
    (* A reg is "loop-invariant" if it's NOT defined inside the loop body.
       ABI registers (< 18) are never invariant — copy propagation may substitute
       user vregs with ABI regs, but TCO or calls may redefine them inside the loop. *)
    let is_invariant_reg r = r >= 18 && not (Hashtbl.mem inside_defs r) in
    let () = Printf.eprintf "LICM %s: hdr=%s body=[%s] inside_defs={%s}\n%!"
      fn.name hdr
      (String.concat ";" body)
      (Hashtbl.fold (fun k _ acc -> Printf.sprintf "%s%d;" acc k) inside_defs "") in
    let inv_set = Hashtbl.create 16 in
    let inv_instrs = ref [] in
    (* A register operand is truly invariant if it's NOT redefined in the loop body
       AND either already proven invariant or defined outside the loop. *)
    let is_op_inv r =
      if Hashtbl.mem inside_defs r then false
      else Hashtbl.mem inv_set r || is_invariant_reg r
    in
    (* Iteratively find invariant instructions *)
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter (fun l ->
        if l <> hdr && List.mem l body_set then
          let b = try List.find (fun blk -> blk.label = l) cur_blocks with _ -> failwith "block not found" in
          List.iter (fun instr ->
            let r_opt = tac_def instr in
            match r_opt with
            | Some r when not (Hashtbl.mem inv_set r) ->
              let is_inv = match instr with
                | TConst _ -> not (Hashtbl.mem inside_defs r)
                | TCopy (_, rs) -> is_op_inv rs
                | TBinop (_, _, r1, r2) -> is_op_inv r1 && is_op_inv r2
                | TBinopImm (_, _, r1, _) -> is_op_inv r1
                | TUnop (_, _, r1) -> is_op_inv r1
                | TLoad (_, rb, _) -> is_op_inv rb
                | TCall _ | TCallVoid _ | TStore _ | TLa _ -> false
              in
              if is_inv then begin
                Hashtbl.replace inv_set r ();
                inv_instrs := (l, instr) :: !inv_instrs;
                changed := true
              end
            | _ -> ()
          ) b.body
      ) body_set
    done;
    if !inv_instrs <> [] then begin
      let pre_hdr_lbl = hdr ^ "_ph" in
      let preds_func = compute_preds cur_blocks in
      let preds_hdr = preds_func hdr in
      let outside_preds = List.filter (fun p -> not (List.mem p body_set)) preds_hdr in
      (* Build pre-header block *)
      let pre_hdr_body = List.rev_map snd !inv_instrs in
      let pre_hdr_blk = { label = pre_hdr_lbl; body = pre_hdr_body; term = TJump hdr } in
      (* Redirect outside preds to pre-header *)
      let updated = List.map (fun b ->
        let term = match b.term with
          | TJump l when l = hdr && List.mem b.label outside_preds -> TJump pre_hdr_lbl
          | TBranch (r, l1, l2) ->
            let l1' = if l1 = hdr && List.mem b.label outside_preds then pre_hdr_lbl else l1 in
            let l2' = if l2 = hdr && List.mem b.label outside_preds then pre_hdr_lbl else l2 in
            if l1' <> l1 || l2' <> l2 then TBranch (r, l1', l2') else b.term
          | _ -> b.term in
        { b with term }
      ) cur_blocks in
      (* Remove hoisted instructions from original blocks *)
      let hoist_map = Hashtbl.create 16 in
      List.iter (fun (lbl, instr) ->
        let prev = try Hashtbl.find hoist_map lbl with _ -> [] in
        Hashtbl.replace hoist_map lbl (instr :: prev)
      ) !inv_instrs;
      let updated = List.map (fun b ->
        let to_remove = try Hashtbl.find hoist_map b.label with _ -> [] in
        if to_remove = [] then b else
        { b with body = List.filter (fun instr -> not (List.mem instr to_remove)) b.body }
      ) updated in
      fn_ref := { !fn_ref with blocks = pre_hdr_blk :: updated }
    end
  ) loops;
  !fn_ref

(* ================================================================= *)
(*  Tail Call Optimization (TCO)                                      *)
(* ================================================================= *)

(** Detect and convert self-recursive tail calls into jumps.
    When a block ends with `TCall(r, self_name, args)` followed by
    `TReturn(Some r)`, replace with moves to param regs and a jump
    to the entry block, eliminating the call/ret overhead. *)
let tco_func (fn : func_cfg) : func_cfg =
  let entry = fn.entry in
  let name = fn.name in
  let blocks = List.map (fun blk ->
    match blk.term with
    | TReturn (Some ret_reg) ->
      (* Find the LAST TCall in the body whose dst matches ret_reg and target is self.
         Walk reversed body: collect non-call instructions as we go. *)
      let rec scan rev_rest before_call_rev =
        match rev_rest with
        | TCall (dst, fn_name, args) :: rest
          when dst = ret_reg && fn_name = name ->
          (* Found it: before_call_rev has instructs after the call (reversed);
             rest has instructions before the call (reversed). Rebuild in order. *)
          let before_call = List.rev rest in
          let arg_moves = List.mapi (fun i arg ->
            if i < 8 then TCopy (10 + i, arg)
            else TStore (8, (i - 8) * 4, arg)
          ) args in
          Some (List.concat [before_call; before_call_rev; arg_moves])
        | instr :: rest -> scan rest (instr :: before_call_rev)
        | [] -> None
      in
      (match scan (List.rev blk.body) [] with
       | Some new_body -> { blk with body = new_body; term = TJump entry }
       | None -> blk)
    | _ -> blk
  ) fn.blocks in
  { fn with blocks }

let optimize_func (fn : func_cfg) : func_cfg =
  let blks = ref fn.blocks in
  for _ = 1 to 3 do
    blks := dce_function !blks;
    blks := global_const_prop !blks;
    blks := List.map copy_prop_block !blks;
    blks := List.map fold_imm_block !blks;
    blks := coalesce_func !blks;
    blks := dce_function !blks;
  done;
  let blks = tco_func { fn with blocks = !blks } in
  let blks = licm_func blks in
  thread_jumps blks

let optimize_program (prog : program_cfg) : program_cfg =
  { prog with functions = List.map optimize_func prog.functions }
