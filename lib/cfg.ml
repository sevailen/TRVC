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
type func_cfg = { name : string; ret_ty : ty; entry : label; blocks : block list }
type program_cfg = { functions : func_cfg list; globals : (string * int) list }

type block_segment =
  | PureSeg of { reg : reg; expr : Ast.expr; env : (string * reg) list }
  | TacSeg of tac

type loop_ctx = { break_lbl : label; continue_lbl : label }

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

type builder_state = {
  mutable cur_label : label;
  mutable cur_body : block_segment list;
  mutable blocks : block list;
  mutable functions : func_cfg list;
  mutable loop_stack : loop_ctx list;
  symbols : Symbol.t;
  mutable stack_slots : (string, int) Hashtbl.t;
  mutable next_slot : int;
  mutable var_env : (string * reg) list;
  mutable globals : (string * int) list;
}

let fresh_label (_st : builder_state) = Util.fresh_label "L"

let emit_tac st t = st.cur_body <- st.cur_body @ [TacSeg t]

let emit_pure (st : builder_state) (env : (string * reg) list) (e : expr) : reg =
  match e with
  | Var x -> (match List.assoc_opt x env with Some r -> r
              | None -> failwith ("Unbound: " ^ x))
  | _ ->
    let r = Util.fresh_reg () in
    st.cur_body <- st.cur_body @ [PureSeg {reg=r; expr=e; env}]; r

let finish_block (st : builder_state) (term : terminator) : unit =
  let body = process_segments st.cur_body in
  st.blocks <- {label=st.cur_label; body; term} :: st.blocks;
  st.cur_body <- []; st.cur_label <- fresh_label st

let start_new_block (st : builder_state) (lbl : label) : unit =
  st.cur_label <- lbl; st.cur_body <- []

(* ================================================================= *)
(*  Variable stack allocation                                        *)
(* ================================================================= *)

let alloc_stack_slot (st : builder_state) (name : string) : int =
  let slot = st.next_slot in
  st.next_slot <- slot + 1;
  Hashtbl.add st.stack_slots name slot; slot

let get_stack_slot (st : builder_state) (name : string) : int =
  match Hashtbl.find_opt st.stack_slots name with
  | Some s -> s | None -> failwith ("no stack slot for " ^ name)

(* ================================================================= *)
(*  Expression builder                                               *)
(* ================================================================= *)

(** Check if expression is pure (no function calls) *)
let rec is_pure = function
  | Call _ | Assign _ -> false
  | Binop (_, e1, e2) -> is_pure e1 && is_pure e2
  | Unop (_, e1) -> is_pure e1
  | _ -> true

let rec build_expr (st : builder_state) (env : (string * reg) list) (e : expr) : reg =
  match e with
  | IntLit _ | Unop _ ->
    let vars = ref [] in
    let rec find_vars = function
      | Var x -> if not (List.mem x !vars) then vars := x :: !vars
      | Unop (_, e1) -> find_vars e1 | Binop (_, e1, e2) -> find_vars e1; find_vars e2
      | _ -> () in
    find_vars e;
    let env' = List.fold_left (fun acc v ->
      if List.mem_assoc v acc then acc else
      let slot = try get_stack_slot st v with Failure _ -> failwith ("undeclared: " ^ v) in
      let r = Util.fresh_reg () in
      emit_tac st (TLoad (r, 8(*fp/s0*), -4 * (slot + 1)));
      (v, r) :: acc) env !vars in
    emit_pure st env' e
  | Var x ->
    (match List.assoc_opt x env with
     | Some r -> r
     | None ->
       (* Try local stack variable *)
       (try
         let slot = get_stack_slot st x in
         let r = Util.fresh_reg () in
         emit_tac st (TLoad (r, 8(*fp/s0*), -4 * (slot + 1)));
         r
       with Failure _ ->
         (* Try global variable *)
         if List.mem_assoc x st.globals then
           let r = Util.fresh_reg () in
           emit_tac st (TLa (r, x));  (* load address *)
           emit_tac st (TLoad (r, r, 0));  (* load value *)
           r
         else
           failwith ("undeclared: " ^ x)))
  | Binop (And, e1, e2) ->
    let r1 = build_expr st env e1 in
    let r = Util.fresh_reg () in
    let right_lbl = fresh_label st in
    let false_lbl = fresh_label st in
    let merge_lbl = fresh_label st in
    emit_tac st (TCopy (r, r1));
    finish_block st (TBranch (r1, right_lbl, false_lbl));
    (* false_lbl *)
    start_new_block st false_lbl;
    emit_tac st (TConst (r, 0));
    finish_block st (TJump merge_lbl);
    (* right_lbl: isolate builder state for e2 evaluation *)
    start_new_block st right_lbl;
    let saved_blocks = st.blocks in
    let saved_label = st.cur_label in
    let saved_body = st.cur_body in
    st.blocks <- [];
    let r2 = build_expr st env e2 in
    let inner_blocks = List.rev st.blocks in
    st.blocks <- saved_blocks;
    st.cur_label <- saved_label;
    st.cur_body <- saved_body;
    let zero = Util.fresh_reg () in
    emit_tac st (TConst (zero, 0));
    emit_tac st (TBinop (r, Ne, r2, zero));
    finish_block st (TJump merge_lbl);
    st.blocks <- inner_blocks @ st.blocks;
    start_new_block st merge_lbl; r
  | Binop (Or, e1, e2) ->
    let r1 = build_expr st env e1 in
    let r = Util.fresh_reg () in
    let right_lbl = fresh_label st in
    let true_lbl = fresh_label st in
    let merge_lbl = fresh_label st in
    emit_tac st (TCopy (r, r1));
    finish_block st (TBranch (r1, true_lbl, right_lbl));
    (* true_lbl *)
    start_new_block st true_lbl;
    emit_tac st (TConst (r, 1));
    finish_block st (TJump merge_lbl);
    (* right_lbl: isolate builder state *)
    start_new_block st right_lbl;
    let saved_blocks = st.blocks in
    let saved_label = st.cur_label in
    let saved_body = st.cur_body in
    st.blocks <- [];
    let r2 = build_expr st env e2 in
    let inner_blocks = List.rev st.blocks in
    st.blocks <- saved_blocks;
    st.cur_label <- saved_label;
    st.cur_body <- saved_body;
    let zero = Util.fresh_reg () in
    emit_tac st (TConst (zero, 0));
    emit_tac st (TBinop (r, Ne, r2, zero));
    finish_block st (TJump merge_lbl);
    st.blocks <- inner_blocks @ st.blocks;
    start_new_block st merge_lbl; r
  | Binop (op, e1, e2) ->
    (* If expression contains function calls, evaluate subexprs manually *)
    if not (is_pure e1) || not (is_pure e2) then begin
      let r1 = build_expr st env e1 in
      let r2 = build_expr st env e2 in
      let r = Util.fresh_reg () in
      emit_tac st (TBinop (r, op, r1, r2));
      r
    end else begin
      let vars = ref [] in
      let rec find_vars = function
        | IntLit _ -> ()
        | Var x -> if not (List.mem x !vars) then vars := x :: !vars
        | Unop (_, e1) -> find_vars e1
        | Binop (_, e1, e2) -> find_vars e1; find_vars e2
        | _ -> () in
      find_vars e;
      let env' = List.fold_left (fun acc v ->
        if List.mem_assoc v acc then acc else
        let slot = get_stack_slot st v in
        let r = Util.fresh_reg () in
        emit_tac st (TLoad (r, 8(*fp/s0*), -4 * (slot + 1)));
        (v, r) :: acc) env !vars in
      emit_pure st env' e
    end
  | Call (f, args) ->
    let arg_regs = List.map (build_expr st env) args in
    let r = Util.fresh_reg () in
    emit_tac st (TCall (r, f, arg_regs)); r
  | Assign _ -> failwith "assign in expr not supported"

(* ================================================================= *)
(*  Statement builder                                                *)
(* ================================================================= *)

let rec build_stmt (st : builder_state) (env : (string * reg) list) (s : stmt) : unit =
  match s with
  | Block ss ->
    Symbol.push_scope st.symbols;
    List.iter (build_stmt st env) ss;
    Symbol.pop_scope st.symbols

  | Empty -> ()

  | ExprStmt (Assign (x, e)) ->
    let r = build_expr st env e in
    (try
       let slot = get_stack_slot st x in
       emit_tac st (TStore (8(*fp*), -4 * (slot + 1), r))
     with Failure _ ->
       (* Global variable: la tmp, x; sw r, 0(tmp) *)
       let tmp = Util.fresh_reg () in
       emit_tac st (TLa (tmp, x));
       emit_tac st (TStore (tmp, 0, r)))

  | ExprStmt e ->
    let _ = build_expr st env e in ()

  | VarDecl (x, e) ->
    let r = build_expr st env e in
    let _slot = alloc_stack_slot st x in
    emit_tac st (TStore (8 (*fp/s0*), -4 * (_slot + 1), r))

  | ConstDecl (x, e) ->
    let _v = Semant.eval_const st.symbols e in
    let _slot = alloc_stack_slot st x in ()

  | If (cond, then_s, else_s) ->
    let rc = build_expr st env cond in
    let then_lbl = fresh_label st in
    let else_lbl = fresh_label st in
    let end_lbl = fresh_label st in
    finish_block st (TBranch (rc, then_lbl, else_lbl));
    start_new_block st then_lbl;
    build_stmt st env then_s;
    finish_block st (TJump end_lbl);
    start_new_block st else_lbl;
    Option.iter (build_stmt st env) else_s;
    finish_block st (TJump end_lbl);
    start_new_block st end_lbl

  | While (cond, body) ->
    let cond_lbl = fresh_label st in
    let body_lbl = fresh_label st in
    let end_lbl = fresh_label st in
    let continue_lbl = fresh_label st in
    finish_block st (TJump cond_lbl);
    start_new_block st cond_lbl;
    let rc = build_expr st env cond in
    finish_block st (TBranch (rc, body_lbl, end_lbl));
    start_new_block st body_lbl;
    st.loop_stack <- {break_lbl=end_lbl; continue_lbl} :: st.loop_stack;
    build_stmt st env body;
    st.loop_stack <- List.tl st.loop_stack;
    finish_block st (TJump continue_lbl);
    start_new_block st continue_lbl;
    finish_block st (TJump cond_lbl);
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
    let r_opt = Option.map (build_expr st env) e_opt in
    finish_block st (TReturn r_opt)

(* ================================================================= *)
(*  Build whole program                                              *)
(* ================================================================= *)

let build_program (prog : Ast.program) : program_cfg =
  Util.reset_reg (); Util.reset_label ();
  let st = {
    cur_label = "entry"; cur_body = []; blocks = []; functions = [];
    loop_stack = [];
    symbols = Symbol.create ();
    stack_slots = Hashtbl.create 16; next_slot = 0; var_env = [];
    globals = [];
  } in
  let globals = ref [] in

  (* Register function signatures *)
  List.iter (function
    | GFuncDef fd ->
      Symbol.add st.symbols fd.fname (Func (fd.fty, fd.params))
    | _ -> ()
  ) prog;

  (* Build each function *)
  List.iter (function
    | GFuncDef fd ->
      st.cur_body <- []; st.blocks <- [];
      st.cur_label <- fd.fname ^ "_entry";
      st.stack_slots <- Hashtbl.create 16; st.next_slot <- 0;
      Symbol.push_scope st.symbols;
      let env = ref [] in
      List.iteri (fun i p ->
        let _slot = alloc_stack_slot st p.pname in
        env := (p.pname, i) :: !env  (* params come from a0-a7, use param index *)
      ) fd.params;
      List.iter (build_stmt st !env) fd.body;
      Symbol.pop_scope st.symbols;
      st.functions <- {name=fd.fname; ret_ty=fd.fty;
                       entry=fd.fname ^ "_entry";
                       blocks=List.rev st.blocks} :: st.functions
    | GVarDecl (x, e) ->
      (try let v = Semant.eval_const st.symbols e in globals := (x,v) :: !globals
       with Failure _ -> ())
    | GConstDecl (x, e) ->
      let v = Semant.eval_const st.symbols e in
      globals := (x, v) :: !globals
  ) prog;

  st.globals <- List.rev !globals;
  { functions = List.rev st.functions; globals = List.rev !globals }

(* ================================================================= *)
(*  Optimizations (adapted from SimPL version)                       *)
(* ================================================================= *)

let term_live_out live_in_map term = match term with
  | TReturn (Some r) -> [r]
  | TReturn None -> []
  | TBranch (r, l1, l2) ->
    let l1in = try Hashtbl.find live_in_map l1 with _ -> [] in
    let l2in = try Hashtbl.find live_in_map l2 with _ -> [] in
    r :: (l1in @ l2in)
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
  ({ blk with body = !kept }, !live)

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
    | TLoad _|TCall _|TCallVoid _|TLa _ ->
      (match instr with TLoad(r,_,_) -> Hashtbl.replace consts r None
       | TCall(r,_,_) -> Hashtbl.replace consts r None
       | TLa(r,_) -> Hashtbl.replace consts r None | _ -> ()); instr
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
