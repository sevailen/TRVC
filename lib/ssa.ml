(** SSA construction for ToyC TAC — dominators, φ nodes, renaming *)

open Ast
open Cfg

(* SSA-form TAC with versioned registers *)
type ssa_reg = int * int  (* (original_reg, version) *)

type ssa_tac =
  | SConst of ssa_reg * int
  | SBinop of ssa_reg * binop * ssa_reg * ssa_reg
  | SUnop of ssa_reg * unop * ssa_reg
  | SCopy of ssa_reg * ssa_reg
  | SLoad of ssa_reg * ssa_reg * int
  | SStore of ssa_reg * int * ssa_reg
  | SCall of ssa_reg * string * ssa_reg list
  | SCallVoid of string * ssa_reg list
  | SPhi of ssa_reg * ssa_reg list

type ssa_block = {
  s_label : label;
  s_body : ssa_tac list;
  s_term : terminator;
  s_preds : label list;
}

type ssa_func = { s_blocks : ssa_block list; s_entry : label }

(* Build CFG predecessor info *)
let compute_preds blocks =
  let preds = Hashtbl.create 16 in
  List.iter (fun b ->
    let add_pred tgt =
      let cur = try Hashtbl.find preds tgt with _ -> [] in
      if not (List.mem b.label cur) then
        Hashtbl.replace preds tgt (b.label :: cur)
    in
    match b.term with
    | TJump l -> add_pred l
    | TBranch (_, l1, l2) -> add_pred l1; add_pred l2
    | TReturn _ -> ()
  ) blocks;
  (fun lbl -> try Hashtbl.find preds lbl with _ -> [])

(* Iterative dominator computation *)
let compute_dominators blocks entry =
  let labels = List.map (fun b -> b.label) blocks in
  let dom = Hashtbl.create 16 in
  let all = Hashtbl.create 16 in  (* all block labels *)
  List.iter (fun l -> Hashtbl.add all l ()) labels;
  let all_set () = labels in
  Hashtbl.add dom entry [entry];
  List.iter (fun l -> if l <> entry then Hashtbl.add dom l (all_set())) labels;
  let preds = compute_preds blocks in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun l ->
      if l <> entry then
        let p_labs = preds l in
        let first = List.hd p_labs in
        let intersection = try Hashtbl.find dom first with _ -> [first] in
        let inter = List.fold_left (fun acc pl ->
          let pd = try Hashtbl.find dom pl with _ -> [pl] in
          List.filter (fun x -> List.mem x pd) acc
        ) intersection (List.tl p_labs) in
        let new_dom = l :: inter in
        let old = try Hashtbl.find dom l with _ -> [] in
        if List.length new_dom <> List.length old
           || List.exists (fun x -> not (List.mem x old)) new_dom then begin
          changed := true; Hashtbl.replace dom l new_dom
        end
    ) labels
  done;
  (fun l -> try Hashtbl.find dom l with _ -> [l])

(* Strict dominators: dom - {self} *)
let strict_dominators dom_fn l =
  List.filter ((<>) l) (dom_fn l)

(* Immediate dominator: the strict dominator that dominates all others *)
let immediate_dominator dom_fn l =
  let sdoms = strict_dominators dom_fn l in
  let rec find_idom candidates = function
    | [] -> candidates
    | d :: rest ->
      (* keep only those NOT strictly dominated by d *)
      let kept = List.filter (fun c ->
        not (List.mem c (strict_dominators dom_fn d))) candidates in
      find_idom kept rest
  in
  match find_idom sdoms sdoms with
  | [idom] -> Some idom
  | [] -> None
  | _ -> None

(* Dominance frontier: nodes where dominance stops *)
let compute_df blocks dom_fn =
  let preds = compute_preds blocks in
  let idom_fn l = immediate_dominator dom_fn l in
  let df = Hashtbl.create 16 in
  List.iter (fun b -> Hashtbl.replace df b.label []) blocks;
  List.iter (fun b ->
    let p_labs = preds b.label in
    if List.length p_labs >= 2 then
      List.iter (fun p ->
        let rec add_df runner =
          if runner <> b.label && not (List.mem runner (dom_fn b.label |> List.tl)) then
            let cur = try Hashtbl.find df runner with _ -> [] in
            if not (List.mem b.label cur) then
              Hashtbl.replace df runner (b.label :: cur);
            match idom_fn runner with
            | Some idom when idom <> b.label -> add_df idom
            | _ -> ()
        in
        add_df p
      ) p_labs
  ) blocks;
  (fun l -> try Hashtbl.find df l with _ -> [])

(* ================================================================= *)
(*  Convert a TAC function to SSA form                                *)
(* ================================================================= *)

let to_ssa (fn : func_cfg) : ssa_func =
  let blocks = fn.blocks in
  let dom_fn = compute_dominators blocks fn.entry in
  let df_fn = compute_df blocks dom_fn in

  (* Find all vreg definitions *)
  let def_sites = Hashtbl.create 64 in  (* vreg -> label list *)
  List.iter (fun b ->
    List.iter (fun instr ->
      let defs = match instr with
        | TConst(r,_)|TUnop(r,_,_)|TBinop(r,_,_,_)
        | TCopy(r,_)|TLoad(r,_,_)|TCall(r,_,_) -> [r]
        | _ -> [] in
      List.iter (fun r ->
        let sites = try Hashtbl.find def_sites r with _ -> [] in
        if not (List.mem b.label sites) then
          Hashtbl.replace def_sites r (b.label :: sites)
      ) defs
    ) b.body
  ) blocks;

  (* Insert φ nodes — simplified: for each vreg defined in multiple blocks
     or defined in a block reachable from multiple paths, insert φ *)
  let phi_nodes = Hashtbl.create 64 in  (* (block_label * vreg) -> () *)
  Hashtbl.iter (fun r sites ->
    let worklist = ref sites in
    let visited = Hashtbl.create 16 in
    while !worklist <> [] do
      let n = List.hd !worklist in
      worklist := List.tl !worklist;
      let df = df_fn n in
      List.iter (fun d ->
        let key = (d, r) in
        if not (Hashtbl.mem phi_nodes key) then begin
          Hashtbl.add phi_nodes key ();
          if not (Hashtbl.mem visited d) then begin
            Hashtbl.add visited d ();
            worklist := d :: !worklist
          end
        end
      ) df
    done
  ) def_sites;

  (* Rename variables — walk dominator tree *)
  let counters = Hashtbl.create 64 in
  let stacks = Hashtbl.create 64 in
  let ssa_instrs = Hashtbl.create 64 in
  let ssa_phis = Hashtbl.create 64 in

  (* Initialize stacks for each vreg *)
  Hashtbl.iter (fun r _ ->
    Hashtbl.replace stacks r []; Hashtbl.replace counters r 0
  ) def_sites;

  let new_version r =
    let c = try Hashtbl.find counters r with _ -> 0 in
    Hashtbl.replace counters r (c+1);
    (r, c)
  in

  let push_version r v =
    let s = try Hashtbl.find stacks r with _ -> [] in
    Hashtbl.replace stacks r (v :: s)
  in

  let current_version r =
    match try Hashtbl.find stacks r with _ -> [] with
    | v :: _ -> v
    | [] -> (* implicitly defined — create version 0 *)
      let v = (r, 0) in push_version r v; v
  in

  (* DFS rename on dominator tree *)
  let dom_children = Hashtbl.create 16 in
  List.iter (fun b ->
    match immediate_dominator dom_fn b.label with
    | Some idom ->
      let children = try Hashtbl.find dom_children idom with _ -> [] in
      Hashtbl.replace dom_children idom (b.label :: children)
    | None -> ()
  ) blocks;

  let visited = Hashtbl.create 16 in
  let rec rename_block lbl =
    if Hashtbl.mem visited lbl then () else begin
      Hashtbl.add visited lbl ();
      let blk = List.find (fun b -> b.label = lbl) blocks in

      (* Push φ versions *)
      List.iter (fun r ->
        let sites = try Hashtbl.find def_sites r with _ -> [] in
        if List.mem lbl sites then begin
          let key = (lbl, r) in
          if Hashtbl.mem phi_nodes key then begin
            let v = new_version r in
            push_version r v;
            let phis = try Hashtbl.find ssa_phis lbl with _ -> [] in
            Hashtbl.replace ssa_phis lbl ((r, v, []) :: phis)
          end
        end
      ) (Hashtbl.fold (fun r _ acc -> r :: acc) def_sites []);

      (* Rename instructions *)
      let renamed_body = List.map (fun instr ->
        let rename_use r = current_version r in
        let make_def r =
          let v = new_version r in push_version r v; v in
        match instr with
        | TConst(r,n) -> SConst(make_def r, n)
        | TBinop(r,op,r1,r2) -> SBinop(make_def r, op, rename_use r1, rename_use r2)
        | TBinopImm(r,_,r1,_) -> SUnop(make_def r, Ast.Pos, rename_use r1)
        | TUnop(r,op,r1) -> SUnop(make_def r, op, rename_use r1)
        | TCopy(r,rs) -> SCopy(make_def r, rename_use rs)
        | TLoad(r,rb,off) -> SLoad(make_def r, rename_use rb, off)
        | TStore(rb,off,rs) -> SStore(rename_use rb, off, rename_use rs)
        | TLa(r,_) -> SConst(make_def r, 0)  (* TLa not SSA'd — just placeholder *)
        | TCall(r,f,args) -> SCall(make_def r, f, List.map rename_use args)
        | TCallVoid(f,args) -> SCallVoid(f, List.map rename_use args)
      ) blk.body in
      Hashtbl.replace ssa_instrs lbl renamed_body;

      (* Fill φ operands in successors *)
      List.iter (fun succ_lbl ->
        Hashtbl.iter (fun r _ ->
          let skey = (succ_lbl, r) in
          if Hashtbl.mem phi_nodes skey then begin
            let phis = try Hashtbl.find ssa_phis succ_lbl with _ -> [] in
            let updated = List.map (fun (reg, ver, ops) ->
              if reg = r then (reg, ver, (current_version r) :: ops)
              else (reg, ver, ops)
            ) phis in
            Hashtbl.replace ssa_phis succ_lbl updated
          end
        ) def_sites;
      ) (match blk.term with TJump l -> [l] | TBranch(_,l1,l2)->[l1;l2] | _->[]);

      (* Recurse dominator tree children *)
      let children = try Hashtbl.find dom_children lbl with _ -> [] in
      List.iter rename_block children;

      (* Pop pushed versions for this block *)
      List.iter (fun r ->
        let sites = try Hashtbl.find def_sites r with _ -> [] in
        if List.mem lbl sites then
          match try Hashtbl.find stacks r with _ -> [] with
          | _ :: rest -> Hashtbl.replace stacks r rest
          | _ -> ()
      ) (Hashtbl.fold (fun r _ acc -> r :: acc) def_sites [])
    end
  in

  rename_block fn.entry;

  (* Build SSA blocks *)
  { s_entry = fn.entry;
    s_blocks = List.map (fun b ->
      let phis_opt = (try Hashtbl.find ssa_phis b.label with _ -> []) in
      let body_instrs = (try Hashtbl.find ssa_instrs b.label with _ -> []) in
      let body =
        if phis_opt = [] then body_instrs
        else
          let phi_instrs = List.map (fun (_r, ver, ops) ->
            SPhi (ver, List.rev ops)) phis_opt in
          phi_instrs @ body_instrs
      in
      (* Keep original terminator — SSA doesn't rename terminators;
         uses in terminators are handled by the original register *)
      let s_term = b.term in
      { s_label = b.label; s_body = body; s_term;
        s_preds = compute_preds blocks b.label }
    ) blocks }

(* ================================================================= *)
(*  Global optimizations on SSA form                                  *)
(* ================================================================= *)

(** Count uses of each ssa_reg across all blocks *)
let count_uses (fn : ssa_func) : (ssa_reg, int) Hashtbl.t =
  let uses = Hashtbl.create 64 in
  let add_use (r : ssa_reg) =
    let n = try Hashtbl.find uses r with _ -> 0 in
    Hashtbl.replace uses r (n+1)
  in
  List.iter (fun b ->
    List.iter (fun instr -> match instr with
      | SBinop(_,_,r1,r2) -> add_use r1; add_use r2
      | SUnop(_,_,r1) -> add_use r1
      | SCopy(_,rs) -> add_use rs
      | SLoad(_,rb,_) -> add_use rb
      | SStore(rb,_,rs) -> add_use rb; add_use rs
      | SCall(_,_,args) -> List.iter add_use args
      | SCallVoid(_,args) -> List.iter add_use args
      | SPhi(_,ops) -> List.iter add_use ops
      | SConst _ -> ()
    ) b.s_body;
    (* Count terminator uses by original register *)
    (match b.s_term with
     | TReturn (Some r) -> add_use (r, 0)
     | TBranch (r, _, _) -> add_use (r, 0)
     | _ -> ())
  ) fn.s_blocks;
  uses

(** Global DCE on SSA: remove definitions with zero uses *)
let ssa_dce (fn : ssa_func) : ssa_func =
  let uses = count_uses fn in
  let s_blocks = List.map (fun b ->
    let body = List.filter (fun instr ->
      let def = match instr with
        | SConst(r,_) | SBinop(r,_,_,_) | SUnop(r,_,_) | SCopy(r,_)
        | SLoad(r,_,_) | SCall(r,_,_) | SPhi(r,_) -> Some r
        | SCallVoid _ | SStore _ -> None
      in
      match def with
      | Some r -> Hashtbl.mem uses r
      | None -> true  (* keep stores and void calls *)
    ) b.s_body in
    { b with s_body = body }
  ) fn.s_blocks in
  { fn with s_blocks }

(** Global constant propagation on SSA *)
let ssa_const_prop (fn : ssa_func) : ssa_func =
  let consts = Hashtbl.create 64 in
  (* Two-pass: first collect all constants, then substitute *)
  List.iter (fun b -> List.iter (fun instr ->
    match instr with
    | SConst(r,n) -> Hashtbl.replace consts r n
    | SCopy(r,rs) ->
      (match Hashtbl.find_opt consts rs with
       | Some n -> Hashtbl.replace consts r n
       | None -> ())
    | _ -> ()
  ) b.s_body) fn.s_blocks;
  (* Substitute *)
  let subst r = match Hashtbl.find_opt consts r with
    | Some n -> Some n | None -> None
  in
  let s_blocks = List.map (fun b ->
    let body = List.concat_map (fun instr -> match instr with
      | SBinop(r,op,r1,r2) ->
        (match subst r1, subst r2 with
         | Some a, Some b -> [SConst(r, Dag.eval_const_binop op a b)]
         | _ -> [SBinop(r, op, r1, r2)])
      | SCopy(r,rs) ->
        (match subst rs with Some n -> [SConst(r, n)] | None -> [SCopy(r, rs)])
      | SPhi(r,ops) ->
        let ops' = List.map (fun op -> match subst op with Some _ -> op | None -> op) ops in
        [SPhi(r, ops')]
      | _ -> [instr]
    ) b.s_body in
    { b with s_body = body }
  ) fn.s_blocks in
  { fn with s_blocks }

(** Convert SSA back to regular TAC (remove φ nodes) *)
let from_ssa (fn : ssa_func) : func_cfg =
  let blocks = List.map (fun b ->
    let body = List.concat_map (fun instr -> match instr with
      | SConst(r,n) -> [TConst(fst r, n)]
      | SBinop(r,op,r1,r2) -> [TBinop(fst r, op, fst r1, fst r2)]
      | SUnop(r,op,r1) -> [TUnop(fst r, op, fst r1)]
      | SCopy(r,rs) -> [TCopy(fst r, fst rs)]
      | SLoad(r,rb,off) -> [TLoad(fst r, fst rb, off)]
      | SStore(rb,off,rs) -> [TStore(fst rb, off, fst rs)]
      | SCall(r,f,args) -> [TCall(fst r, f, List.map fst args)]
      | SCallVoid(f,args) -> [TCallVoid(f, List.map fst args)]
      | SPhi(dst, ops) ->
        if ops <> [] then [TCopy(fst dst, fst (List.hd ops))]
        else []
    ) b.s_body in
    { label = b.s_label; body; term = b.s_term }
  ) fn.s_blocks in
  { name = ""; ret_ty = Int; entry = fn.s_entry; blocks;
    num_slots = 0; num_params = 0 }

(** Full SSA optimization pipeline *)
let optimize_ssa (fn : func_cfg) : func_cfg =
  let ssa_fn = to_ssa fn in
  let ssa_fn = ssa_const_prop ssa_fn in
  let ssa_fn = ssa_dce ssa_fn in
  let result = from_ssa ssa_fn in
  { result with name = fn.name; ret_ty = fn.ret_ty }
