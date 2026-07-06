(** Low-level IR: RV32 instructions with virtual registers, CFG->IR, register alloc *)

open Cfg

type vreg = int

type instr =
  | Li of vreg * int | Mv of vreg * vreg
  | Add of vreg * vreg * vreg | Sub of vreg * vreg * vreg
  | Mul of vreg * vreg * vreg | Div of vreg * vreg * vreg
  | Rem of vreg * vreg * vreg
  | Slt of vreg * vreg * vreg | Xori of vreg * vreg * int
  | Slti of vreg * vreg * int
  | Seqz of vreg * vreg | Snez of vreg * vreg
  | And of vreg * vreg * vreg | Or of vreg * vreg * vreg
  | Lw of vreg * vreg * int | Sw of vreg * int * vreg
  | La of vreg * string
  | Addi of vreg * vreg * int
  | Jalr of vreg | Jal of string
  | Beqz of vreg * string | Bnez of vreg * string
  | J of string | Label of string | Ret

type ir_block = { label : string; instrs : instr list }
type ir_func = { name : string; blocks : ir_block list;
                 num_slots : int; num_params : int; is_main : bool }
type program = { functions : ir_func list; globals : (string * int) list }

(* virtual register conventions:
   2 = sp, 8 = fp, 10..17 = a0..a7 (transient arg/return), >=18 = general *)
let a i = 10 + i

(* --- Instruction selection --- *)
let rec lower_tac t = match t with
  | TConst(r,n) -> [Li(r,n)]
  | TBinop(r,op,r1,r2) ->
    (match op with
     | Ast.Add -> [Add(r,r1,r2)]
     | Ast.Sub -> [Sub(r,r1,r2)]
     | Ast.Mul -> [Mul(r,r1,r2)]
     | Ast.Div -> [Div(r,r1,r2)]
     | Ast.Mod -> [Rem(r,r1,r2)]
     | Ast.Lt -> [Slt(r,r1,r2)]
     | Ast.Gt -> [Slt(r,r2,r1)]
     | Ast.Le -> [Slt(r,r2,r1); Xori(r,r,1)]
     | Ast.Ge -> [Slt(r,r1,r2); Xori(r,r,1)]
     | Ast.Eq -> [Sub(r,r1,r2); Seqz(r,r)]
     | Ast.Ne -> [Sub(r,r1,r2); Snez(r,r)]
     | Ast.And -> [And(r,r1,r2)]
     | Ast.Or -> [Or(r,r1,r2)])
  | TBinopImm(r,op,r1,n) ->
    (match op with
     | Ast.Add -> [Addi(r,r1,n)]
     | Ast.Lt -> [Slti(r,r1,n)]
     | _ -> failwith "unsupported immediate binop")
  | TUnop(r,Neg,r1) ->
    let zero = Util.fresh_reg() in
    [Li(zero,0); Sub(r,zero,r1)]
  | TUnop(r,Not,r1) -> [Seqz(r,r1)]
  | TUnop(r,Pos,r1) -> [Mv(r,r1)]
  | TCopy(rd,rs) -> [Mv(rd,rs)]
  | TLa(rd,lbl) -> [La(rd,lbl)]
  | TLoad(rd,rb,off) -> [Lw(rd,rb,off)]
  | TStore(rb,off,rs) -> [Sw(rb,off,rs)]
  | TCall(r,f,args) -> lower_call (Some r) f args
  | TCallVoid(f,args) -> lower_call None f args

and lower_call ret f args =
  let n = List.length args in
  let stack_args = if n > 8 then n - 8 else 0 in
  let npad = if stack_args = 0 then 0 else ((4*stack_args + 15) / 16) * 16 in
  let reg_movs =
    List.filteri (fun i _ -> i < 8) args |> List.mapi (fun i ar -> Mv(a i, ar)) in
  let pre = if stack_args > 0 then [Addi(2,2,-npad)] else [] in
  let stores =
    if stack_args > 0 then
      List.filteri (fun i _ -> i >= 8) args |> List.mapi (fun k ar -> Sw(2, k*4, ar))
    else [] in
  let post = if stack_args > 0 then [Addi(2,2,npad)] else [] in
  let get_res = match ret with Some r -> [Mv(r,10)] | None -> [] in
  pre @ stores @ reg_movs @ [Jal f] @ post @ get_res

let lower_term epi t = match t with
  | TReturn None -> [J epi]
  | TReturn (Some r) -> [Mv(10,r); J epi]
  | TJump lbl -> [J lbl]
  | TBranch(r,l1,l2) -> [Bnez(r,l1); J l2]

let lower_block epi (blk : Cfg.block) : ir_block =
  let body = List.concat_map lower_tac blk.body in
  let term = lower_term epi blk.term in
  { label = blk.label; instrs = Label blk.label :: body @ term }

let epi_label name = ".Lepi_" ^ name

(* Drop a block's trailing `J l` when l is the very next block (fall-through). *)
let drop_fallthrough fn =
  let arr = Array.of_list fn.blocks in
  let n = Array.length arr in
  let blocks = List.mapi (fun k b ->
    let next = if k + 1 < n then Some arr.(k+1).label else None in
    match List.rev b.instrs, next with
    | J l :: rest, Some nl when l = nl -> { b with instrs = List.rev rest }
    | _ -> b) fn.blocks in
  { fn with blocks }

let lower_func (fn : Cfg.func_cfg) (is_main : bool) : ir_func =
  let epi = epi_label fn.name in
  let blocks = List.map (lower_block epi) fn.blocks in
  drop_fallthrough
    { name = fn.name; blocks;
      num_slots = fn.num_slots; num_params = fn.num_params; is_main }

let lower_program (prog : Cfg.program_cfg) : program =
  { functions = List.map (fun f -> lower_func f (f.name = "main")) prog.functions;
    globals = prog.globals }

(* --- Register allocation (linear scan over live intervals) --- *)
type phys_reg =
  | A0|A1|A2|A3|A4|A5|A6|A7
  | T0|T1|T2|T3|T4|T5|T6
  | S0|S1|S2|S3|S4|S5|S6|S7|S8|S9|S10|S11
  | SP|FP|RA|ZERO

let caller_pool = [T0;T1;T2;T3]
let callee_pool = [S1;S2;S3;S4;S5;S6;S7;S8;S9;S10;S11]
let is_callee = function S1|S2|S3|S4|S5|S6|S7|S8|S9|S10|S11 -> true | _ -> false

let preg_str = function
  | A0->"a0"|A1->"a1"|A2->"a2"|A3->"a3"|A4->"a4"|A5->"a5"|A6->"a6"|A7->"a7"
  | T0->"t0"|T1->"t1"|T2->"t2"|T3->"t3"|T4->"t4"|T5->"t5"|T6->"t6"
  | S0->"s0"|S1->"s1"|S2->"s2"|S3->"s3"|S4->"s4"|S5->"s5"|S6->"s6"
  | S7->"s7"|S8->"s8"|S9->"s9"|S10->"s10"|S11->"s11"
  | SP->"sp"|FP->"fp"|RA->"ra"|ZERO->"zero"

type alloc_result = {
  mapping : (vreg, phys_reg) Hashtbl.t;   (* non-spilled vregs *)
  spill : (vreg, int) Hashtbl.t;          (* spilled vreg -> compact index *)
  spill_count : int;
  used_callee : phys_reg list;            (* callee-saved regs actually used *)
}

module IS = Set.Make (Int)

(* def/use of allocatable vregs (>=18) for one instruction *)
let def_use = function
  | Li(r,_) | La(r,_) -> ([r],[])
  | Mv(r,s) -> ([r],[s])
  | Add(r,x,y)|Sub(r,x,y)|Mul(r,x,y)|Div(r,x,y)|Rem(r,x,y)
  | Slt(r,x,y)|And(r,x,y)|Or(r,x,y) -> ([r],[x;y])
  | Addi(r,s,_)|Xori(r,s,_)|Slti(r,s,_)|Seqz(r,s)|Snez(r,s) -> ([r],[s])
  | Lw(r,b,_) -> ([r],[b])
  | Sw(b,_,s) -> ([],[b;s])
  | Beqz(r,_)|Bnez(r,_)|Jalr r -> ([],[r])
  | Jal _|J _|Label _|Ret -> ([],[])

let allocate fn =
  let blocks = Array.of_list fn.blocks in
  let nb = Array.length blocks in
  (* label -> block index *)
  let lbl2idx = Hashtbl.create 64 in
  Array.iteri (fun i b -> Hashtbl.replace lbl2idx b.label i) blocks;
  (* successors of each block, from J/Bnez/Beqz targets that name a real block *)
  let succ = Array.map (fun b ->
    List.fold_left (fun acc ins -> match ins with
      | J l | Bnez(_,l) | Beqz(_,l) ->
        (match Hashtbl.find_opt lbl2idx l with Some j -> j :: acc | None -> acc)
      | _ -> acc) [] b.instrs) blocks in
  (* def/use sets per block (only vregs >=18), computed backward *)
  let flt l = IS.of_list (List.filter (fun r -> r >= 18) l) in
  let bdef = Array.make nb IS.empty and buse = Array.make nb IS.empty in
  Array.iteri (fun i b ->
    let uu = ref IS.empty and dd = ref IS.empty in
    List.iter (fun ins ->
      let ds, us = def_use ins in
      let ds = flt ds and us = flt us in
      uu := IS.union us (IS.diff !uu ds);
      dd := IS.union !dd ds
    ) (List.rev b.instrs);
    buse.(i) <- !uu; bdef.(i) <- !dd
  ) blocks;
  (* fixpoint: live_in/live_out per block *)
  let live_in = Array.make nb IS.empty and live_out = Array.make nb IS.empty in
  let changed = ref true in
  while !changed do
    changed := false;
    for i = nb - 1 downto 0 do
      let out = List.fold_left (fun acc s -> IS.union acc live_in.(s)) IS.empty succ.(i) in
      let inn = IS.union buse.(i) (IS.diff out bdef.(i)) in
      if not (IS.equal out live_out.(i)) then (live_out.(i) <- out; changed := true);
      if not (IS.equal inn live_in.(i)) then (live_in.(i) <- inn; changed := true)
    done
  done;
  (* per-instruction: global index, live-out, call indices; derive intervals *)
  let first = Hashtbl.create 256 and last = Hashtbl.create 256 in
  let calls = ref [] in
  let crossers = Hashtbl.create 64 in
  let gidx = ref 0 in
  let bump r g =
    if r >= 18 then begin
      (match Hashtbl.find_opt first r with
       | Some f when f <= g -> () | _ -> Hashtbl.replace first r g);
      (match Hashtbl.find_opt last r with
       | Some l when l >= g -> () | _ -> Hashtbl.replace last r g)
    end in
  Array.iteri (fun i b ->
    let live = ref live_out.(i) in           (* live-after current instr *)
    let n = List.length b.instrs in
    let base = !gidx in
    gidx := !gidx + n;
    List.iteri (fun k ins ->
      let g = base + (n - 1 - k) in           (* global index of this instr *)
      let ds, us = def_use ins in
      let ds = flt ds and us = flt us in
      IS.iter (fun r -> bump r g) !live;      (* live across this point *)
      IS.iter (fun r -> bump r g) ds;
      IS.iter (fun r -> bump r g) us;
      (match ins with
       | Jal _ -> calls := g :: !calls;
         IS.iter (fun r -> Hashtbl.replace crossers r ()) !live
       | _ -> ());
      live := IS.union us (IS.diff !live ds)
    ) (List.rev b.instrs)
  ) blocks;
  ignore calls;
  let crosses_v r = Hashtbl.mem crossers r in
  let intervals =
    Hashtbl.fold (fun r s acc -> (r, s, Hashtbl.find last r) :: acc) first []
    |> List.sort (fun (_,s1,_) (_,s2,_) -> compare s1 s2) in


  let mapping = Hashtbl.create 64 in
  let spill = Hashtbl.create 16 in
  let spill_ctr = ref 0 in
  let used_callee = Hashtbl.create 16 in
  (* free pools (mutable) *)
  let free_caller = ref caller_pool in
  let free_callee = ref callee_pool in
  (* active: (end, vreg, phys) sorted by end ascending *)
  let active = ref [] in
  let release phys =
    if is_callee phys then free_callee := phys :: !free_callee
    else free_caller := phys :: !free_caller in
  let expire start =
    let rec go = function
      | (e,_,phys) :: rest when e < start -> release phys; go rest
      | l -> l in
    active := go (List.sort (fun (e1,_,_) (e2,_,_) -> compare e1 e2) !active) in
  let add_active e r phys = active := (e, r, phys) :: !active in
  List.iter (fun (r, s, e) ->
    expire s;
    let need_callee = crosses_v r in
    let chosen =
      if need_callee then
        (match !free_callee with
         | p :: ps -> free_callee := ps; Some p
         | [] -> None)
      else
        (match !free_caller with
         | p :: ps -> free_caller := ps; Some p
         | [] -> (match !free_callee with
                  | p :: ps -> free_callee := ps; Some p
                  | [] -> None))
    in
    match chosen with
    | Some phys ->
      Hashtbl.replace mapping r phys;
      if is_callee phys then Hashtbl.replace used_callee phys ();
      add_active e r phys
    | None ->
      (* spill: steal the active interval with the farthest end if beneficial *)
      let cand =
        List.fold_left (fun acc (ae,ar,aphys) ->
          match acc with
          | Some (be,_,_) when be >= ae -> acc
          | _ -> Some (ae,ar,aphys)) None
          (List.filter (fun (_,_,p) ->
             if need_callee then is_callee p else true) !active) in
      (match cand with
       | Some (ae, ar, aphys) when ae > e ->
         (* spill ar, give its reg to r *)
         Hashtbl.replace spill ar !spill_ctr; incr spill_ctr;
         Hashtbl.remove mapping ar;
         active := List.filter (fun (_,x,_) -> x <> ar) !active;
         Hashtbl.replace mapping r aphys;
         if is_callee aphys then Hashtbl.replace used_callee aphys ();
         add_active e r aphys
       | _ ->
         Hashtbl.replace spill r !spill_ctr; incr spill_ctr)
  ) intervals;
  { mapping; spill; spill_count = !spill_ctr;
    used_callee = Hashtbl.fold (fun p () acc -> p :: acc) used_callee [] }
