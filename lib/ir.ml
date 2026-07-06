(** Low-level IR: RV32 instructions with virtual registers, CFG->IR, register alloc *)

open Cfg

type vreg = int

type instr =
  | Li of vreg * int | Mv of vreg * vreg
  | Add of vreg * vreg * vreg | Sub of vreg * vreg * vreg
  | Mul of vreg * vreg * vreg | Div of vreg * vreg * vreg
  | Rem of vreg * vreg * vreg
  | Slt of vreg * vreg * vreg | Xori of vreg * vreg * int
  | And of vreg * vreg * vreg | Or of vreg * vreg * vreg
  | Lw of vreg * vreg * int | Sw of vreg * int * vreg
  | La of vreg * string
  | Jalr of vreg | Jal of string
  | Beqz of vreg * string | Bnez of vreg * string
  | J of string | Label of string | Ret

type ir_block = { label : string; instrs : instr list }
type ir_func = { name : string; blocks : ir_block list;
                 num_vregs : int; is_main : bool }
type program = { functions : ir_func list; globals : (string * int) list }

(* --- Instruction selection --- *)
let lower_tac t = match t with
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
     | Ast.Eq ->
       let t1 = Util.fresh_reg() in
       [Sub(r,r1,r2); Li(t1,0); Slt(t1,t1,r); Xori(r,t1,1)]
     | Ast.Ne ->
       let t = Util.fresh_reg() in
       [Sub(r,r1,r2); Li(t,0); Slt(t,t,r)]
     | Ast.And -> [And(r,r1,r2)]
     | Ast.Or -> [Or(r,r1,r2)])
  | TUnop(r,Neg,r1) ->
    let zero = Util.fresh_reg() in
    [Li(zero,0); Sub(r,zero,r1)]
  | TUnop(r,Not,r1) ->
    let t = Util.fresh_reg() in
    [Li(t,0); Slt(t,t,r1); Xori(r,t,1)]
  | TUnop(r,Pos,r1) -> [Mv(r,r1)]
  | TCopy(rd,rs) -> [Mv(rd,rs)]
  | TLoad(rd,rb,off) -> [Lw(rd,rb,off)]
  | TStore(rb,off,rs) -> [Sw(rb,off,rs)]
  | TCall(r,f,args) ->
    let setup = List.mapi (fun i a -> Mv(10+i, a)) args in
    setup @ [Jal f; Mv(r,10)]
  | TCallVoid(f,args) ->
    let setup = List.mapi (fun i a -> Mv(10+i, a)) args in
    setup @ [Jal f]

let lower_term t = match t with
  | TReturn None -> []  (* epilogue handles ret *)
  | TReturn (Some r) -> [Mv(10,r)]  (* epilogue handles ret *)
  | TJump lbl -> [J lbl]
  | TBranch(r,l1,l2) -> [Bnez(r,l1); J l2]

let lower_block (blk : Cfg.block) : ir_block =
  let body = List.concat_map lower_tac blk.body in
  let term = lower_term blk.term in
  { label = blk.label; instrs = Label blk.label :: body @ term }

let lower_func (fn : Cfg.func_cfg) (is_main : bool) : ir_func =
  Util.reg_counter := max (!Util.reg_counter) 12;
  let blocks = List.map lower_block fn.blocks in
  { name = fn.name; blocks; num_vregs = Util.get_reg_count(); is_main }

let lower_program (prog : Cfg.program_cfg) : program =
  { functions = List.map (fun f -> lower_func f false) prog.functions;
    globals = prog.globals }

(* --- Register allocation --- *)
type phys_reg =
  | A0|A1|A2|A3|A4|A5|A6|A7
  | T0|T1|T2|T3|T4|T5|T6
  | S0|S1|S2|S3|S4|S5|S6|S7|S8|S9|S10|S11
  | SP|FP|RA|ZERO

let allocatable =
  [T0;T1;T2;T3;T4;T5;T6;A2;A3;A4;A5;A6;A7]

let preg_str = function
  | A0->"a0"|A1->"a1"|A2->"a2"|A3->"a3"|A4->"a4"|A5->"a5"|A6->"a6"|A7->"a7"
  | T0->"t0"|T1->"t1"|T2->"t2"|T3->"t3"|T4->"t4"|T5->"t5"|T6->"t6"
  | S0->"s0"|S1->"s1"|S2->"s2"|S3->"s3"|S4->"s4"|S5->"s5"|S6->"s6"
  | S7->"s7"|S8->"s8"|S9->"s9"|S10->"s10"|S11->"s11"
  | SP->"sp"|FP->"fp"|RA->"ra"|ZERO->"zero"

type alloc_result = {
  mapping : (vreg, phys_reg) Hashtbl.t;
  spills : vreg list;
}

let allocate fn =
  let mapping = Hashtbl.create 32 in
  Hashtbl.add mapping 8 FP;
  Hashtbl.add mapping 10 A0;
  Hashtbl.add mapping 11 A1;
  let pool = ref allocatable in
  let spilled = ref [] in
  let all = ref [] in
  let add r = if not (List.mem r !all) then all := r :: !all in
  List.iter (fun b -> List.iter (function
    | Li(r,_)|Mv(r,_)|Lw(r,_,_)|La(r,_)|Jalr r|Beqz(r,_)|Bnez(r,_) -> add r
    | Add(r,r1,r2)|Sub(r,r1,r2)|Mul(r,r1,r2)|Div(r,r1,r2)|Rem(r,r1,r2)
    | Slt(r,r1,r2)|And(r,r1,r2)|Or(r,r1,r2) -> add r; add r1; add r2
    | Xori(r,r1,_) -> add r; add r1
    | Sw(r1,_,r2) -> add r1; add r2
    | _ -> ()) b.instrs) fn.blocks;
  List.iter (fun r ->
    if r <> 8 && r <> 10 && r <> 11 then
      match !pool with
      | p :: ps -> pool := ps; Hashtbl.add mapping r p
      | [] -> spilled := r :: !spilled
  ) (List.sort compare !all);
  { mapping; spills = !spilled }
