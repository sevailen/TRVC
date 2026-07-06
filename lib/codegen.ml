(** RV32 assembly emission *)

open Ir
open Printf

let reg_of alloc r =
  if r = 8 then "fp" else if r = 10 then "a0" else if r = 11 then "a1"
  else preg_str (Hashtbl.find alloc.mapping r)

let is_spilled alloc r =
  r <> 8 && r <> 10 && r <> 11 && not (Hashtbl.mem alloc.mapping r)

let spill_slot r = 8 + (r - 12) * 4

let emit_prologue name frame is_main =
  let g = if is_main then ".globl main\n" else "" in
  sprintf "%s%s:\n\taddi sp,sp,-%d\n\tsw ra,%d(sp)\n\tsw fp,%d(sp)\n\tmv fp,sp\n"
    g name frame (frame-4) (frame-8)

let emit_epilogue frame =
  sprintf "\tmv sp,fp\n\tlw ra,%d(sp)\n\tlw fp,%d(sp)\n\taddi sp,sp,%d\n\tret\n"
    (frame-4) (frame-8) frame

let emit_block alloc blk =
  let buf = Buffer.create 256 in
  let scratch_toggle = ref 0 in
  let next_scratch () =
    let s = if !scratch_toggle = 0 then "t5" else "t6" in
    scratch_toggle := 1 - !scratch_toggle; s
  in
  let load r =
    let s = next_scratch () in
    sprintf "\tlw %s,-%d(fp)\n" s (spill_slot r), s
  in
  let store r src =
    sprintf "\tsw %s,-%d(fp)\n" src (spill_slot r)
  in

  List.iter (fun i -> match i with
    | Label l -> bprintf buf "%s:\n" l
    | Li (rd, n) ->
      if is_spilled alloc rd
      then bprintf buf "\tli t4,%d\n%s" n (store rd "t4")
      else bprintf buf "\tli %s,%d\n" (reg_of alloc rd) n
    | Mv (rd, rs) ->
      if is_spilled alloc rd
      then (if is_spilled alloc rs
            then let _, ns = load rs in bprintf buf "%s" (store rd ns)
            else bprintf buf "%s" (store rd (reg_of alloc rs)))
      else (if is_spilled alloc rs
            then let _, ns = load rs in bprintf buf "\tmv %s,%s\n" (reg_of alloc rd) ns
            else bprintf buf "\tmv %s,%s\n" (reg_of alloc rd) (reg_of alloc rs))
    | Add (rd, r1, r2) | Sub (rd, r1, r2) | Mul (rd, r1, r2)
    | Div (rd, r1, r2) | Rem (rd, r1, r2)
    | Slt (rd, r1, r2) | And (rd, r1, r2) | Or (rd, r1, r2) ->
      let op = match i with Add _->"add" | Sub _->"sub" | Mul _->"mul"
        | Div _->"div" | Rem _->"rem" | Slt _->"slt"
        | And _->"and" | Or _->"or" | _->"???" in
      scratch_toggle := 0;
      let p1, n1 = if is_spilled alloc r1 then load r1 else ("", reg_of alloc r1) in
      let p2, n2 = if is_spilled alloc r2 then load r2 else ("", reg_of alloc r2) in
      bprintf buf "%s%s" p1 p2;
      let d = if is_spilled alloc rd then next_scratch () else reg_of alloc rd in
      bprintf buf "\t%s %s,%s,%s\n" op d n1 n2;
      if is_spilled alloc rd then bprintf buf "%s" (store rd d)
    | Xori (rd, rs, imm) ->
      scratch_toggle := 0;
      let p, n = if is_spilled alloc rs then load rs else ("", reg_of alloc rs) in
      bprintf buf "%s" p;
      let d = if is_spilled alloc rd then next_scratch () else reg_of alloc rd in
      bprintf buf "\txori %s,%s,%d\n" d n imm;
      if is_spilled alloc rd then bprintf buf "%s" (store rd d)
    | Lw (rd, rb, off) ->
      scratch_toggle := 0;
      let p, n = if is_spilled alloc rb then load rb else ("", reg_of alloc rb) in
      bprintf buf "%s" p;
      let d = if is_spilled alloc rd then next_scratch () else reg_of alloc rd in
      bprintf buf "\tlw %s,%d(%s)\n" d off n;
      if is_spilled alloc rd then bprintf buf "%s" (store rd d)
    | Sw (rb, off, rs) ->
      scratch_toggle := 0;
      let pb, nb = if is_spilled alloc rb then load rb else ("", reg_of alloc rb) in
      let ps, ns = if is_spilled alloc rs then load rs else ("", reg_of alloc rs) in
      bprintf buf "%s%s\tsw %s,%d(%s)\n" pb ps ns off nb
    | La (rd, lbl) ->
      let d = if is_spilled alloc rd then "t4" else reg_of alloc rd in
      bprintf buf "\tla %s,%s\n" d lbl;
      if is_spilled alloc rd then bprintf buf "%s" (store rd "t4")
    | Jalr rs ->
      scratch_toggle := 0;
      let p, n = if is_spilled alloc rs then load rs else ("", reg_of alloc rs) in
      bprintf buf "%s\tjalr ra,%s\n" p n
    | Jal lbl -> bprintf buf "\tcall %s\n" lbl
    | Beqz (rs, lbl) ->
      scratch_toggle := 0;
      let p, n = if is_spilled alloc rs then load rs else ("", reg_of alloc rs) in
      bprintf buf "%s\tbeqz %s,%s\n" p n lbl
    | Bnez (rs, lbl) ->
      scratch_toggle := 0;
      let p, n = if is_spilled alloc rs then load rs else ("", reg_of alloc rs) in
      bprintf buf "%s\tbnez %s,%s\n" p n lbl
    | J lbl -> bprintf buf "\tj %s\n" lbl
    | Ret -> bprintf buf "\tret\n"
  ) blk.instrs;
  Buffer.contents buf

let emit (prog : program) =
  let buf = Buffer.create 2048 in
  Buffer.add_string buf ".text\n";
  List.iter (fun fn ->
    let alloc = allocate fn in
    let spill_count = List.length alloc.spills in
    let frame = 8 + spill_count * 4 in
    Buffer.add_string buf (emit_prologue fn.name frame fn.is_main);
    List.iter (fun blk ->
      Buffer.add_string buf (emit_block alloc blk)
    ) fn.blocks;
    Buffer.add_string buf (emit_epilogue frame);
    Buffer.add_string buf "\n"
  ) prog.functions;
  if prog.globals <> [] then begin
    Buffer.add_string buf ".data\n";
    List.iter (fun (x, v) ->
      Buffer.add_string buf (sprintf "%s:\n\t.word %d\n" x v)
    ) prog.globals
  end;
  Buffer.contents buf
