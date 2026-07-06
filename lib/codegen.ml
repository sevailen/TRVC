(** RV32 assembly emission: frame model, spills, calling-convention prologue *)

open Ir
open Printf

(* ---- allocation queries ---- *)
let reg_name alloc r =
  if r = 2 then "sp" else if r = 8 then "fp"
  else if r >= 10 && r <= 17 then sprintf "a%d" (r - 10)
  else preg_str (Hashtbl.find alloc.mapping r)

let spilled alloc r = r >= 18 && Hashtbl.mem alloc.spill r

(* byte offsets from fp *)
let local_off k = -(12 + 4 * k)
let spill_off num_slots j = -(12 + 4 * num_slots + 4 * j)
let callee_off num_slots spill_count i =
  -(12 + 4 * num_slots + 4 * spill_count + 4 * i)

let in_range off = off >= -2048 && off <= 2047

(* Load from fp+off into [dst], using [ascr] as address scratch for large offsets. *)
let ld_fp buf dst off ascr =
  if in_range off then bprintf buf "\tlw %s,%d(fp)\n" dst off
  else bprintf buf "\tli %s,%d\n\tadd %s,fp,%s\n\tlw %s,0(%s)\n" ascr off ascr ascr dst ascr

let st_fp buf src off ascr =
  if in_range off then bprintf buf "\tsw %s,%d(fp)\n" src off
  else bprintf buf "\tli %s,%d\n\tadd %s,fp,%s\n\tsw %s,0(%s)\n" ascr off ascr ascr src ascr

let emit_func buf fn alloc =
  let num_slots = fn.num_slots in
  let sc = alloc.spill_count in
  let callee = alloc.used_callee in
  let ncallee = List.length callee in
  let raw = 8 + 4*num_slots + 4*sc + 4*ncallee in
  let frame = ((raw + 15) / 16) * 16 in

  (* ---- prologue ---- *)
  if fn.is_main then bprintf buf ".globl main\n";
  bprintf buf "%s:\n" fn.name;
  bprintf buf "\tli t6,%d\n\tsub sp,sp,t6\n\tadd t6,sp,t6\n" frame;
  bprintf buf "\tsw ra,-4(t6)\n\tsw fp,-8(t6)\n\tmv fp,t6\n";
  (* save callee-saved actually used *)
  List.iteri (fun i c ->
    st_fp buf (preg_str c) (callee_off num_slots sc i) "t5") callee;
  (* save incoming params into their slots *)
  for i = 0 to fn.num_params - 1 do
    if i < 8 then
      st_fp buf (sprintf "a%d" i) (local_off i) "t5"
    else begin
      (* incoming stack arg at fp + (i-8)*4 *)
      let inoff = (i - 8) * 4 in
      if in_range inoff then bprintf buf "\tlw t5,%d(fp)\n" inoff
      else bprintf buf "\tli t5,%d\n\tadd t5,fp,t5\n\tlw t5,0(t5)\n" inoff;
      st_fp buf "t5" (local_off i) "t4"
    end
  done;

  (* ---- body ---- *)
  (* materialize a source operand into a concrete reg name.
     [vscr] = value scratch for spilled operands; uses t6 as address scratch. *)
  let mat_src r vscr =
    if spilled alloc r then begin
      ld_fp buf vscr (spill_off num_slots (Hashtbl.find alloc.spill r)) "t6"; vscr
    end else reg_name alloc r
  in
  (* write a computed value [valreg] to dest r (store if spilled). [ascr]=addr scratch *)
  let put_dst r valreg ascr =
    if spilled alloc r then
      st_fp buf valreg (spill_off num_slots (Hashtbl.find alloc.spill r)) ascr
  in
  let dst_reg r = if spilled alloc r then "t6" else reg_name alloc r in

  List.iter (fun blk ->
    List.iter (fun i -> match i with
      | Label l -> bprintf buf "%s:\n" l
      | Li (rd, n) ->
        bprintf buf "\tli %s,%d\n" (dst_reg rd) n; put_dst rd (dst_reg rd) "t5"
      | Mv (rd, rs) ->
        let s = mat_src rs "t4" in
        let d = dst_reg rd in
        if d <> s then bprintf buf "\tmv %s,%s\n" d s;
        put_dst rd d "t5"
      | Add(rd,r1,r2) | Sub(rd,r1,r2) | Mul(rd,r1,r2) | Div(rd,r1,r2)
      | Rem(rd,r1,r2) | Slt(rd,r1,r2) | And(rd,r1,r2) | Or(rd,r1,r2) ->
        let op = match i with Add _->"add"|Sub _->"sub"|Mul _->"mul"|Div _->"div"
          |Rem _->"rem"|Slt _->"slt"|And _->"and"|Or _->"or"|_->"?" in
        let a1 = mat_src r1 "t4" in
        let a2 = mat_src r2 "t5" in
        let d = dst_reg rd in
        bprintf buf "\t%s %s,%s,%s\n" op d a1 a2;
        put_dst rd d "t4"
      | Addi (rd, rs, n) ->
        let s = mat_src rs "t4" in
        let d = dst_reg rd in
        bprintf buf "\taddi %s,%s,%d\n" d s n;
        put_dst rd d "t5"
      | Xori (rd, rs, n) ->
        let s = mat_src rs "t4" in
        let d = dst_reg rd in
        bprintf buf "\txori %s,%s,%d\n" d s n;
        put_dst rd d "t5"
      | Seqz (rd, rs) ->
        let s = mat_src rs "t4" in
        let d = dst_reg rd in
        bprintf buf "\tseqz %s,%s\n" d s;
        put_dst rd d "t5"
      | Snez (rd, rs) ->
        let s = mat_src rs "t4" in
        let d = dst_reg rd in
        bprintf buf "\tsnez %s,%s\n" d s;
        put_dst rd d "t5"
      | La (rd, lbl) ->
        let d = dst_reg rd in
        bprintf buf "\tla %s,%s\n" d lbl;
        put_dst rd d "t5"
      | Lw (rd, rb, off) ->
        let b = mat_src rb "t4" in
        let d = dst_reg rd in
        if in_range off then bprintf buf "\tlw %s,%d(%s)\n" d off b
        else bprintf buf "\tli t5,%d\n\tadd t5,%s,t5\n\tlw %s,0(t5)\n" off b d;
        put_dst rd d "t4"
      | Sw (rb, off, rs) ->
        let b = mat_src rb "t4" in
        let s = mat_src rs "t5" in
        if in_range off then bprintf buf "\tsw %s,%d(%s)\n" s off b
        else bprintf buf "\tli t6,%d\n\tadd t6,%s,t6\n\tsw %s,0(t6)\n" off b s
      | Beqz (rs, lbl) ->
        let s = mat_src rs "t4" in bprintf buf "\tbeqz %s,%s\n" s lbl
      | Bnez (rs, lbl) ->
        let s = mat_src rs "t4" in bprintf buf "\tbnez %s,%s\n" s lbl
      | Jalr rs -> let s = mat_src rs "t4" in bprintf buf "\tjalr ra,%s\n" s
      | Jal lbl -> bprintf buf "\tcall %s\n" lbl
      | J lbl -> bprintf buf "\tj %s\n" lbl
      | Ret -> bprintf buf "\tret\n"
    ) blk.instrs
  ) fn.blocks;

  (* ---- epilogue ---- *)
  bprintf buf "%s:\n" (Ir.epi_label fn.name);
  List.iteri (fun i c ->
    ld_fp buf (preg_str c) (callee_off num_slots sc i) "t5") callee;
  bprintf buf "\tlw ra,-4(fp)\n\tlw t6,-8(fp)\n\tmv sp,fp\n\tmv fp,t6\n\tret\n"

let emit (prog : program) =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf ".text\n";
  List.iter (fun fn ->
    let alloc = allocate fn in
    emit_func buf fn alloc;
    Buffer.add_string buf "\n"
  ) prog.functions;
  if prog.globals <> [] then begin
    Buffer.add_string buf ".data\n";
    List.iter (fun (x, v) ->
      Buffer.add_string buf (sprintf "%s:\n\t.word %d\n" x v)
    ) prog.globals
  end;
  Buffer.contents buf
