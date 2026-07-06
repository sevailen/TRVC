
(* Utility: counters and label generators *)

(* Reserved virtual registers:
   2 = sp, 8 = fp/s0, 10..17 = a0..a7.
   General-purpose fresh vregs start at 18. *)
let reg_counter = ref 18

let fresh_reg () =
  let r = !reg_counter in
  incr reg_counter;
  r

let reset_reg () =
  reg_counter := 18

let get_reg_count () =
  !reg_counter

let label_counter = ref 0

let fresh_label prefix =
  incr label_counter;
  Printf.sprintf "%s_%d" prefix !label_counter

let reset_label () =
  label_counter := 0

let func_counter = ref 0

let fresh_func_name () =
  incr func_counter;
  Printf.sprintf "func_%d" !func_counter
