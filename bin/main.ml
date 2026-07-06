(** ToyC Compiler: stdin -> RISC-V 32 assembly -> stdout *)
open Toylib

let () =
  try
    let source = In_channel.input_all stdin in
    let lexbuf = Lexing.from_string source in
    let ast = Parser.program Lexer.read lexbuf in
    Semant.check ast;
    let cfg = Cfg.build_program ast in
    let opt_cfg =
      if Array.length Sys.argv > 1 && Sys.argv.(1) = "-opt" then
        Cfg.optimize_program cfg
      else cfg in
    let ir = Ir.lower_program opt_cfg in
    let asm = Codegen.emit ir in
    print_string asm
  with
  | Failure msg -> Printf.eprintf "Error: %s\n" msg; exit 1
  | Sys_error msg -> Printf.eprintf "I/O error: %s\n" msg; exit 1
  | e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e); exit 1
