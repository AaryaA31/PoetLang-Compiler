open Ast

let () =
  let lexbuf = Lexing.from_channel stdin in
  (* Assuming Parser.program returns a (bind list * func_def list) tuple *)
  let prog = Parser.program Scanner.token lexbuf in
  let output = string_of_program prog in
  print_endline output
