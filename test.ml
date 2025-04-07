
open Ast

let () =
  let lexbuf = Lexing.from_channel stdin in
  let stmts = Parser.program Scanner.token lexbuf in
  let output = String.concat "" (List.map string_of_stmt stmts) in
  print_endline output
