{ open Parser }
let digit = ['0'-'9']
let id = ['a'-'z']+


rule token = parse
| [' ' '\t' '\n']      { token lexbuf }    (* whitespace *)
| digit+ as lxm        { LITERAL(int_of_string lxm) }
| "+"                  { PLUS }
| "-"                  { MINUS }
| "*"                  { TIMES }
| "/"                  { DIVIDE }
| "="                  { ASSIGN }
| ";"                  { SEMICOLON }
| id as name           { VARIABLE(name) }
| "("                  { LPAREN }
| ")"                  { RPAREN }
| eof                  { EOF }
| _                    { failwith ("Unknown character: " ^ Lexing.lexeme lexbuf) }
