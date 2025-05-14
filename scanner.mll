{ 
  open Parser
}
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = alpha | digit

let id = alpha (alphanum | '_' | '/')*
let whitespace = [' ' '\t' '\r' '\n']+
let string_char = [^ '"' '\\']
let escape_seq = "\\" ['n' 't' '\\' '"' '\'']

let comment = "//" [^ '\n']* '\n'
let block_comment = "/*" ([^ '*'] | '*' [^ '/'])* "*/"

rule token = parse
| whitespace             { token lexbuf }
| comment                { token lexbuf }
| block_comment          { token lexbuf }

(* Keywords *)
| "arr"                  { ARR } 
| "tup"                  { TUP }
| "int"                  { INT }
| "float"                { FLOAT }
| "bool"                 { BOOL }
| "string"               { STRING }
| "return"               { RETURN }
| "while"                { WHILE }
| "if"                   { IF }
| "else"                 { ELSE }
| "def"                  { DEF }
| "and"                  { AND }
| "or"                   { OR }
| "not"                  { NOT }
| "True"                 { BOOL_LITERAL(true) }
| "False"                { BOOL_LITERAL(false) }
| "rhyme"                { RHYME}

(* Operators *)
| "+"                    { PLUS }
| "-"                    { MINUS }
| "*"                    { TIMES }
| "/"                    { DIVIDE }
| "%"                    { MODULO }
| "=="                   { EQ }
| "!="                   { NEQ }
| "<="                   { LEQ }
| "<"                    { LT }
| ">="                   { GEQ }
| ">"                    { GT }
| "="                    { ASSIGN }

(* Separators *)
| "("                    { LPAREN }
| ")"                    { RPAREN }
| "{"                    { LBRACE }
| "}"                    { RBRACE }
| ","                    { COMMA }
| ":"                    { COLON }
| ";"                    { SEMI }

| '['                    { LBRACKET }
| ']'                    { RBRACKET }


(* Literals *)
| digit+ as lxm                   { INT_LITERAL(int_of_string lxm) }
| digit+ '.' digit+ as lxm       { FLOAT_LITERAL(float_of_string lxm) }
| '"' (string_char | escape_seq)* '"' as s 
                                 { STRING_LITERAL(String.sub s 1 (String.length s - 2)) }

(* Identifiers *)
| id as name                     { ID(name) }

(* End of file *)
| eof                            { EOF }

| _                              { raise (Failure ("Unknown character: " ^ Lexing.lexeme lexbuf)) }
