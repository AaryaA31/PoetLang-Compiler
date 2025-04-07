type token =
  | INT_LITERAL of (
# 5 "parser.mly"
        int
# 6 "parser.mli"
)
  | FLOAT_LITERAL of (
# 6 "parser.mly"
        float
# 11 "parser.mli"
)
  | STRING_LITERAL of (
# 7 "parser.mly"
        string
# 16 "parser.mli"
)
  | BOOL_LITERAL of (
# 8 "parser.mly"
        bool
# 21 "parser.mli"
)
  | ID of (
# 9 "parser.mly"
        string
# 26 "parser.mli"
)
  | DEF
  | RETURN
  | IF
  | ELSE
  | WHILE
  | FOR
  | IN
  | BREAK
  | CONTINUE
  | LOOP
  | TRUE
  | FALSE
  | INT
  | FLOAT
  | BOOL
  | STRING
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULO
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | AND
  | OR
  | NOT
  | ASSIGN
  | SEMI
  | COMMA
  | COLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.stmt list
