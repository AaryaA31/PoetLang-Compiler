type token =
  | INT_LITERAL of (
# 5 "parser.mly"
        int
# 6 "parser.ml"
)
  | FLOAT_LITERAL of (
# 6 "parser.mly"
        float
# 11 "parser.ml"
)
  | STRING_LITERAL of (
# 7 "parser.mly"
        string
# 16 "parser.ml"
)
  | BOOL_LITERAL of (
# 8 "parser.mly"
        bool
# 21 "parser.ml"
)
  | ID of (
# 9 "parser.mly"
        string
# 26 "parser.ml"
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

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
  open Ast
# 72 "parser.ml"
let yytransl_const = [|
  262 (* DEF *);
  263 (* RETURN *);
  264 (* IF *);
  265 (* ELSE *);
  266 (* WHILE *);
  267 (* FOR *);
  268 (* IN *);
  269 (* BREAK *);
  270 (* CONTINUE *);
  271 (* LOOP *);
  272 (* TRUE *);
  273 (* FALSE *);
  274 (* INT *);
  275 (* FLOAT *);
  276 (* BOOL *);
  277 (* STRING *);
  278 (* PLUS *);
  279 (* MINUS *);
  280 (* TIMES *);
  281 (* DIVIDE *);
  282 (* MODULO *);
  283 (* EQ *);
  284 (* NEQ *);
  285 (* LT *);
  286 (* GT *);
  287 (* LEQ *);
  288 (* GEQ *);
  289 (* AND *);
  290 (* OR *);
  291 (* NOT *);
  292 (* ASSIGN *);
  293 (* SEMI *);
  294 (* COMMA *);
  295 (* COLON *);
  296 (* LPAREN *);
  297 (* RPAREN *);
  298 (* LBRACE *);
  299 (* RBRACE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT_LITERAL *);
  258 (* FLOAT_LITERAL *);
  259 (* STRING_LITERAL *);
  260 (* BOOL_LITERAL *);
  261 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\006\000\006\000\007\000\007\000\004\000\004\000\004\000\
\004\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\003\000\004\000\007\000\005\000\003\000\
\002\000\002\000\002\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\003\000\000\000\001\000\001\000\003\000\001\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\012\000\013\000\014\000\017\000\000\000\000\000\
\000\000\000\000\000\000\015\000\016\000\038\000\039\000\040\000\
\041\000\000\000\000\000\042\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\000\010\000\000\000\000\000\
\000\000\001\000\003\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\000\000\000\000\000\000\035\000\000\000\000\000\
\033\000\008\000\004\000\000\000\000\000\021\000\022\000\023\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\032\000\000\000\000\000\037\000\000\000\007\000\
\000\000\006\000"

let yydgoto = "\002\000\
\020\000\021\000\022\000\023\000\024\000\053\000\054\000"

let yysindex = "\015\000\
\031\255\000\000\000\000\000\000\000\000\000\000\002\255\003\255\
\006\255\020\255\021\255\000\000\000\000\000\000\000\000\000\000\
\000\000\058\255\031\255\000\000\064\000\031\255\060\255\186\255\
\058\255\058\255\058\255\058\255\000\000\000\000\026\255\077\255\
\024\255\000\000\000\000\032\255\058\255\058\255\058\255\058\255\
\058\255\058\255\058\255\058\255\058\255\058\255\058\255\058\255\
\058\255\000\000\202\255\169\255\027\255\000\000\097\255\117\255\
\000\000\000\000\000\000\029\255\029\255\000\000\000\000\000\000\
\086\000\086\000\236\254\236\254\236\254\236\254\075\000\063\000\
\000\000\058\255\000\000\031\255\031\255\000\000\061\255\000\000\
\031\255\000\000"

let yyrindex = "\000\000\
\072\000\000\000\000\000\000\000\000\000\000\000\218\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\053\255\000\000\000\000\001\000\000\000\000\000\
\000\000\071\255\000\000\000\000\000\000\000\000\054\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\072\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\132\255\149\255\000\000\000\000\000\000\
\031\000\043\000\229\255\244\255\004\000\019\000\056\255\233\254\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\018\000\192\255\000\000\238\255\000\000\040\000"

let yytablesize = 374
let yytable = "\032\000\
\002\000\037\000\038\000\039\000\040\000\041\000\051\000\052\000\
\055\000\056\000\031\000\079\000\080\000\031\000\031\000\001\000\
\082\000\031\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\070\000\071\000\072\000\003\000\
\004\000\005\000\006\000\007\000\033\000\025\000\008\000\035\000\
\009\000\026\000\027\000\010\000\011\000\028\000\012\000\013\000\
\014\000\015\000\016\000\017\000\039\000\040\000\041\000\052\000\
\029\000\030\000\003\000\004\000\005\000\006\000\031\000\034\000\
\036\000\026\000\058\000\075\000\059\000\081\000\018\000\002\000\
\019\000\012\000\013\000\018\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\030\000\030\000\018\000\018\000\030\000\030\000\018\000\002\000\
\030\000\018\000\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\034\000\
\036\000\078\000\000\000\000\000\000\000\057\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\000\000\000\000\000\000\000\000\000\000\
\000\000\076\000\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\000\000\
\000\000\019\000\019\000\000\000\000\000\077\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\000\000\000\000\
\019\000\019\000\020\000\020\000\019\000\000\000\000\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\000\000\
\000\000\020\000\020\000\000\000\000\000\020\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\000\000\000\000\000\000\074\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\000\000\000\000\050\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\000\000\000\000\073\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\000\000\000\000\018\000\026\000\
\026\000\026\000\026\000\026\000\026\000\026\000\026\000\000\000\
\000\000\026\000\026\000\000\000\000\000\026\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\000\000\000\000\
\027\000\027\000\000\000\000\000\027\000\000\000\028\000\028\000\
\028\000\028\000\028\000\028\000\028\000\028\000\000\000\000\000\
\028\000\028\000\000\000\002\000\028\000\029\000\029\000\029\000\
\029\000\029\000\029\000\029\000\029\000\000\000\000\000\029\000\
\029\000\024\000\024\000\029\000\000\000\000\000\000\000\024\000\
\024\000\000\000\000\000\024\000\024\000\025\000\025\000\024\000\
\000\000\000\000\000\000\025\000\025\000\000\000\000\000\025\000\
\025\000\000\000\000\000\025\000\037\000\038\000\039\000\040\000\
\041\000\042\000\043\000\044\000\045\000\046\000\047\000\048\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\046\000\047\000\037\000\038\000\039\000\040\000\041\000\
\000\000\000\000\044\000\045\000\046\000\047\000"

let yycheck = "\018\000\
\000\000\022\001\023\001\024\001\025\001\026\001\025\000\026\000\
\027\000\028\000\034\001\076\000\077\000\037\001\038\001\001\000\
\081\000\041\001\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\001\001\
\002\001\003\001\004\001\005\001\019\000\036\001\008\001\022\000\
\010\001\040\001\040\001\013\001\014\001\040\001\016\001\017\001\
\018\001\019\001\020\001\021\001\024\001\025\001\026\001\074\000\
\037\001\037\001\001\001\002\001\003\001\004\001\005\001\000\000\
\005\001\040\001\043\001\041\001\037\001\009\001\040\001\000\000\
\042\001\016\001\017\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\033\001\034\001\037\001\038\001\037\001\038\001\041\001\043\001\
\041\001\040\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\041\001\
\041\001\074\000\255\255\255\255\255\255\041\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\255\255\255\255\255\255\255\255\255\255\
\255\255\041\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\255\255\
\255\255\022\001\023\001\255\255\255\255\041\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\255\255\255\255\
\037\001\038\001\022\001\023\001\041\001\255\255\255\255\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\255\255\
\255\255\037\001\038\001\255\255\255\255\041\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\255\255\255\255\255\255\038\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\255\255\255\255\037\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\255\255\255\255\037\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\255\255\255\255\037\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\255\255\
\255\255\037\001\038\001\255\255\255\255\041\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\255\255\255\255\
\037\001\038\001\255\255\255\255\041\001\255\255\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\255\255\255\255\
\037\001\038\001\255\255\043\001\041\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\255\255\255\255\037\001\
\038\001\027\001\028\001\041\001\255\255\255\255\255\255\033\001\
\034\001\255\255\255\255\037\001\038\001\027\001\028\001\041\001\
\255\255\255\255\255\255\033\001\034\001\255\255\255\255\037\001\
\038\001\255\255\255\255\041\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\022\001\023\001\024\001\025\001\026\001\
\255\255\255\255\029\001\030\001\031\001\032\001"

let yynames_const = "\
  DEF\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  IN\000\
  BREAK\000\
  CONTINUE\000\
  LOOP\000\
  TRUE\000\
  FALSE\000\
  INT\000\
  FLOAT\000\
  BOOL\000\
  STRING\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MODULO\000\
  EQ\000\
  NEQ\000\
  LT\000\
  GT\000\
  LEQ\000\
  GEQ\000\
  AND\000\
  OR\000\
  NOT\000\
  ASSIGN\000\
  SEMI\000\
  COMMA\000\
  COLON\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  EOF\000\
  "

let yynames_block = "\
  INT_LITERAL\000\
  FLOAT_LITERAL\000\
  STRING_LITERAL\000\
  BOOL_LITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 39 "parser.mly"
                  ( _1 )
# 340 "parser.ml"
               : Ast.stmt list))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
                        ( [] )
# 346 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 43 "parser.mly"
                        ( _1 :: _2 )
# 354 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 46 "parser.mly"
                                             ( Expr (Assign (false, _2, LitInt 0)) )
# 362 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                                             ( Expr (Assign (false, _1, _3)) )
# 370 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 48 "parser.mly"
                                             ( If (_3, _5, _7) )
# 379 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 49 "parser.mly"
                                             ( While (_3, _5) )
# 387 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 50 "parser.mly"
                                             ( Block _2 )
# 394 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                                             ( Break )
# 400 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                                             ( Continue )
# 406 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                                             ( Expr _1 )
# 413 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 56 "parser.mly"
                           ( LitInt _1 )
# 420 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 57 "parser.mly"
                           ( LitFloat _1 )
# 427 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                           ( LitString _1 )
# 434 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                           ( LitBool true )
# 440 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
                           ( LitBool false )
# 446 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 61 "parser.mly"
                           ( LitBool _1 )
# 453 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                           ( Id _1 )
# 460 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                           ( Binop (_1, Add, _3) )
# 468 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                           ( Binop (_1, Sub, _3) )
# 476 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                           ( Binop (_1, Mul, _3) )
# 484 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                           ( Binop (_1, Div, _3) )
# 492 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                           ( Binop (_1, Mod, _3) )
# 500 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                           ( Binop (_1, Eq, _3) )
# 508 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                           ( Binop (_1, Neq, _3) )
# 516 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                           ( Binop (_1, Lt, _3) )
# 524 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                           ( Binop (_1, Gt, _3) )
# 532 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                           ( Binop (_1, Lte, _3) )
# 540 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                           ( Binop (_1, Gte, _3) )
# 548 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                           ( Binop (_1, And, _3) )
# 556 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                           ( Binop (_1, Or, _3) )
# 564 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 76 "parser.mly"
                           ( Call (_1, _3) )
# 572 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                           ( _2 )
# 579 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                       ( [] )
# 585 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 81 "parser.mly"
                       ( _1 )
# 592 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                              ( [_1] )
# 599 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 85 "parser.mly"
                              ( _1 :: _3 )
# 607 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
            ( Int )
# 613 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
            ( Float )
# 619 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
            ( Bool )
# 625 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
            ( String )
# 631 "parser.ml"
               : 'typ))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.stmt list)
