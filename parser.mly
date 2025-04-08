%{
  open Ast
%}

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOL_LITERAL
%token <string> ID

%token DEF RETURN IF ELSE WHILE FOR IN BREAK CONTINUE LOOP
%token TRUE FALSE
%token INT FLOAT BOOL STRING

%token PLUS MINUS TIMES DIVIDE MODULO
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT

%token ASSIGN SEMI COMMA COLON
%token LPAREN RPAREN LBRACE RBRACE

%token EOF

%start program
%type <Ast.program> program


%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULO

%%

program:
  decls stmt_list_opt EOF {
    let (vdecl, fdecls) = $1 in 
    let s_stmt = $2 in 
    let funcs = if fdecls = [] then [{ rtyp = Int; fname = "fake_func"; formals = []; locals = []; body = s_stmt }]
    else fdecls
    in
    (vdecl, funcs)
  }

stmt_list_opt:
  | stmt_list         { $1 }

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

vdecl:
  typ ID { ($1, $2) }

typ:
    INT     { Int }
  | FLOAT   { Float }
  | BOOL    { Bool }
  | STRING  { String }

fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
    /* empty */         { [] }
  | stmt stmt_list      { $1 :: $2 }

stmt:
  
  | IF LPAREN expr RPAREN stmt ELSE stmt     { If ($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt            { While ($3, $5) }
  | LBRACE stmt_list RBRACE                  { Block $2 }
  | BREAK SEMI                               { Break }
  | CONTINUE SEMI                            { Continue }
  | expr SEMI                                { Expr $1 }
  | RETURN expr SEMI                         { Return $2 }

expr:
    INT_LITERAL            { LitInt $1 }
  | FLOAT_LITERAL          { LitFloat $1 }
  | STRING_LITERAL         { LitString $1 }
  | TRUE                   { LitBool true }
  | FALSE                  { LitBool false }
  | BOOL_LITERAL           { LitBool $1 }
  | ID                     { Id $1 }
  | expr PLUS expr         { Binop ($1, Add, $3) }
  | expr MINUS expr        { Binop ($1, Sub, $3) }
  | expr TIMES expr        { Binop ($1, Mul, $3) }
  | expr DIVIDE expr       { Binop ($1, Div, $3) }
  | expr MODULO expr       { Binop ($1, Mod, $3) }
  | expr EQ expr           { Binop ($1, Eq, $3) }
  | expr NEQ expr          { Binop ($1, Neq, $3) }
  | expr LT expr           { Binop ($1, Lt, $3) }
  | expr GT expr           { Binop ($1, Gt, $3) }
  | expr LEQ expr          { Binop ($1, Lte, $3) }
  | expr GEQ expr          { Binop ($1, Gte, $3) }
  | expr AND expr          { Binop ($1, And, $3) }
  | expr OR expr           { Binop ($1, Or, $3) }
  | ID ASSIGN expr         { Assign($1, $3) }
  | ID LPAREN args_opt RPAREN  { Call ($1, $3) }
  | LPAREN expr RPAREN     { $2 }


args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }



