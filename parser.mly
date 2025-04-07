%{
  open Ast
%}

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOL_LITERAL
%token <string> ID

%token DEF RETURN IF ELSE FOR WHILE IN STRUCT
%token TRUE FALSE
%token INT FLOAT BOOL STRING TUP 

%token PLUS MINUS TIMES DIVIDE MODULO
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT

%token ASSIGN SEMI COMMA COLON
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK

%token BORROW MUTABLE MUTABLEBORROW

%token BREAK CONTINUE LOOP

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
    decls EOF { $1 }

decls:
    /* empty */ { ([], []) }
  | vdecl SEMI decls { (($1::fst $3), snd $3)}
  | fdecl decls { (fst $2, ($1 ::snd $2)) }

vdecl_list:
      { [] }
  | vdecl SEMI vdecl_list   {$1::$3}

vdecl:
  typ ID { ($1, $2) }

typ:
   INT { Int }
  | BOOL { Bool }
  | FLOAT { Float }
  | STRING { String }

fdecl:
    vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    { {
      rtyp=fst $1;
      fname= snd $1;
      formals= $3;
      locals= $6;
      body= $7;
    }
     }

formals_opt:
        { [] }
  | formals_list {$1}

formals_list:
  vdecl {[$1]}
  | vdecl COMMA formals_list      { $1::$3 }


stmt_list:
    /* empty */ { [] }
  | stmt stmt_list    { $1 :: $2 }

expr_list:
    expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }

args_opt:
    { [] }
  | expr_list { $1 }

stmt:
    expr SEMI                   { Expr $1 }
  | RETURN expr SEMI            { Return $2 }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
  | FOR ID IN expr LBRACE stmt_list RBRACE { For($2, $4, $6) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | LBRACE stmt_list RBRACE     { BlockStmt $2 }
  | BREAK SEMI { Break }
  | CONTINUE SEMI { Continue }
  | LOOP stmt { Loop($2) }


expr:
    INT_LITERAL           { LitInt($1) }
  | FLOAT_LITERAL         { LitFloat($1) }
  | STRING_LITERAL        { LitString($1) }
  | TRUE                  { LitBool(true) }
  | FALSE                 { LitBool(false) }
  | ID                    { Id($1) }
  | expr PLUS expr        { Binop($1, Add, $3) }
  | expr MINUS expr       { Binop($1, Sub, $3) }
  | expr TIMES expr       { Binop($1, Mul, $3) }
  | expr DIVIDE expr      { Binop($1, Div, $3) }
  | expr MODULO expr      { Binop($1, Mod, $3) }
  | expr EQ expr          { Binop($1, Eq, $3) }
  | expr NEQ expr         { Binop($1, Neq, $3) }
  | expr LT expr          { Binop($1, Lt, $3) }
  | expr LEQ expr         { Binop($1, Lte, $3) }
  | expr GT expr          { Binop($1, Gt, $3) }
  | expr GEQ expr         { Binop($1, Gte, $3) }
  | expr AND expr         { Binop($1, And, $3) }
  | expr OR expr          { Binop($1, Or, $3) }
  | ID ASSIGN expr        { Assign(false, $1, $3) }
  | LPAREN expr RPAREN    { $2 }
  | LBRACK expr_list RBRACK { ArrayLit($2) }
  | LPAREN expr_list RPAREN { TupleLit($2) }
  | ID LPAREN args_opt RPAREN { Call($1, $3) }

%%


