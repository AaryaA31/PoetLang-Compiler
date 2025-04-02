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
%token INT FLOAT BOOL STRING TUP MUT

%token PLUS MINUS TIMES DIVIDE MODULO
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT

%token ASSIGN SEMI COMMA COLON
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK

%token BORROW MUTABLE MUTABLEBORROW

%token EOF

%start program
%type <Ast.program> program

%%

program:
    decls EOF { $1 }

decls:
    /* empty */ { [] }
  | decls fdecl { $2 :: $1 }

fdecl:
    DEF ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
    { FunctionDef($2, $4, None, List.rev $7) }

formals_opt:
    /* empty */ { [] }
  | formals       { $1 }

formals:
    ID                { [$1] }
  | ID COMMA formals  { $1 :: $3 }

stmt_list:
    /* empty */ { [] }
  | stmt stmt_list    { $1 :: $2 }

stmt:
    expr SEMI                   { Expr $1 }
  | RETURN expr SEMI            { Return $2 }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
  | FOR ID IN expr LBRACE stmt_list RBRACE { For($2, $4, $6) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | LBRACE stmt_list RBRACE     { Block(List.rev $2) }

expr:
    INT_LITERAL           { IntLit($1) }
  | FLOAT_LITERAL         { FloatLit($1) }
  | STRING_LITERAL        { StringLit($1) }
  | TRUE                  { BoolLit(true) }
  | FALSE                 { BoolLit(false) }
  | ID                    { Id($1) }
  | expr PLUS expr        { Binop($1, Add, $3) }
  | expr MINUS expr       { Binop($1, Sub, $3) }
  | expr TIMES expr       { Binop($1, Mult, $3) }
  | expr DIVIDE expr      { Binop($1, Div, $3) }
  | expr MODULO expr      { Binop($1, Mod, $3) }
  | expr EQ expr          { Binop($1, Eq, $3) }
  | expr NEQ expr         { Binop($1, Neq, $3) }
  | expr LT expr          { Binop($1, Lt, $3) }
  | expr LEQ expr         { Binop($1, Leq, $3) }
  | expr GT expr          { Binop($1, Gt, $3) }
  | expr GEQ expr         { Binop($1, Geq, $3) }
  | expr AND expr         { Binop($1, And, $3) }
  | expr OR expr          { Binop($1, Or, $3) }
  | NOT expr              { Unop(Not, $2) }
  | MINUS expr %prec UMINUS { Unop(Neg, $2) }
  | PLUS expr  %prec UPLUS { Unop(Pos, $2) }
  | BORROW expr           { Unop(Borrow(false), $2) }
  | MUTABLEBORROW expr    { Unop(Borrow(true), $2) }
  | TIMES expr %prec DEREF { Unop(Deref, $2) }
  | ID ASSIGN expr        { Assign(false, $1, $3) }
  | MUT ID COLON ID ASSIGN expr { Assign(true, $2, $6) }
  | LPAREN expr RPAREN    { $2 }

%%


