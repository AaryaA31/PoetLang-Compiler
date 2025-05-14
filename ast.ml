type operator =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or

    
type typ =
  | Int
  | Float
  | Bool
  | String


type expr =
  | Binop of expr * operator * expr
  | LitInt of int
  | LitFloat of float
  | LitBool of bool
  | LitString of string
  | Id of string
  | Assign of string * expr  
  | Call of string * expr list

and stmt =
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Block of stmt list



type bind = typ * string * expr option

type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * func_def list

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | And -> "&&"
  | Or -> "||"
  | Div -> "/"
  | Mul -> "*"
  | Mod -> "%"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="

let string_of_typ = function
  Int -> "int"
| Float -> "float"
| Bool -> "bool"
| String -> "string"


let rec string_of_expr = function
    LitInt l -> string_of_int l
  | LitFloat f -> string_of_float f              
  | LitBool true -> "true"
  | LitBool false -> "false"
  | LitString s -> "\"" ^ s ^ "\""              
  | Id s -> s
  | Binop (e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign (id, e) -> id ^ " = " ^ string_of_expr e
  | Call (f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
and string_of_stmt = function
    Expr expr -> string_of_expr expr ^ ";\n"
  | Return expr -> "return " ^ string_of_expr expr ^ ";\n"
  | If (e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While (e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Block stmts ->  "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"


  let string_of_vdecl (t, id, init_opt) =
    let base = string_of_typ t ^ " " ^ id in
    match init_opt with
    | None ->
        base ^ ";\n"
    | Some e ->
        base ^ " = " ^ string_of_expr e ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map (fun (_typ, name, _init) -> name) fdecl.formals) ^  
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "Parsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
