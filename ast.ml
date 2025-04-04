(* Ast.ml *)

type operator =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or
(* 
type unop =
  | Neg
  | Pos
  | Not
  | Deref
  | Borrow of bool  true = mutable *)
    
type typ =
  | Int
  | Float
  | Bool
  | String
  | Tuple of typ list
  | Array of typ * int  

type expr =
  | Binop of expr * operator * expr
  | LitInt of int
  | LitFloat of float
  | LitBool of bool
  | LitString of string
  | Id of string
  | Assign of bool * string * expr    (* bool = is_mutable *)
  | ArrayLit of expr list
  | TupleLit of expr list
  | Call of string * expr list

and stmt =
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of string * expr * stmt list
  | While of expr * stmt
  | Loop of stmt
  | Break
  | Continue
  | BlockStmt of stmt list

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
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

let rec string_of_typ = function
  Int -> "int"
| Float -> "float"
| Bool -> "bool"
| String -> "string"
| Tuple ts ->  "(" ^ String.concat ", " (List.map string_of_typ ts) ^ ")"
| Array (t, n) -> "array[" ^ string_of_typ t ^ "; " ^ string_of_int n ^ "]"

let rec string_of_expr = function
    LitInt l -> string_of_int l
  | LitFloat f -> string_of_float f              
  | LitBool true -> "true"
  | LitBool false -> "false"
  | LitString s -> "\"" ^ s ^ "\""              
  | Id s -> s
  | Binop (e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign (is_mut, id, e) -> (if is_mut then "mut " else "") ^ id ^ " = " ^ string_of_expr e
  | ArrayLit el -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | TupleLit el -> "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Call (f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
and string_of_stmt = function
    Expr expr -> string_of_expr expr ^ ";\n"
  | Return expr -> "return " ^ string_of_expr expr ^ ";\n"
  | If (e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For (v, e, stmts) -> "for " ^ v ^ " in " ^ string_of_expr e ^ " do\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "done\n"
  | While (e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Loop s -> "loop " ^ string_of_stmt s
  | Break -> "break;\n"
  | Continue -> "continue;\n"
  | BlockStmt stmts ->  "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"


let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
