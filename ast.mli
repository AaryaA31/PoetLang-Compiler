(* Ast.ml *)

type operator =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or

type unop =
  | Neg
  | Pos
  | Not
  | Deref
  | Borrow of bool  (* true = mutable *)
    
type typ =
  | TInt
  | TFloat
  | TBool
  | TString
  | TTuple of typ list
  | TArray of typ * int
  | TStruct of string

type expr =
  | Binop of expr * operator * expr
  | Unop of unop * expr
  | LitInt of int
  | LitFloat of float
  | LitBool of bool
  | LitString of string
  | Id of string
  | Assign of bool * string * expr    (* bool = is_mutable *)
  | ArrayLit of expr list
  | TupleLit of expr list
  | StructLit of (string * expr) list
  | Call of string * expr list
  | Block of stmt list

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

type func_def = {
  name: string;
  args: string list;
  ret_type: typ option;
  body: stmt list;
}

type program = func_def list

