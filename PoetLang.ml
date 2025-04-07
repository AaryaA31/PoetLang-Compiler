(* PoetLang.ml *)

open Ast
module VarMap = Map.Make(String)

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string

let string_of_value = function
  | VInt n -> string_of_int n
  | VFloat f -> string_of_float f
  | VBool b -> string_of_bool b
  | VString s -> s

let rec eval_expr (vars : value VarMap.t) (e : expr) : value * value VarMap.t =
  match e with
  | LitInt n -> (VInt n, vars)
  | LitFloat f -> (VFloat f, vars)
  | LitBool b -> (VBool b, vars)
  | LitString s -> (VString s, vars)
  | Id x ->
      (try (VarMap.find x vars, vars)
       with Not_found -> failwith ("Undefined variable " ^ x))
  | Binop (e1, op, e2) ->
      let v1, vars = eval_expr vars e1 in
      let v2, vars = eval_expr vars e2 in
      let int_op f = match v1, v2 with
        | VInt a, VInt b -> VInt (f a b)
        | _ -> failwith "Type error in arithmetic"
      in
      let result = match op with
        | Add -> int_op ( + )
        | Sub -> int_op ( - )
        | Mul -> int_op ( * )
        | Div -> int_op ( / )
        | Mod -> int_op (mod)
        | Eq -> VBool (v1 = v2)
        | Neq -> VBool (v1 <> v2)
        | Lt -> VBool (v1 < v2)
        | Lte -> VBool (v1 <= v2)
        | Gt -> VBool (v1 > v2)
        | Gte -> VBool (v1 >= v2)
        | And -> (match v1, v2 with VBool a, VBool b -> VBool (a && b) | _ -> failwith "Bad and")
        | Or -> (match v1, v2 with VBool a, VBool b -> VBool (a || b) | _ -> failwith "Bad or")
      in
      (result, vars)

  | Assign (_, name, expr) ->
      let v, vars' = eval_expr vars expr in
      (v, VarMap.add name v vars')
  | Call ("print", args) ->
      let vals, vars = eval_args vars args in
      List.iter (fun v -> print_endline (string_of_value v)) vals;
      (VInt 0, vars)
  | _ -> failwith "Unsupported expression"

and eval_args vars = function
  | [] -> ([], vars)
  | hd :: tl ->
      let v1, vars = eval_expr vars hd in
      let rest, vars = eval_args vars tl in
      (v1 :: rest, vars)

let rec eval_stmt (vars : value VarMap.t) (stmt : stmt) : value VarMap.t =
  match stmt with
  | Expr e -> let _, vars = eval_expr vars e in vars
  | Return e -> let v, _ = eval_expr vars e in
                print_endline (string_of_value v); vars
  | If (cond, then_s, else_s) ->
      let v, vars = eval_expr vars cond in
      (match v with
       | VBool true -> eval_stmt vars then_s
       | VBool false -> eval_stmt vars else_s
       | _ -> failwith "Non-boolean condition in if")
  | While (cond, body) ->
      let rec loop vars =
        let v, vars = eval_expr vars cond in
        match v with
        | VBool true -> loop (eval_stmt vars body)
        | VBool false -> vars
        | _ -> failwith "Non-boolean condition in while"
      in loop vars
  | Block stmts ->
      List.fold_left eval_stmt vars stmts
  | _ -> failwith "Unsupported statement"

let run_function (f : func_def) =
  (* args ignored for now *)
  let vars = VarMap.empty in
  ignore (List.fold_left eval_stmt vars f.body)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let (_, funcs) = Parser.program Scanner.token lexbuf in
  List.iter run_function funcs

