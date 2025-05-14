open Ast
open Sast

module StringMap = Map.Make (String)



let compute_word_endings (check_str : string) : string =
  let vowel_check = function
    | 'a' | 'e' | 'i' | 'o' | 'u'
    | 'A' | 'E' | 'I' | 'O' | 'U' -> true
    | _ -> false
  in
  let len = String.length check_str in
  let rec helper idx =
    if idx >= 0 then
      if vowel_check (String.get check_str idx) then
        String.sub check_str idx (len - idx)
      else
        helper (idx - 1)
    else
      check_str
  in
  helper (len - 1)





let check (globals, functions) =
  let global_names = List.map (fun (_ty, name, _init) -> name) globals in
  (match global_names with
   | [] | [_] -> () 
   | first :: tail ->
       let suf0 = compute_word_endings first in
       List.iter (fun nm ->
         if compute_word_endings nm <> suf0 then
           raise (Failure
             ("non-rhyming global '" ^ nm
              ^ "' (expected rhyme with \"" ^ suf0 ^ "\")"))
       ) tail
  );

  let rhyme_funcs =
    List.filter (fun fd -> fd.fname <> "main") functions
  in
  let func_names = List.map (fun fd -> fd.fname) rhyme_funcs in
  (match func_names with
   | [] | [_] -> ()
   | first :: tail ->
       let suf0 = compute_word_endings first in
       List.iter (fun nm ->
         if compute_word_endings nm <> suf0 then
           raise (Failure
             ("non-rhyming function '" ^ nm
              ^ "' (expected rhyme with " ^ suf0 ^ ")"))
       ) tail
  );



  let check_binds (kind : string) (binds : (typ * string* expr option) list) =
    let rec dups = function
      | [] -> ()
      | (_, n1, _) :: (_, n2, _) :: _ when n1 = n2 ->
          raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in
    dups (List.sort (fun (_, a, _) (_, b, _) -> compare a b) binds)
  in
  
let built_in_decls =
  StringMap.add "print_string"
    { rtyp    = String;
      fname   = "print_string";
      formals = [(String, "s", None)];
      locals  = [];
      body    = [] }
  (StringMap.add "print_bool"
    { rtyp    = Bool;
      fname   = "print_bool";
      formals = [(Bool, "b", None)];
      locals  = [];
      body    = [] }
   (StringMap.add "print_int"
      { rtyp    = Int;
        fname   = "print_int";
        formals = [(Int, "x", None)];
        locals  = [];
        body    = [] }
    (StringMap.add "print_float"
      { rtyp    = Float;
        fname   = "print_float";
        formals = [(Float, "f", None)];
        locals  = [];
        body    = [] }
     StringMap.empty)))
in

  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and make_err er = raise (Failure er)
    and n = fd.fname in
    match fd with
    | _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ -> StringMap.add n fd map
  in

  let function_decls = List.fold_left add_func built_in_decls functions in

  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in


  let check_func func =
    check_binds "formal" func.formals;
    

    let formal_names = List.map ((fun (_, name, _) -> name)) func.formals in 
    (match formal_names with
    | [] | [_] -> ()
    | first :: tail ->
        let suf0 = compute_word_endings first in
        List.iter (fun nm ->
          if compute_word_endings nm <> suf0 then
            raise (Failure
              ("non-rhyming function '" ^ nm
               ^ "' (expected rhyme with \"" ^ suf0 ^ "\")"))
        ) tail
   );
   let local_names = List.map ((fun (_, name, _) -> name)) func.locals in 
    (match local_names with
    | [] | [_] -> ()
    | first :: tail ->
        let suf0 = compute_word_endings first in
        List.iter (fun nm ->
          if compute_word_endings nm <> suf0 then
            raise (Failure
              ("non-rhyming function '" ^ nm
               ^ "' (expected rhyme with \"" ^ suf0 ^ "\")"))
        ) tail
   );

  


    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    let symbols =
      List.fold_left
        (fun m (ty, name, init) -> StringMap.add name ty m)
        StringMap.empty
        (globals @ func.formals @ func.locals)
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let rec check_expr = function
      | LitInt l -> (Int, SLitInt l)
      | LitFloat l -> (Float, SLitFloat l)
      | LitBool l -> (Bool, SLitBool l)
      | LitString l -> (String, SLitString l)
      | Id var -> (type_of_identifier var, SId var)
      | Assign (var, e) as ex ->
          let lt = type_of_identifier var
          and (rt, e') = check_expr e in
          let err =
            "illegal assignment "
            ^ string_of_typ lt ^ " = "
            ^ string_of_typ rt ^ " in "
            ^ string_of_expr ex
          in
          (check_assign lt rt err, SAssign (var, (rt, e')))
      | Binop (e1, op, e2) as e ->
          let (t1, e1') = check_expr e1
          and (t2, e2') = check_expr e2 in
          let err =
            "illegal binary operator "
            ^ string_of_typ t1 ^ " "
            ^ string_of_op op ^ " "
            ^ string_of_typ t2 ^ " in "
            ^ string_of_expr e
          in
          let result_ty =
            match (op, t1, t2) with
            | (Add | Sub | Mul | Div | Mod), Int, Int -> Int
            | (Add | Sub | Mul | Div), Float, Float ->  Float
            | (Eq  | Neq), ty1, ty2 when ty1 = ty2  -> Bool
            | (Lt | Lte | Gt | Gte), Int, Int -> Bool
            | (And | Or), Bool, Bool  -> Bool
            | _ -> raise (Failure err)
          in
          (result_ty, SBinop ((t1, e1'), op, (t2, e2')))
      | Call (fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args <> param_length then
            raise
              (Failure
                 ("expecting "
                 ^ string_of_int param_length
                 ^ " arguments in "
                 ^ string_of_expr call))
          else
            let check_call (ft, name, init) e =
              let (et, e') = check_expr e in
              let err =
                "illegal argument found "
                ^ string_of_typ et ^ " expected "
                ^ string_of_typ ft ^ " in "
                ^ string_of_expr e
              in
              (check_assign ft et err, e')
            in
            let args' = List.map2 check_call fd.formals args in
            (fd.rtyp, SCall (fname, args'))
      
    in

    let check_inits (kind : string) (binds : (typ * string * expr option) list) =
      List.iter (fun (decl_ty, name, init_opt) ->
        match init_opt with
        | Some ast_e ->
            let (e_ty, _sx) = check_expr ast_e in
            if e_ty <> decl_ty then
              raise (Failure
                (Printf.sprintf
                   "initializer for %s '%s' has type %s but expected %s"
                   kind name
                   (string_of_typ e_ty) (string_of_typ decl_ty)))
        | None -> ()
      ) binds
    in
  
    check_inits "global" globals;
    check_inits "formal" func.formals;
    check_inits "locals" func.locals;

    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      | _ ->
          raise
            (Failure
               ("expected Boolean expression in "
               ^ string_of_expr e))
    in


    let rec check_stmt_list = function
      | [] -> []
      | Block sl :: sl' -> check_stmt_list (sl @ sl')
      | s :: sl -> check_stmt s :: check_stmt_list sl
    and check_stmt = function
      | Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If (e, st1, st2) ->
          SIf (check_bool_expr e, check_stmt st1, check_stmt st2)
      | While (e, st) ->
          SWhile (check_bool_expr e, check_stmt st)
      | Return e ->
          let (t, e') = check_expr e in
          if t = func.rtyp then
            SReturn (t, e')
          else
            raise
              (Failure
                 ("return gives "
                 ^ string_of_typ t
                 ^ " expected "
                 ^ string_of_typ func.rtyp
                 ^ " in "
                 ^ string_of_expr e))
    in
    {
      srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals = func.locals;
      sbody = check_stmt_list func.body;
    }
  in
  (globals, List.map check_func functions)
