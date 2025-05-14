module L = Llvm
module A = Ast
open Sast
open Ast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context    = L.global_context () in

  let the_module = L.create_module context "PoetLang" in

  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context 
  and f64_t    = L.double_type  context     
in
let string_t = L.pointer_type i8_t in 

let ltype_of_typ = function
  | A.Int  -> i32_t
  | A.Float -> f64_t
  | A.Bool  -> i1_t
  | A.String -> string_t
in

let global_vars : L.llvalue StringMap.t =
  List.fold_left
    (fun map (ty, name, opt_init) ->
       let llty = ltype_of_typ ty in
       let init =
         match opt_init, ty with
         | Some LitInt   i, _ -> L.const_int   llty i
         | Some LitBool  b, _ -> L.const_int   llty (if b then 1 else 0)
         | Some LitFloat f, _  -> L.const_float llty f
         | Some LitString s, _ -> L.const_string context s
         | None, A.String -> L.const_string context ""
         | _ -> L.const_int llty 0 
       in
       let g = L.define_global name init the_module in
       StringMap.add name g map)
    StringMap.empty
    globals
  in

let printf_t : L.lltype =
  L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
let printf_func : L.llvalue =
  L.declare_function "printf" printf_t the_module in

   let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    List.fold_left
      (fun m fdecl ->
         let name = fdecl.sfname in
         let formals = Array.of_list (List.map (fun (t,_,_) -> ltype_of_typ t) fdecl.sformals) in
         let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formals in
         let fn = L.define_function name ftype the_module in
         StringMap.add name (fn, fdecl) m)
      StringMap.empty
      functions
    in

let build_function_body fdecl =
  let (the_function, _) = StringMap.find fdecl.sfname function_decls in
  let builder = L.builder_at_end context (L.entry_block the_function) in

  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  let string_fmt_str = L.build_global_stringptr "%s\n" "fmt_str" builder in
  let true_fmt_str  = L.build_global_stringptr "true\n"  "true_str"  builder in
  let false_fmt_str = L.build_global_stringptr "false\n" "false_str" builder in
  let float_fmt_str = L.build_global_stringptr "%f\n" "fmt_float" builder in

  let local_vars =
    let add_formal m (t, n, _ ) p =
      L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p local builder);
      StringMap.add n local m


    and add_local m (t, n, opt_init) =
      let llty = ltype_of_typ t in
      let local_var = L.build_alloca llty n builder in
      (match opt_init with
      | Some (LitInt i)  ->
          ignore (L.build_store (L.const_int   llty i) local_var builder)
      | Some (LitBool b) ->
          ignore (L.build_store (L.const_int   llty (if b then 1 else 0)) local_var builder)
      | Some (LitFloat f)->
          ignore (L.build_store (L.const_float llty f) local_var builder)
      | Some (LitString s) ->
          let strptr = L.build_global_stringptr s "str" builder in
          ignore (L.build_store strptr local_var builder)
      |  None -> if t = A.String then let strptr = L.build_global_stringptr "" "str" builder in
         ignore (L.build_store strptr local_var builder)
      | _ -> ());
      StringMap.add n local_var m
    in

    let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
        (Array.to_list (L.params the_function)) in
    List.fold_left add_local formals fdecl.slocals
  in
 

  let lookup n =
    match StringMap.find_opt n local_vars with
    | Some ptr -> ptr
    | None ->
      (match StringMap.find_opt n global_vars with
       | Some ptr -> ptr
       | None ->
           failwith ("undeclared variable " ^ n)) in 
  let rec build_expr builder ((result_ty, e) : sexpr) = match e with
        SLitInt i  -> L.const_int i32_t i
      | SLitBool b  -> L.const_int i1_t (if b then 1 else 0)
      | SLitFloat f -> L.const_float f64_t f 
      | SId s       -> L.build_load (lookup s) s builder
      | SLitString s -> L.build_global_stringptr s "str" builder
      | SAssign (s, e) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((t1, sx1), op, (t2, sx2)) ->
      
        if t1 <> t2 then
          failwith
            (Printf.sprintf
                "type error in binary op %s: left is %s but right is %s"
                (string_of_op op)
                (string_of_typ t1)
                (string_of_typ t2)
            )
        else
          let v1 = build_expr builder (t1, sx1) in
          let v2 = build_expr builder (t2, sx2) in
          begin match op, t1 with
      
          | Add, Int -> L.build_add v1 v2 "tmp" builder
          | Sub, Int -> L.build_sub v1 v2 "tmp" builder
          | Mul, Int -> L.build_mul v1 v2 "tmp" builder
          | Div, Int -> L.build_sdiv v1 v2 "tmp" builder
          | Mod, Int -> L.build_srem v1 v2 "tmp" builder
    
          | Add, Float -> L.build_fadd v1 v2 "tmp" builder
          | Sub, Float -> L.build_fsub v1 v2 "tmp" builder
          | Mul, Float -> L.build_fmul v1 v2 "tmp" builder
          | Div, Float -> L.build_fdiv v1 v2 "tmp" builder
  
          | Eq,  Int -> L.build_icmp L.Icmp.Eq  v1 v2 "tmp" builder
          | Neq, Int -> L.build_icmp L.Icmp.Ne  v1 v2 "tmp" builder
          | Lt,  Int -> L.build_icmp L.Icmp.Slt v1 v2 "tmp" builder
          | Gte, Int -> L.build_icmp L.Icmp.Sge v1 v2 "tmp" builder
          | Gt,  Int -> L.build_icmp L.Icmp.Sgt v1 v2 "tmp" builder
          | Lte, Int -> L.build_icmp L.Icmp.Sle v1 v2 "tmp" builder
    
          | Eq,    Float -> L.build_fcmp L.Fcmp.Oeq v1 v2 "tmp" builder
          | Neq,   Float -> L.build_fcmp L.Fcmp.One v1 v2 "tmp" builder
          | Lt,    Float -> L.build_fcmp L.Fcmp.Olt v1 v2 "tmp" builder
          | Gte,   Float -> L.build_fcmp L.Fcmp.Oge v1 v2 "tmp" builder
          | Gt,    Float -> L.build_fcmp L.Fcmp.Ogt v1 v2 "tmp" builder
          | Lte,   Float -> L.build_fcmp L.Fcmp.Ole v1 v2 "tmp" builder
  
          
          | And, Bool -> L.build_and v1 v2 "tmp" builder
          | Or,  Bool -> L.build_or  v1 v2 "tmp" builder
    
          | _ ->
              failwith
                (Printf.sprintf
                    "illegal operator %s on type %s"
                    (string_of_op op) (string_of_typ t1))
          end
      | SCall ("print_int", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall ("print_bool", [e]) ->
        let b = build_expr builder e in
        let fmt = L.build_select b true_fmt_str false_fmt_str "tf" builder in
        L.build_call printf_func [| fmt; b |] "printf" builder
      | SCall ("print_string", [e]) ->
        L.build_call printf_func [| string_fmt_str; (build_expr builder e) |]
          "printf" builder
      | SCall ("print_float", [e]) ->
        L.build_call printf_func [| float_fmt_str; (build_expr builder e) |]
          "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
    in
       let add_terminal builder instr =
        match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (instr builder) in
  

      let rec build_stmt builder = function
          SBlock sl -> List.fold_left build_stmt builder sl
        | SExpr e -> ignore(build_expr builder e); builder
        | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
        | SIf (predicate, then_stmt, else_stmt) ->
          let bool_val = build_expr builder predicate in
  
          let then_bb = L.append_block context "then" the_function in
          ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
          let else_bb = L.append_block context "else" the_function in
          ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);
  
          let end_bb = L.append_block context "if_end" the_function in
          let build_br_end = L.build_br end_bb in 
          add_terminal (L.builder_at_end context then_bb) build_br_end;
          add_terminal (L.builder_at_end context else_bb) build_br_end;
  
          ignore(L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context end_bb
  
        | SWhile (predicate, body) ->
          let while_bb = L.append_block context "while" the_function in
          let build_br_while = L.build_br while_bb in 
          ignore (build_br_while builder);
          let while_builder = L.builder_at_end context while_bb in
          let bool_val = build_expr while_builder predicate in
          let body_bb = L.append_block context "while_body" the_function in
          add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;
          let end_bb = L.append_block context "while_end" the_function in
          ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
          L.builder_at_end context end_bb
  
      in
      
      let func_builder = build_stmt builder (SBlock fdecl.sbody) in
  
    
      add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  
    in
  
    List.iter build_function_body functions;
    the_module