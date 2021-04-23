(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Photon" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and string_t   = L.pointer_type (L.i8_type context)
  and void_t     = L.void_type   context 
  and image_t   = L.pointer_type (L.named_struct_type context "PImage")
  and array_t t   = L.struct_type context [| L.pointer_type (L.i32_type context); (L.pointer_type t) |]
  in

  (* Return the LLVM type for a Photon type *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Pint  -> i8_t 
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> string_t
    | A.Image -> image_t
    | A.Array t -> array_t (ltype_of_typ t)
  in
  let type_str t = 
    match t with
       A.Int -> "int"
     | A.Pint -> "pint"
     | A.Bool -> "bool"
     | A.Float -> "float"
     | A.String -> "str"
     | _ -> raise (Failure "Invalid string map key type")
  in
  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m 
    in List.fold_left global_var StringMap.empty globals 
  in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in
  
  let getmin_t : L.lltype =
    L.function_type i32_t [| i32_t; i32_t |] in
  let getmin_func : L.llvalue =
    L.declare_function "get_min" getmin_t the_module in
  
  let getmax_t : L.lltype =
    L.function_type i32_t [| i32_t; i32_t |] in
  let getmax_func : L.llvalue =
    L.declare_function "get_max" getmax_t the_module in
  
  let getsqrt_t : L.lltype =
    L.function_type i8_t [| i8_t |] in
  let getsqrt_func : L.llvalue =
    L.declare_function "get_sqrt" getsqrt_t the_module in

  let loadimage_t : L.lltype =
    L.function_type image_t [| string_t |] in
  let loadimage_func : L.llvalue =
    L.declare_function "Image_load" loadimage_t the_module 
  in

  (* LLVM insists each basic block end with exactly one "terminator" 
      instruction that transfers control.  This function runs "instr builder"
      if the current block does not already have a terminator.  Used,
      e.g., to handle the "fall off the end of the function" case. *)

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder) 
  in

  (*construct necessary dependencies for built in array functions *)

  let build_while builder build_predicate build_body func_def =
    let pred_bb = L.append_block context "while" func_def in
    ignore(L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" func_def in
    add_terminal (build_body (L.builder_at_end context body_bb)) (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = build_predicate pred_builder in

    let merge_bb = L.append_block context "merge" func_def in
    ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb
  in

  let build_if builder build_predicate build_then_stmt build_else_stmt func_def =
    let bool_val = build_predicate builder in
    let merge_bb = L.append_block context "merge" func_def in
    let build_br_merge = L.build_br merge_bb in (* partial function *)

    let then_bb = L.append_block context "then" func_def in
    add_terminal (build_then_stmt (L.builder_at_end context then_bb)) 
    build_br_merge;

    let else_bb = L.append_block context "else" func_def in
    add_terminal (build_else_stmt (L.builder_at_end context else_bb)) 
    build_br_merge;

    ignore(L.build_cond_br bool_val then_bb else_bb builder);
    L.builder_at_end context merge_bb
  in

  (* array functions *)

  (* 
    ltype array_get(array a, i32_t index) 
  *)
  let array_get : L.llvalue StringMap.t = 
    let array_get_ty m typ = 
      let ltype = (ltype_of_typ typ) in 

      let def_name = (type_str typ) in
      let def = L.define_function ("array_get" ^ def_name) (L.function_type ltype [| L.pointer_type (array_t ltype); i32_t |]) the_module in

      let build = L.builder_at_end context (L.entry_block def) in
      let array_ptr = L.build_alloca (L.pointer_type (array_t ltype)) "array_ptr_alloc" build in

      let _ = L.build_store (L.param def 0) array_ptr build in
      let index_ptr = L.build_alloca i32_t "index_alloc" build in

      let _ = L.build_store (L.param def 1) index_ptr build in
      let array_load = L.build_load array_ptr "array_load" build in
      let array_ar_ptr = L.build_struct_gep array_load 1 "array_ar_ptr" build in
      let array_ar_load = L.build_load array_ar_ptr "array_load" build in
      let index = L.build_load index_ptr "index_load" build in

      let array_ar_elem_ptr = L.build_gep array_ar_load [| index |] "list_arry_element_ptr" build in
      let ele_val = L.build_load array_ar_elem_ptr "array_ar_elem_ptr" build in
      let _ = L.build_ret ele_val build in
      StringMap.add def_name def m in
     
  List.fold_left array_get_ty StringMap.empty [ A.Bool; A.Int; A.Float; A.String ] in

  (* void array_set(array a, i32_t index, ltype value) *)
  let array_set : L.llvalue StringMap.t = 
    let array_set_ty m typ =

     let ltype = (ltype_of_typ typ) in 

     let def_name = (type_str typ) in

     let def = L.define_function ("array_set" ^ def_name) (L.function_type void_t [| L.pointer_type (array_t ltype); i32_t; ltype |]) the_module in
     let build = L.builder_at_end context (L.entry_block def) in
     
     let array_ptr = L.build_alloca (L.pointer_type (array_t ltype)) "array_ptr_alloc" build in
     ignore(L.build_store (L.param def 0) array_ptr build);

     let array_load = L.build_load array_ptr "array_load" build in
     let array_ar_ptr = L.build_struct_gep array_load 1 "array_ar_ptr" build in
     let array_ar_load = L.build_load array_ar_ptr "array_ar_load" build in

     let index_element_ptr = L.build_gep array_ar_load [| L.param def 1 |] "array_ar_next_ele_ptr" build in
     let _ = L.build_store (L.param def 2) index_element_ptr build in
     let _ = L.build_ret_void build in
     StringMap.add def_name def m in 
  List.fold_left array_set_ty StringMap.empty [ A.Bool; A.Int; A.Float; A.String ] in

  (* void array_push(array, ltype value) *) 
  
  let array_push_ty m typ =
    let ltype = (ltype_of_typ typ) in 
    let def_name = (type_str typ) in
    let def = L.define_function ("array_push" ^ def_name) (L.function_type void_t [| L.pointer_type (array_t ltype); ltype |]) the_module in
    let build = L.builder_at_end context (L.entry_block def) in
    let array_ptr = L.build_alloca (L.pointer_type (array_t ltype)) "array_ptr_alloc" build in
    ignore(L.build_store (L.param def 0) array_ptr build);
    let valPtr = L.build_alloca ltype "val_alloc" build in
    ignore(L.build_store (L.param def 1) valPtr build);
    let array_load = L.build_load array_ptr "array_load" build in

    let array_ar_ptr = L.build_struct_gep array_load 1 "array_ar_ptr" build in
    let array_ar_load = L.build_load array_ar_ptr "array_ar_load" build in
    let array_size_ptr_ptr = L.build_struct_gep array_load 0 "array_size_ptr_ptr" build in 
    let array_size_ptr = L.build_load array_size_ptr_ptr "array_size_ptr" build in
    let array_size = L.build_load array_size_ptr "array_size" build in

    let next_index = array_size in
    let next_element_ptr = L.build_gep array_ar_load [| next_index |] "array_ar_next_ele_ptr" build in
    let next_size = L.build_add array_size (L.const_int i32_t 1) "inc_size" build in
    let _ = L.build_store next_size array_size_ptr build in
    let _ = L.build_store (L.build_load valPtr "val" build) next_element_ptr build in
    let _ = L.build_ret_void build in
    StringMap.add def_name def m in 
  let array_push : L.llvalue StringMap.t =
    List.fold_left array_push_ty StringMap.empty [ A.Bool; A.Int; A.Float; A.String ] in
 
  (* i32_t array_size(array a) *)
  let array_size : L.llvalue StringMap.t = 
  let array_size_ty m typ =
  let ltype = (ltype_of_typ typ) in 
  let def_name = (type_str typ) in

  let def = L.define_function ("array_size" ^ def_name) (L.function_type i32_t [| L.pointer_type (array_t ltype) |]) the_module in
  let build = L.builder_at_end context (L.entry_block def) in

  let array_ptr = L.build_alloca (L.pointer_type (array_t ltype)) "array_ptr_alloc" build in
  ignore(L.build_store (L.param def 0) array_ptr build);

  let array_load = L.build_load array_ptr "array_load" build in

  let array_size_ptr_ptr = L.build_struct_gep array_load 0 "array_size_ptr_ptr" build in 
  let array_size_ptr = L.build_load array_size_ptr_ptr "array_size_ptr" build in
  let array_size = L.build_load array_size_ptr "array_size" build in
  ignore(L.build_ret array_size build);
  StringMap.add def_name def m in 
  List.fold_left array_size_ty StringMap.empty [ A.Bool; A.Int; A.Float; A.String ] in

  (* building the array *)
  let init_array builder array_ptr array_type = 
    (* initialize size to 0 *)
    let sizePtrPtr = L.build_struct_gep array_ptr 0 "array_size_ptr" builder in 
      let sizePtr = L.build_alloca i32_t "array_size" builder in
      let _ = L.build_store (L.const_int i32_t 0) sizePtr builder in
      ignore(L.build_store sizePtr sizePtrPtr builder);
    (* initialize array *)
    let array_ar_ptr = L.build_struct_gep array_ptr 1 "list.arry" builder in 
    (* TODO: allocate nothing and have list grow dynamically as necessary when pushing into the list *)
      let p = L.build_array_alloca (ltype_of_typ array_type) (L.const_int i32_t 1028) "p" builder in
      ignore(L.build_store p array_ar_ptr builder);
  in

  (* i32_t array_find(array, val) *)
  let array_find : L.llvalue StringMap.t = 
    let array_find_ty m typ =
      let ltype = (ltype_of_typ typ) in 
      let def_name = (type_str typ) in

      let def = L.define_function ("array_find" ^ def_name) (L.function_type i32_t [| L.pointer_type (array_t ltype); ltype |]) the_module in
      let build = L.builder_at_end context (L.entry_block def) in
      let array_ptr = L.build_alloca (L.pointer_type (array_t ltype)) "array_ptr_alloc" build in
      ignore(L.build_store (L.param def 0) array_ptr build);

      let find_value_ptr = L.build_alloca ltype "find_val_alloc" build in
      ignore(L.build_store (L.param def 1) find_value_ptr build);
      let find_value = L.build_load find_value_ptr "find_val" build in

      let array_load = L.build_load array_ptr "array_load" build in
      let array_size_ptr_ptr = L.build_struct_gep array_load 0 "array_size_ptr_ptr" build in 
      let array_size_ptr = L.build_load array_size_ptr_ptr "array_size_ptr" build in
      let array_size = L.build_load array_size_ptr "array_size" build in
      let loop_index_ptr = L.build_alloca i32_t "loop_cnt" build in
      let _ = L.build_store (L.const_int i32_t 0) loop_index_ptr build in
      let loop_upper_bound = array_size in
      let loop_cond _builder = 
          L.build_icmp L.Icmp.Slt (L.build_load loop_index_ptr "loop_iter_cnt" _builder) loop_upper_bound "loop_cond" _builder
      in
      let loop_body _builder = 
        let index = L.build_load loop_index_ptr "to_index" _builder in
        let get_val = L.build_call (StringMap.find (type_str typ) array_get) [| array_load; index |] "array_get" _builder in
        let if_cond _builder2 = 
            (match typ with
                A.Int | A.Bool -> L.build_icmp L.Icmp.Eq 
              | A.Float -> L.build_fcmp L.Fcmp.Oeq
              | _ -> raise (Failure ("array_find does not support this array type"))
            ) get_val find_value "if_cond" _builder2 
        in
        let if_body _builder2 = ignore(L.build_ret index _builder2); _builder2 in
        let else_body _builder2 = ignore(L.const_int i32_t 0); _builder2 in
        let if_builder = build_if _builder if_cond if_body else_body def in
        let index_incr = L.build_add (L.build_load loop_index_ptr "loop_index" if_builder) (L.const_int i32_t 1) "loop_itr" if_builder in
        let _ = L.build_store index_incr loop_index_ptr if_builder in 
        if_builder
      in
      let while_builder = build_while build loop_cond loop_body def in
      ignore(L.build_ret (L.const_int i32_t (-1)) while_builder);
      StringMap.add def_name def m in 
    List.fold_left array_find_ty StringMap.empty [ A.Bool; A.Int; A.Float ] 
  in

  (*Image Functions*)


  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals) in
      let 
        ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types 
      in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m 
    in
    List.fold_left function_decl StringMap.empty functions 
  in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
	      let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore(
          match t with 
            A.Array array_type -> init_array builder local array_type
          | _ -> ()
        );
        ignore (L.build_store p local builder);
        StringMap.add n local m 
        (* Allocate space for any locally declared variables and add the
        * resulting registers to our map *)
        and add_local m (t, n) =
	      let local_var = L.build_alloca (ltype_of_typ t) n builder in 
        ignore(
          match t with 
            A.Array array_type -> init_array builder local_var array_type
          | _ -> ()
        );
        StringMap.add n local_var m in
        let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals 
          (Array.to_list (L.params the_function))
        in List.fold_left add_local formals fdecl.slocals 
      in

  let clamp v min max =
    if v > max then max else
    if v < min then min else v
  in

  (* Return the value for a variable or formal argument. Check local names first, then global names *)
  let lookup n = 
    try StringMap.find n local_vars with Not_found -> StringMap.find n global_vars
  in

  (* Cast an evaluated expression 'e' from type 'rt' to type 'lt' *)
  let cast_expr e lt rt builder = 
    if lt = rt then e else
    let llt = ltype_of_typ lt in
    match lt, rt with
      | A.Pint, A.Int   -> L.build_trunc e llt "pintCast" builder
      | A.Int, A.Pint   -> L.build_zext e llt "intCast" builder
      | A.Float, A.Pint -> L.build_uitofp e llt "floatCast" builder
      | A.Float, A.Int  -> L.build_sitofp e llt "floatCast" builder
      | _ -> raise (Failure "internal error: semant should have rejected an unsupported type conversion")
  in

  (* Construct code for an expression; return its value *)
  let rec expr builder ((t, e) : sexpr) = match e with
    | SLiteral i  -> L.const_int i32_t i
    | SPintLit p  -> L.const_int i8_t (clamp p 0 255)
    | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
    | SFliteral l -> L.const_float_of_string float_t l
    | SStrLiteral s   -> L.build_global_stringptr s "str" builder
    | SNoexpr     -> L.const_int i32_t 0
    | SId s       -> L.build_load (lookup s) s builder
    | SAssign (s, (rt, e)) -> 
        let e' = expr builder (rt, e) in
        let e' = cast_expr e' t rt builder in 
        ignore(L.build_store e' (lookup s) builder); e'
    | SBinop ((rt1, e1), op, (rt2, e2)) -> 
      (* If binop type is a bool, then cast both expressions to float for comparision *)
      let cast_t = if t = A.Bool then 
        if rt1 = rt2 then rt1 else A.Float 
        else t 
      in
      (* Evaluate both expressions and cast to same type 'cast_t' *)
      let e1' = expr builder (rt1, e1)
      and e2' = expr builder (rt2, e2) in 
      let e1' = cast_expr e1' cast_t rt1 builder
      and e2' = cast_expr e2' cast_t rt2 builder in

      if cast_t = A.Float then (match op with
        | A.Add     -> L.build_fadd
        | A.Sub     -> L.build_fsub
        | A.Mult    -> L.build_fmul
        | A.Div     -> L.build_fdiv
        | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
        | A.Neq     -> L.build_fcmp L.Fcmp.One
        | A.Less    -> L.build_fcmp L.Fcmp.Olt
        | A.Leq     -> L.build_fcmp L.Fcmp.Ole
        | A.Greater -> L.build_fcmp L.Fcmp.Ogt
        | A.Geq     -> L.build_fcmp L.Fcmp.Oge
        | A.And | A.Or -> raise (Failure "internal error: semant should have rejected and/or on float")
        ) e1' e2' "floatBinop" builder
      else (match op with
        | A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "nonFloatBinop" builder
    | SUnop(op, ((t, _) as e)) ->
      (* Unop *)
      let e' = expr builder e in (match op with
          A.Neg when t = A.Float -> L.build_fneg 
        | A.Neg                  -> L.build_neg
        | A.Not                  -> L.build_not) e' "tmp" builder
  
    (* array functions *)
    | SArrayGet (array_type, id, e) ->
      L.build_call (StringMap.find (type_str array_type) array_get) [| (lookup id); (expr builder e) |] "array_get" builder
    | SArraySize (array_type, id) -> 
      L.build_call ((StringMap.find (type_str array_type)) array_size) [| (lookup id) |] "array_size" builder
    | SArrayFind (array_type, id, e) ->
      L.build_call (StringMap.find (type_str array_type) array_find) [| (lookup id); (expr builder e) |] "array_find" builder
    | SArrayLiteral (array_type, literals) ->
      let ltype = (ltype_of_typ array_type) in
      let new_array_ptr = L.build_alloca (array_t ltype) "new_array_ptr" builder in
      let _ = init_array builder new_array_ptr array_type in
      let map_func literal = 
          ignore(L.build_call (StringMap.find (type_str array_type) array_push) [| new_array_ptr; (expr builder literal) |] "" builder);
      in
      let _ = List.rev (List.map map_func literals) in
      L.build_load new_array_ptr "new_array" builder
      
    (* built in functions *)
    | SCall ("print", [e]) | SCall ("printb", [e]) ->
      L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
    | SCall ("printbig", [e]) ->
      L.build_call printbig_func [| (expr builder e) |] "printbig" builder
    | SCall ("printp",[e]) ->
      L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
    | SCall ("printf", [e]) -> 
      L.build_call printf_func [| float_format_str ; (expr builder e) |] "printf" builder
    | SCall ("prints",[e]) ->
      L.build_call printf_func [| str_format_str ; (expr builder e) |] "printf" builder
    | SCall ("min", [e1; e2]) ->
      L.build_call getmin_func [| (expr builder e1); (expr builder e2) |] "get_min" builder
    | SCall ("max", [e1; e2]) ->
      L.build_call getmax_func [| (expr builder e1); (expr builder e2) |] "get_max" builder
    | SCall ("sqrt", [e]) ->
      L.build_call getsqrt_func [| (expr builder e) |] "get_sqrt" builder
    | SCall ("load", [e]) ->
      L.build_call loadimage_func [| (str_format_str) |] "load" builder
    | SCall (f, args) ->
      let (fdef, fdecl) = StringMap.find f function_decls in
      let llargs = List.rev (List.map (expr builder) (List.rev args)) in
      let result = (match fdecl.styp with 
        | A.Void -> ""
        | _ -> f ^ "_result"
      ) in L.build_call fdef (Array.of_list llargs) result builder
  in
  
  
  (* Build the code for the given statement; return the builder for
      the statement's successor (i.e., the next instruction will be built
      after the one generated by this call) *)

  let rec stmt builder = function
	  | SBlock sl -> List.fold_left stmt builder sl
    | SArrayPush (id, e) -> 
        ignore(L.build_call (StringMap.find (type_str (fst e)) array_push) [| (lookup id); (expr builder e) |] "" builder); builder 
    | SArraySet (array_type, id, e1, e2) ->
        ignore(L.build_call (StringMap.find (type_str array_type) array_set) [| (lookup id); (expr builder e1); (expr builder e2) |] "" builder); builder
    | SExpr e -> ignore(expr builder e); builder 
    | SReturn e -> 
        ignore(match fdecl.styp with
          (* Special "return nothing" instr *)
          | A.Void -> L.build_ret_void builder 
          (* Build return statement *)
          | _ -> L.build_ret (expr builder e) builder 
        ); builder
    | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = expr builder predicate in
	      let merge_bb = L.append_block context "merge" the_function in
        let build_br_merge = L.build_br merge_bb in (* partial function *)

        let then_bb = L.append_block context "then" the_function in
        add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
          build_br_merge;

        let else_bb = L.append_block context "else" the_function in
        add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
          build_br_merge;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb

    | SWhile (predicate, body) ->
      let pred_bb = L.append_block context "while" the_function in
      ignore(L.build_br pred_bb builder);

      let body_bb = L.append_block context "while_body" the_function in
      add_terminal (stmt (L.builder_at_end context body_bb) body)
        (L.build_br pred_bb);

      let pred_builder = L.builder_at_end context pred_bb in
      let bool_val = expr pred_builder predicate in

      let merge_bb = L.append_block context "merge" the_function in
      ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
      L.builder_at_end context merge_bb

    (* Implement for loops as while loops *)
    | SFor (e1, e2, e3, body) -> 
      stmt builder ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
  in

  (* Build the code for each statement in the function *)
  let builder = stmt builder (SBlock fdecl.sbody) in

  (* Add a return if the last block falls off the end *)
  add_terminal builder (match fdecl.styp with
      A.Void -> L.build_ret_void
    | A.Float -> L.build_ret (L.const_float float_t 0.0)
    | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
