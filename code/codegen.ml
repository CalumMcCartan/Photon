(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

Project Name: Photon

*)

module L = Llvm
module A = Ast

open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context

  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  and the_module = L.create_module context "Photon" in

  let str_t = L.pointer_type i8_t in

  (* Convert Photon types to LLVM types *)
  let ltype_of_typ = function
    A.Int_   -> i32_t
    | A.Str_ -> str_t
  in 

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module 

  in
  let string_format_str = L.build_global_stringptr "%s\n" "fmt"

in

(* this seems to be the recommended approach for compiling a list of function declarations
  however, this would require a significant change to the way in which function declarations
  were established. unsure of the route to take
*)

(*
    (* Define each function (arguments and return type) so we can 
    define it's body and call it later *)
  let function_decls : (L.llvalue * sfdecl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let str_format_str = 
    and int_format_str = L.build_global_stringptr "%d\n" "int_fmt" builder in
    *)

  (* Construct code for an expression; return its value *)

    let rec expr builder ((_, e) : sexpr) = match e with
    SInt i  -> L.const_int i32_t i
    | SStr s -> L.build_global_stringptr s "tmp" builder
    | SObjFunc(print, e) -> L.build_call printf_func [| string_format_str ; (expr builder e) |]
    (* "printf" builder *)

  in
  (*

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) in


  *)
    the_module; the_module


