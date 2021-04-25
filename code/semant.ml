(* Semantic checking for the Photon compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) = List.iter (function
	  | (Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
    | _ -> ()
    ) binds;
    let rec dups = function
      | [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 -> raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)

  check_binds "global" globals;

  (**** Check functions ****)

  
  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, formals', rtype) = StringMap.add name {
        (* object between brackets is func_decl object? *)
        typ = rtype;
        fname = name;
        formals = formals';
        locals = [];
        body = []; (* empty list *)
    } map
    in List.fold_left add_bind StringMap.empty [ 
      ("print", [(Int, "x")], Void);
      ("printb", [(Bool, "x")], Void);
      ("printf", [(Float, "x")], Void);
      ("prints", [(String, "x")], Void);
      ("printbig", [(Int, "x")], Void);
      ("min", [(Int, "x");(Int, "y")], Int);
      ("max", [(Int, "x");(Int, "y")], Int);
      ("sqrt", [(Float, "x")], Float);
      ("load", [(String, "x")], Image); 
      ("save", [(Image, "img"); (String, "fname")], Void); 
      ("create", [(Int, "w"); (Int, "h"); (Pint, "r"); (Pint, "g"); (Pint, "b"); (Pint, "a");], Image); 
      ("destroy", [(Image, "img")], Void); 
      ("flip", [(Image, "img")], Image); 
      ("to_gray", [(Image, "img")], Image); 
      ("image_paste", [(Image, "target"); (Image, "orig") ], Image); 
      ("get_pixel", [(Image, "img"); (Int, "x")], Int);
      ("set_pixel", [(Image, "img"); (Int, "x"); (Int, "x"); (Int, "x"); (Int, "x"); (Int, "x")], Int);
      ("width", [(Image, "img")], Int);
      ("height", [(Image, "img")], Int)

      ]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
      the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if (lvaluet = rvaluet) then lvaluet else
      match lvaluet, rvaluet with
        (* Types must be the same or one of these combos *)
        | Pint, Int
        | Int, Pint
        | Float, Pint
        | Float, Int -> lvaluet
        | _ -> raise (Failure err)
    in   

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
      StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let check_array_type id = 
      match (type_of_identifier id) with 
          Array t -> t
        | t -> raise (Failure ("check array type error, typ: " ^ string_of_typ t))
    in

    let combine_numeric_types t1 t2 =
      if t1 = t2 then t1 else match t1, t2 with
        (* Use the more precise type *)
        | Int, Float  | Float, Int  -> Float
        | Pint, Float | Float, Pint -> Float
        | Pint, Int   | Int, Pint   -> Int
        | _ -> raise (Failure ("cannot combine numeric types " ^ string_of_typ t1 ^ " & " ^ string_of_typ t2))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Literal l -> (Int, SLiteral l)
      | Fliteral l -> (Float, SFliteral l)
      | StrLiteral l -> (String, SStrLiteral l)
      | Alias n -> 
        let (r, g, b, a) = (match n with 
          | "_black"   -> (0, 0, 0, 255)
          | "_white"   -> (255, 255, 255, 255)
          | "_grey"    -> (128, 128, 128, 255)
          | "_red"     -> (255, 0, 0, 255)
          | "_green"   -> (0, 255, 0, 255)
          | "_blue"    -> (0, 0, 255, 255)
          | "_cyan"    -> (0, 255, 255, 255)
          | "_magenta" -> (255, 0, 255, 255)
          | "_yellow"  -> (255, 255, 0, 255)
          | _ -> raise (Failure ("alias " ^ n ^ " does not exist"))
        ) in expr (ArrayLiteral ([Literal(r); Literal(g); Literal(b); Literal(a)]))
      | BoolLit l  -> (Bool, SBoolLit l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          let same = t1 = t2 in
          let both_numeric = 
            ((t1 = Int) || (t1 = Pint) || (t1 = Float)) &&
            ((t2 = Int) || (t2 = Pint) || (t2 = Float))
          in
          (* Determine expression type based on operator and operand types *)
          (* Math ops require any two numeric types, logical ops require two bools *)
          let ty = match op with
          | Add | Sub | Mult | Div       when both_numeric -> combine_numeric_types t1 t2
          | Less | Leq | Greater | Geq   when both_numeric -> Bool
          | Equal | Neq                  when same || both_numeric -> Bool
          | And | Or                     when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | ArrayGet (var, e) -> 
         let (t, e') = expr e in
         let ty = match t with 
             Int -> Int
             | _ -> raise (Failure ("array_get index must be integer, not " ^ string_of_typ t)) 
         in let array_type = check_array_type var
         in (array_type, SArrayGet(array_type, var, (ty, e')))
      | ArraySize var -> 
          (Int, SArraySize(check_array_type var, var))
      | ArrayLiteral vals ->
         let (t', _) = expr (List.hd vals) in
         let map_func lit = expr lit in
         let vals' = List.map map_func vals in
         (* TODO: check that all vals are of the same type *)
         (Array t', SArrayLiteral(t', vals'))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, (et, e'))
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in
    let check_int_expr e = 
      let (t', e') = expr e
      and err = "expected Integer expression in " ^ string_of_expr e
      in if t' != Int then raise (Failure err) else (t', e') 
    in
  
    let check_match_array_type_expr l e = 
      let (t', e') as e'' = expr e in
      let err = "array type and expression type do not match " ^ (string_of_typ t') ^ ", " ^ (string_of_sexpr e'') in
      if t' != (check_array_type l) then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
        | ArraySet (var, e1, e2) ->
          SArraySet(check_array_type var, var, check_int_expr e1, check_match_array_type_expr var e2)
        | ArrayAdd (var, e) -> 
          let _ = check_array_type var in
          SArrayAdd(var, check_match_array_type_expr var e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        let err = ("return gives " ^ string_of_typ t ^ " expected " ^ string_of_typ func.typ ^ " in " ^ string_of_expr e) in
        if check_assign func.typ t err = func.typ then SReturn (t, e')
        else raise (Failure err)
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = match check_stmt (Block func.body) with
        | SBlock(sl) -> sl
        | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
