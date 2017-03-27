module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

exception Error of string

(* Note: () means "We have to write code here" *)

let translate program = (* translate an A.program to LLVM *)
  let context = L.global_context () in
  let sake = L.create_module context "Sake"
    and i32_t  = L.i32_type  context
    and i8_t   = L.i8_type   context
    and i1_t   = L.i1_type   context
    and void_t = L.void_type context in
  let init = function (* initialize primitive *)
  | A.Int -> L.const_int i32_t 0
  | A.Char -> L.const_int i8_t 0
  | A.Bool -> L.const_int i1_t 0
  | _ -> raise (Error "init is for primitives, dude") in
  (* let array_init length = ()
   * (* TODO: something to do with L.array_type : lltype -> int -> lltype *)
   *)
  let enum_init types = (* TODO: enums have to do with integers *)() (* *) in
  let map init types = (* function for generating StringMaps from lists *)
    let iter (dtype, name) = StringMap.add name (L.define_global name init sake) map in
      List.fold_left iter StringMap.empty lvalues in
  let types = map enum_init program.types in (* user-defined types *)
  let inputs = map init program.inputs in (* global inputs for concurrent FSM collection *)
  let outputs = map init program.outputs in (* global outputs for concurrent FSM collection *)
  let locals = map init program.locals in (* fsm write-local state variables *)
  let states =
    let iter map fsm = 
      let state =  in (* TODO: state gen code *)
        StringMap.add fsm.fsm_name name fsm map in
    List.fold_left iter StringMap.empty program.fsms in
  let lookup name = try StringMap.find name with Not_found -> StringMap.find name in
  let build_fsm fsm_decl = (* TODO: builds fsm-updating functions function *)
      let fsm = L.entry_block
        let rec expr builder = function
        | A.Literal i -> L.const_int i32_t i
        | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
        | A.Noexpr -> L.const_int i32_t 0
        | _ -> (* TODO: remainder of expression builder *)() in
        let rec stmt builder = function
        | A.Block body -> List.fold_left stmt builder body
        | A.Expr e -> let _ = expr builder e in builder
        | A.If (cond, body, else_body) -> () (* build in order:
                                              * - merge (the end of the statement)
                                              * - then (br merge)
                                              * - else (br merge)
                                              * - predicate (cond br: then/else) *)
        | A.For name iter body -> ()
        | A.While e 
        | A.Goto state -> () (* TODO: terminate state execution *)
        (* TODO: builds function for calling all the fsm-updating functions,
                   uses alloca to allocate save-space for next state *)
    in sake

  (* L.function_type to create function (tick) *)
  (* L.insertion_block to indicate where to insert function blocks *)
  (* L.build_at_end to add LLVM statements to L.entry_block *)
  (* L.build_alloca to add LLVM memory allocation *)
  (* L.build_load and L.build_store for the expression builder *)
  (* L.build_(add|sub|mul|sdiv|or|...) for arithmetic/comparison *)
  (* L.build_ret_void for returning void *)
  (* Note: evaluate assignments from right to left, not left to right *)





    (* Fill in the body of the given function *)
      let build_fsm_body fsmdecl =
        let (the_fsm, _) = StringMap.find fsmdecl.A.name fsm_decls in
        let builder = L.builder_at_end context (L.entry_block the_fsm) in
        (*****  Include struct for FSM states  *****)
        let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

        (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
      let local_vars =
        let add_formal m (t, n) p = L.set_value_name n p;
  let local = L.build_alloca (ltype_of_typ t) n builder in
  ignore (L.build_store p local builder);
  StringMap.add n local m in

        let add_local m (t, n) =
          let local_var = L.build_alloca (ltype_of_typ t) n builder
  in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

      (* Return the value for a variable or formal argument *)
      let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
      in





    (*
        | Call of string * expr list
  | Fsm_call of string * fsm_call * expr list
  | Cond of expr * expr * expr
  *)



      let rec expr builder = function
        A.Literal.IntLit i -> L.const_int i32_t i
        | A.Literal.CharLit c -> L.const_int i8_t c
      | A.Literal.BoolLit b -> L.const_int i1_t (if b="true" then 1 else 0)
      | A.Empty -> L.const_int i32_t 0
      | A.Variable s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
          let e1' = expr builder e1
    and e2' = expr builder e2 in
          (match op with
      A.Add     -> L.build_add
      | A.Sub     -> L.build_sub
    | A.Mult    -> L.build_mul
    | A.Div     -> L.build_sdiv
    | A.And     -> L.build_and
    | A.Or      -> L.build_or
    | A.Eq   -> L.build_icmp L.Icmp.Eq
    | A.Neq     -> L.build_icmp L.Icmp.Ne
    | A.Lt    -> L.build_icmp L.Icmp.Slt
    | A.Le     -> L.build_icmp L.Icmp.Sle
    | A.Gt -> L.build_icmp L.Icmp.Sgt
    | A.Ge     -> L.build_icmp L.Icmp.Sge
      ) e1' e2' "tmp" builder
    | A.Uop(op, e) ->
        let e' = expr builder e in
        (match op with
      A.Neg     -> L.build_neg
    | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
      ignore (L.build_store e' (lookup s) builder); e'
      (*      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
        L.build_call printf_func [| int_format_str ; (expr builder e) |]
      "printf" builder
      | A.Call ("printbig", [e]) ->
          L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | A.Call (f, act) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
      let actuals = List.rev (List.map (expr builder) (List.rev act)) in
      let result = (match fdecl.A.typ with A.Void -> ""
      | _ -> f ^ "_result") in
      L.build_call fdef (Array.of_list actuals) result builder
      in
    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
      let add_terminal builder f =
        match L.block_terminator (L.insertion_block builder) with
  Some _ -> ()
      | None -> ignore (f builder) in *)

      (* Build the code for the given statement; return the builder for
       the statement's successor *)
      let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
    A.Void -> L.build_ret_void builder
      | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
          let bool_val = expr builder predicate in
          let merge_bb = L.append_block context "merge" the_function in

          let then_bb = L.append_block context "then" the_function in
          add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
          (L.build_br merge_bb);

   let else_bb = L.append_block context "else" the_function in
   add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
   (L.build_br merge_bb);

   ignore (L.build_cond_br bool_val then_bb else_bb builder);
   L.builder_at_end context merge_bb



      | A.Switch (predicate, list_cases) -> (* not done yet *)

          let switch_val = expr builder predicate
      in
      let switch = L.build_switch switch_val elsebb (List.length list_cases) builder;;

let helper case_num = function
  let stmt_bb = (L.append_block context "case_num" the_function)
  in
       add_terminal (stmt (L.builder_at_end context ) stmt_num) (L.build_br
  in
       L.add_case switch case_num stmt_bb
in  List.iter helper list_cases


      | A.Goto (dest) ->




          | A.While (predicate, body) ->
              let pred_bb = L.append_block context "while" the_function in
              ignore (L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" the_function in
    add_terminal (stmt (L.builder_at_end context body_bb) body)
    (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = expr pred_builder predicate in

    let merge_bb = L.append_block context "merge" the_function in
    ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb

          | A.For (e1, e2, e3, body) -> stmt builder
      ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
          | t -> L.build_ret (var_init t))
    in

  List.iter build_function_body functions;
  the_module












  (********************NOTHING BELOW THIS IS REAL***********************)


  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Declare the built-in printbig() function *)
      let printbig_t = L.function_type i32_t [| i32_t |] in
      let printbig_func = L.declare_function "printbig" printbig_t the_module in

      (* Define each function (arguments and return type) so we can call it *)
      let function_decls =
        let function_decl m fdecl =
          let name = fdecl.A.fname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
        List.fold_left function_decl StringMap.empty functions in

      (* Fill in the body of the given function *)
      let build_function_body fdecl =
        let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
        let builder = L.builder_at_end context (L.entry_block the_function) in

        let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

        (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
      let local_vars =
        let add_formal m (t, n) p = L.set_value_name n p;
  let local = L.build_alloca (ltype_of_typ t) n builder in
  ignore (L.build_store p local builder);
  StringMap.add n local m in

        let add_local m (t, n) =
          let local_var = L.build_alloca (ltype_of_typ t) n builder
  in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

      (* Return the value for a variable or formal argument *)
      let lookup n = try StringMap.find n local_vars
    with Not_found -> StringMap.find n global_vars
      in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
      A.Literal i -> L.const_int i32_t i
          | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
          | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
          let e1' = expr builder e1
    and e2' = expr builder e2 in
          (match op with
      A.Add     -> L.build_add
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
      ) e1' e2' "tmp" builder
    | A.Unop(op, e) ->
        let e' = expr builder e in
        (match op with
      A.Neg     -> L.build_neg
    | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
      ignore (L.build_store e' (lookup s) builder); e'
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
          L.build_call printf_func [| int_format_str ; (expr builder e) |]
          "printf" builder
      | A.Call ("printbig", [e]) ->
          L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | A.Call (f, act) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (expr builder) (List.rev act)) in
          let result = (match fdecl.A.typ with A.Void -> ""
      | _ -> f ^ "_result") in
          L.build_call fdef (Array.of_list actuals) result builder
          in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
  Some _ -> ()
      | None -> ignore (f builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
      A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
    A.Void -> L.build_ret_void builder
      | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
          let bool_val = expr builder predicate in
          let merge_bb = L.append_block context "merge" the_function in

          let then_bb = L.append_block context "then" the_function in
          add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
          (L.build_br merge_bb);

   let else_bb = L.append_block context "else" the_function in
   add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
   (L.build_br merge_bb);

   ignore (L.build_cond_br bool_val then_bb else_bb builder);
   L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore (L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" the_function in
    add_terminal (stmt (L.builder_at_end context body_bb) body)
    (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = expr pred_builder predicate in

    let merge_bb = L.append_block context "merge" the_function in
    ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
      ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
    in

  List.iter build_function_body functions;
  the_module
