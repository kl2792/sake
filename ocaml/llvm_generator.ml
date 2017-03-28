module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

exception Error of string

(* Note: () means "We have to write code here" *)

let translate filename program = (* translate an A.program to LLVM *)
  let context = L.global_context () in
  let sake = L.create_module context "Sake"
    and i32_t  = L.i32_type  context
    and i8_t   = L.i8_type   context
    and i1_t   = L.i1_type   context
    and void_t = L.void_type context in
  let printbig_t =
    L.function_type i32_t [| i32_t |] in
  let print_func =
    L.declare_function "printbig" printbig_t sake in
  let add_terminal builder f = 
    match L.block_terminator (L.insertion_block builder) with
    Some _ -> () | None -> ignore (f builder) in
  let rec expr builder = function
    A.IntLit i -> L.const_int i32_t i
  | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
  |  A.Print e -> L.build_call print_func [| (expr builder e) |] "printbig" builder
(*  | A.CharLit c -> L.const_int i8_t c

      | A.Range -> () (* DON'T NEED FOR HELLO WORLD *)
      | A.ArrayLit -> ()
      | A.StringLit -> ()
      | A.Fsm_call -> ()
*)
      | A.Empty -> L.const_int i32_t 0
(*      | A.Variable s ->  L.build_load (lookup s) s builder *)
      | A.Uop (uop, e) ->
        let e' = expr builder e in
          (match uop with
          A.Neg -> L.build_neg
      | A.Not -> L.build_not
      ) e' "tmp" builder
      | A.Binop (e1, op, e2) ->
          let e1' = expr builder e1
            and e2' = expr builder e2 in
          (match op with
          A.Add -> L.build_add
      | A.Sub -> L.build_sub
        | A.Mul -> L.build_mul
        | A.Div -> L.build_sdiv
        | A.Eq -> L.build_icmp L.Icmp.Eq
        | A.Neq -> L.build_icmp L.Icmp.Ne
        | A.Lt -> L.build_icmp L.Icmp.Slt
        | A.Le -> L.build_icmp L.Icmp.Sle
        | A.Gt -> L.build_icmp L.Icmp.Sgt
        | A.Ge -> L.build_icmp L.Icmp.Sge
        | A.And -> L.build_and
        | A.Or -> L.build_or
      ) e1' e2' "tmp" builder
(*        | A.Assign (s, e) ->
            let e' = expr builder e in
             let _ = L.build_store e' (lookup s) builder in e' *) in
  let rec stmt builder = function
    A.Block body -> List.fold_left stmt builder body
        | A.Expr e -> let _ = expr builder e in builder
(*      | A.If (predicate, then_stmt, else_stmt) -> () 
          let bool_val = expr builder predicate in
          let merge_bb = L.append_block context "merge" the_function in
          (*need to change the_function*)

          let then_bb = L.append_block context "then" the_function in
          add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
          (L.build_br merge_bb);
        (*need to change the_function*)

        let else_bb = L.append_block context "else" the_function in
        add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
        (L.build_br merge_bb);
        (*need to change the_function*)

        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb

      | A.While (predicate, body) -> () 
          let pred_bb = L.append_block context "while" the_function in
          ignore (L.build_br pred_bb builder);
        (*need to change the_function*)

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (stmt (L.builder_at_end context body_bb) body)
        (L.build_br pred_bb);
        (*need to change the_function*)

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr pred_builder predicate in

        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb
        (*need to change the_function*)

      | A.For (name, iter, body) -> ()
      | A.Goto state -> (* TODO: terminate state execution *)() *) in 
  let tick =
    let ftype = L.function_type void_t [| |] in
    L.define_function "tick" ftype sake in
  let main =
    let ftype = L.function_type i32_t [| |] in
    L.define_function "main" ftype sake in
  let tick_build = L.builder_at_end context (L.entry_block tick) in
  let tick_builder = stmt tick_build (A.Block (fst program.A.fsms)) in 
  let tick_terminal = add_terminal tick_build L.build_ret_void in (* return void in tick *)
  let main_build = L.builder_at_end context (L.entry_block main) in
  let main_tick_call = L.build_call tick [| |] "tick" main_build in
  let main_terminal = add_terminal main_build (L.build_ret (L.const_int i32_t 0)) in

  (* let allocation = ()
    (* TODO: allocation block *) in
  let build_fsm = (* TODO: fsm_execution block *)() in
  let writing = (* TODO: block for writing to pointer *)() in *)
  sake

  (* L.function_type to create function (tick) *)
  (* L.insertion_block to indicate where to insert function blocks *)
  (* L.build_at_end to add LLVM statements to L.entry_block *)
  (* L.build_alloca to add LLVM memory allocation *)
  (* L.build_load and L.build_store for the expression builder *)
  (* L.build_(add|sub|mul|sdiv|or|...) for arithmetic/comparison *)
  (* L.build_ret_void for returning void *)
  (* Note: evaluate assignments from right to left, not left to right *)
