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
  | A.Enum -> L.const_int i32_t 0
  | A.Char -> L.const_int i8_t 0
  | A.Bool -> L.const_int i1_t 0
  | _ -> raise (Error "init is for primitives, dude") in (* let array_init length = () *)
  let map init lvalues = (* function for generating StringMaps from lists *)
    let iter map (dtype, name) =
      StringMap.add name (L.define_global name (init dtype) sake) map in
    List.fold_left iter StringMap.empty lvalues in
  let inputs = map (init dtype) program.A.inputs in (* global inputs for concurrent FSM collection *)
  let outputs = map (init dtype) program.A.outputs in (* global outputs for concurrent FSM collection *)
  let locals = map (init dtype) program.A.locals in (* fsm write-local state variables *)
  let types = map (init dtype) program.A.types in (* user-defined types *)
  let states =
    let iter map fsm =(* TODO: state gen code *)
      let init_fsm fsm = (**)
        let rec iter_state map count = function
          | [] -> map
          | state::tl -> StringMap.add state.A.state_name (L.const_int i32_t count) map;
              f map (count + 1) tl in
        List.fold_left iter_state StringMap.empty 0 fsm.A.fsm_body in
      StringMap.add fsm.A.fsm_name (L.define_global fsm.A.fsm_name (init_fsm fsm) sake) map in
    List.fold_left iter StringMap.empty program.A.fsms in
  let lookup_enum enum name = 
    let name_map = StringMap.find states enum in
    StringMap.find name name_map
  (* TODO: lookup enums' value's integral representations *) in
  let lookup name = try StringMap.find name with Not_found -> StringMap.find name in
  let build_fsm fsm_decl = (* TODO: builds fsm-updating functions function *)
    let fsm = L.entry_block in
    let rec expr builder = function
      | A.IntLit i -> L.const_int i32_t i
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.CharLit c -> L.const_int i8_t c
      (* DON'T NEED FOR HELLO WORLD *)
      | A.Range -> ()
      | A.ArrayLit -> ()
      | A.StringLit -> ()
      | A.Fsm_call -> ()

      | A.Empty -> L.const_int i32_t 0
      | A.Variable s -> L.build_load (lookup s) s builder
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
        | A.Mult -> L.build_mul
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
        | A.Assign (s, e) ->
            let e' = expr builder e in
            ignore (L.build_store e' (lookup s) builder); e'
            in

      let rec stmt builder = function
        | A.Block body -> List.fold_left stmt builder body
      | A.Expr e -> let _ = expr builder e in builder
      | A.If (predicate, then_stmt, else_stmt) ->    (* build in order:
        * - merge (the end of the statement)
                                            * - then (br merge)
                                            * - else (br merge)
                                            * - predicate (cond br: then/else)
                                            *)

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

      | A.While (predicate, body) ->
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
      | A.Goto state -> (* TODO: terminate state execution *)() in 
      (* TODO: build function for calling all the fsm-updating functions,
               use alloca to allocate save-space for next state *)() in
  let tick = L.define_function  (* TODO: build tick function *)() in
    sake

  (* L.function_type to create function (tick) *)
  (* L.insertion_block to indicate where to insert function blocks *)
  (* L.build_at_end to add LLVM statements to L.entry_block *)
  (* L.build_alloca to add LLVM memory allocation *)
  (* L.build_load and L.build_store for the expression builder *)
  (* L.build_(add|sub|mul|sdiv|or|...) for arithmetic/comparison *)
  (* L.build_ret_void for returning void *)
  (* Note: evaluate assignments from right to left, not left to right *)
