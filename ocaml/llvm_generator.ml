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
  (* let enum_init types = (* TODO: enums have to do with integers *)() (* *) in *)
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
      let fsm = L.entry_block in 
      let rec expr builder = function
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
                                            * - predicate (cond br: then/else)
                                            *)
      | A.For (name, iter, body) -> ()
      | A.While (cond, body) -> ()
      | A.Goto state -> () (* TODO: terminate state execution *)
      (* TODO: builds function for calling all the fsm-updating functions,
               uses alloca to allocate save-space for next state *)
  let tick = (* TODO: build tick function *)() in
    sake

  (* L.function_type to create function (tick) *)
  (* L.insertion_block to indicate where to insert function blocks *)
  (* L.build_at_end to add LLVM statements to L.entry_block *)
  (* L.build_alloca to add LLVM memory allocation *)
  (* L.build_load and L.build_store for the expression builder *)
  (* L.build_(add|sub|mul|sdiv|or|...) for arithmetic/comparison *)
  (* L.build_ret_void for returning void *)
  (* Note: evaluate assignments from right to left, not left to right *)
