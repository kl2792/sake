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
  | _ -> raise (Error "init is for primitives, dude") in
  (* let array_init length = ()
   * (* TODO: something to do with L.array_type : lltype -> int -> lltype *)
   *)
  (* let enum_init types = (* TODO: enums have to do with integers *)() (* *) in *)
  let map init lvalues = (* function for generating StringMaps from lists *)
    let iter map (dtype, name) = StringMap.add name (L.define_global name (init dtype) sake) map in
    List.fold_left iter StringMap.empty lvalues in
  let inputs = map (init dtype) program.A.inputs in (* global inputs for concurrent FSM collection *)
  let outputs = map (init dtype) program.A.outputs in (* global outputs for concurrent FSM collection *)
  let locals = map (init dtype) program.A.locals in (* fsm write-local state variables *)
  let types = map (init dtype) program.A.types in (* user-defined types *)

(* 

  To Kai-Zhan:

    Not 100% about what you intended, but this is what my code does:

    states: Contains a StringMap of FSMs, which in turn points to StringMaps with
    keys: states, values: numbers

    lookup_enum: Loks into StringMap states for fsm called "enum" and gets a value
    which is the StringMap of "enum"s states. Then, it looks into that StringMap for
    state "name" and returns the number of that state from the map.

    If you need to make any changes, feel free and inform me when I am back. I should
    be back by 9/9:30 at the latest. If you are in doubt about the code or anything,
    we can wait. Also, look through all the code before it. I made the slight changes
    we spoke about, but it would help to have you review it once as well.

  - Arunavha

*)

  let states =
    let iter map fsm =(* TODO: state gen code *)
      let init fsm = (**)
        let rec iter_state map count = function
        | [] -> map
        | state::tl -> StringMap.add state.A.state_name (L.const_int i32_t count) map; f map count+1 tl in
      List.fold_left iter_state StringMap.empty 0 fsm.A.fsm_body in
    StringMap.add fsm.A.fsm_name (L.define_global fsm.A.fsm_name (init fsm) sake) map in
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
        let e1' = expr builder e1 and
        let e2' = expr builder e2 in
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
