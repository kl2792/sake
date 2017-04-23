module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

exception Error of string
exception Not_found

(* Note: () means "We have to write code here" *)

(* Translate an A.program to LLVM *)
let translate filename program =
  let context = L.global_context () in
  let sake = L.create_module context "sake"
    and i32_t  = L.i32_type  context
    and i8_t   = L.i8_type   context
    and i1_t   = L.i1_type   context
    and void_t = L.void_type context in

  (* Helper functions *)
  let lltype = function 
    | A.Int -> i32_t
    | A.Char -> i8_t
    | A.Bool -> i1_t
    | A.Enum _ -> i32_t
    | _ -> raise (Error "haven't figured this out yet") in
  let llop = function
    | A.Add -> L.build_add
    | A.Sub -> L.build_sub
    | A.Mul -> L.build_mul
    | A.Div -> L.build_sdiv
    | A.Eq  -> L.build_icmp L.Icmp.Eq
    | A.Neq -> L.build_icmp L.Icmp.Ne
    | A.Lt  -> L.build_icmp L.Icmp.Slt
    | A.Le  -> L.build_icmp L.Icmp.Sle
    | A.Gt  -> L.build_icmp L.Icmp.Sgt
    | A.Ge  -> L.build_icmp L.Icmp.Sge
    | A.And -> L.build_and
    | A.Or  -> L.build_or in
  let lluop = function
    | A.Neg -> L.build_neg
    | A.Not -> L.build_not in
  let llenum s = lltype (A.Enum s) in
  let lldtype (t, _) = lltype t in
  let init v t = L.const_int (lltype t) v in

  (* New types *)
  let input_t =
    let types = Array.of_list (List.map lldtype program.A.input) in
    L.struct_type context types in
  let output_t =
    let types = Array.of_list (List.map lldtype program.A.output) in
    L.struct_type context types in
  let state_t =
    let fsms = List.map (fun f -> f.A.fsm_name) program.A.fsms in
    let states = List.map llenum fsms in
    let public = List.map (fun (t, _, _) -> lltype t) program.A.public in
    let types = Array.of_list (states @ public) in
    L.struct_type context types in

  (* Variables *)
  let global_vars =
    let map vars =
      let merge index map (dtype, name) =
        let global = L.define_global name (init 0 dtype) sake in
        StringMap.add name (index, global) map in
      List.fold_left (merge 0) StringMap.empty vars in
    let inputs = map program.A.input in (* FSM inputs *)
    let outputs = map program.A.output in (* FSM outputs *)
    let public = List.map (fun (t, n, _) -> t, n) program.A.public in
    let public = map public in (* FSM public write-local state variables *)
    let states = List.map (fun fsm -> (A.Enum ""), fsm.A.fsm_name) program.A.fsms in
    let states = map states in (* FSM state indicators *)
    let names = ["input"; "output"; "public"; "state"]
    and submaps = [inputs; outputs; public; states] in
    let merge map name submap = StringMap.add name submap map in
    List.fold_left2 merge StringMap.empty names submaps in

  (* Lookup functions *)
  let locals = ref [] in
  let value name =
    let rec find_name i = function
      | [] -> raise (Error "... y tho")
      | enum :: tail -> if enum == name then i else find_name (i + 1) tail in
    find_name 0 program.A.types in
  let lookup name locals maps = (* search for given name in specified maps *)
    let rec search = function
      | [] -> raise (Error "what the hell?")
      | map :: maps ->
          let map = StringMap.find map global_vars in
          try
            StringMap.find name map
          with Not_found -> search maps in
    search maps in

  (* External functions *)
  let print =
    let ftype = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    L.declare_function "printf" ftype sake in
  let memcpy =
    let formals = [| L.pointer_type state_t; L.pointer_type state_t; i32_t |] in
    let ftype = L.function_type (L.pointer_type i8_t) formals in
    L.declare_function "memcpy" ftype sake in

  (* Expression builder *)
  let rec expr builder = function
    | A.IntLit i -> L.const_int i32_t i
    | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | A.CharLit c -> L.const_int i8_t (int_of_char c)
    | A.StringLit s -> L.const_stringz context s
    | A.Empty -> L.const_int i32_t 0
    | A.Variable s -> raise (Error "NIMP: Variable")(*L.build_load (lookup s) s builder*)
    | A.Print (fmt, args) ->
        let fmt' = L.build_global_stringptr fmt "fmt" builder in
        let args = [fmt'; expr builder (List.hd args)] in
        (* let args = fmt :: (List.map (expr builder) args) in *)
        let args = Array.of_list args in
        L.build_call print args "printf" builder
    | A.Uop (uop, e) ->
        let build = lluop uop in
        let e = expr builder e in
        build e "tmp" builder
    | A.Binop (e1, op, e2) ->
        let build = llop op in
        let e1 = expr builder e1 and e2 = expr builder e2 in
        build e1 e2 "tmp" builder
    | A.Assign (s, e) -> raise (Error "NIMP: Assign")
        (* let e = expr builder e in (* TODO: fix assign *)
       let _ = L.build_store e (lookup s) builder in
        e *)
    | A.Escape s -> raise (Error "NIMP: Escape")
    | A.Range (s, e, i) -> raise (Error "NIMP: Range")
(*    | A.ArrayLit exps -> raise (Error "NIMP: ArrayLit") *)
    | A.Cond (cond, e1, e2) -> raise (Error "NIMP: Cond") in

  let add_terminal builder f =
    match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (f builder) in

  (* Statement builder *)
  let rec stmt fn builder = function
    | A.Block body -> List.fold_left (stmt fn) builder body
    | A.Expr e -> let _ = expr builder e in builder
    | A.If (predicate, then_stmt, else_stmt) ->
        let merge_bb = L.append_block context "merge" fn in
        let then_bb = L.append_block context "then" fn in
        let _ =
          add_terminal (stmt fn (L.builder_at_end context then_bb) then_stmt)
          (L.build_br merge_bb) in
        let else_bb = L.append_block context "else" fn in
        add_terminal (stmt fn (L.builder_at_end context else_bb) else_stmt)
        (L.build_br merge_bb);
      let bool_val = expr builder predicate in
      ignore (L.build_cond_br bool_val then_bb else_bb builder);
      L.builder_at_end context merge_bb
    | A.While (predicate, body) ->
        let pred_bb = L.append_block context "while" fn in
        ignore (L.build_br pred_bb builder);
      let body_bb = L.append_block context "while_body" fn in
      add_terminal (stmt fn (L.builder_at_end context body_bb) body)
      (L.build_br pred_bb);
      let pred_builder = L.builder_at_end context pred_bb in
      let bool_val = expr pred_builder predicate in
      let merge_bb = L.append_block context "merge" fn in
      ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
      L.builder_at_end context merge_bb
    | A.Switch (predicate, cases) ->
        let case = expr builder predicate in
        let merge_bb = L.append_block context "merge" fn in
        let switch_in = L.build_switch case merge_bb (List.length cases) builder in
        let rec iter i = function
          | [] -> ()
          | (c, s) :: tail ->
              let onval = expr builder c in
              let case_bb = L.append_block context (Printf.sprintf "case_%d" i) fn in
              add_terminal (stmt fn (L.builder_at_end context case_bb) s)
              (L.build_br merge_bb);
              L.add_case switch_in onval case_bb;
            iter (i + 1) tail in
        iter 0 cases; 
        L.builder_at_end context merge_bb 
    | A.For (name, iter, body) ->
        (* TODO: implement local variables for for loop *)
        raise (Error "stop it")
    | A.Ldecl (dtype, decls) -> raise (Error "stop it, i said")
    | A.State name -> raise (Error "not this one!")
    | A.Goto state -> raise (Error "don't be an idiot, goto isn't done yet") in

  (* FSM functions *)
  let fsms =
    let rec build_fsms = function
      | [] -> []
      | fsm :: fsms ->
          let fn =
            let types = [state_t; state_t; input_t; output_t] in
            let pointers = Array.of_list (List.map L.pointer_type types) in
            let ftype = L.function_type i32_t pointers in
            L.define_function fsm.A.fsm_name ftype sake in
          let builder = L.builder_at_end context (L.entry_block fn) in
          let _ = stmt fn builder (A.Block fsm.A.fsm_body) in
          let _ = add_terminal builder (L.build_ret (L.const_int i32_t 0)) in
          locals := [StringMap.empty];
          (fsm.A.fsm_name, fn) :: (build_fsms fsms) in
    build_fsms program.A.fsms in

  (* Tick function definition *)
  let tick =
    let types = [state_t; input_t; output_t] in
    let args = Array.of_list (List.map L.pointer_type types) in
    let ftype = L.function_type void_t args in
    L.define_function (filename ^ "_tick") ftype sake in
  let builder = L.builder_at_end context (L.entry_block tick) in
  let state = L.build_alloca state_t "state" builder in
  let calls = 
    let rec call = function
      | [] -> ()
      | (name, fsm) :: tail ->
          let args = L.params tick in
          let args = Array.of_list (state :: (Array.to_list args)) in
          L.build_call fsm args name builder;
          call tail in
    call fsms in
  (*let args = List.map (fun s -> Printf.printf "%s " (L.string_of_llvalue s)) (Array.to_list (L.params fsm)); Printf.printf "\n" in*)
  (*let args = List.map L.const_pointer_null [state_t; state_t; input_t; output_t] in*)
  (*let _ = L.build_call fsm (Array.of_list args) name builder in*)
  (*let calls =
    let build (name, fn) = L.build_call fn () name builder  in
    L.iter build fsms in (* TODO: use inputs to tick, alloc'ed memory *) *)
  let writing =
    let args = [|(L.params tick).(0); state; L.size_of state_t|] in
    (*L.build_call memcpy args "memcpy" builder*)() in
  let terminal = add_terminal builder L.build_ret_void in
  sake

  (* L.function_type to create function (tick) *)
  (* L.insertion_block to indicate where to insert function blocks *)
  (* L.build_at_end to add LLVM statements to L.entry_block *)
  (* L.build_alloca to add LLVM memory allocation *)
  (* L.build_load and L.build_store for the expression builder *)
  (* L.build_(add|sub|mul|sdiv|or|...) for arithmetic/comparison *)
  (* L.build_ret_void for returning void *)
(* Note: evaluate assignments from right to left, not left to right *)
