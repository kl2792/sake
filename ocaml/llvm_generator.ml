module L = Llvm
module A = Sast

module StringMap = Map.Make(String)

exception Error of string
exception Not_found

(* Note: () means "We have to write code here" *)

(* Translate an A.program to LLVM *)
let translate filename program =
  let context = L.global_context () in
  let sake = L.create_module context "sake"
    and i64_t  = L.i64_type  context
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
  let init t v = L.const_int (lltype t) v in
  let bae = L.builder_at_end context in
  let abc = L.append_block context in

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

  (* New and imported functions *)
  let tick =
    let types = [state_t; input_t; output_t] in
    let args = Array.of_list (List.map L.pointer_type types) in
    let ftype = L.function_type void_t args in
    L.define_function (filename ^ "_tick") ftype sake in
  let printf =
    let ftype = L.var_arg_function_type i32_t [|L.pointer_type i8_t|] in
    L.declare_function "printf" ftype sake in
  let memcpy =
    let args = [|L.pointer_type state_t; L.pointer_type state_t; i64_t|] in
    let ftype = L.function_type (L.pointer_type state_t) args in
    L.declare_function "memcpy" ftype sake in

  (* Variables *)
  let map vars =
    let merge map (dtype, name, value) = (* TODO: init with e *)
      let global = L.define_global name (init dtype 0) sake in
      StringMap.add name global map in
    List.fold_left merge StringMap.empty vars in
  let zmap vars = map (List.map (fun (t, n) -> t, n, 0) vars) in
  let public = map program.A.public in
  let state = zmap (List.map (fun f -> A.Int, f.A.fsm_name) program.A.fsms) in
  let input = zmap program.A.input and output = zmap program.A.output in
  let locals = ref StringMap.empty in

  (* Lookup functions *)
  let value name = (* TODO: delete *)
    let rec find_name i = function
      | [] -> raise (Error "... y tho")
      | enum :: tail -> if enum = name then i else find_name (i + 1) tail in
    find_name 0 program.A.types in
  let lookup fn io name = (* search for given name in specified maps *)
    try StringMap.find name !locals
    with Not_found ->
      try StringMap.find ((L.value_name fn) ^ "_" ^ name) public
      with Not_found ->
        try StringMap.find name public
        with Not_found -> StringMap.find name io in

  (* Expression builder *)
  let rec expr fn builder = function
    | A.IntLit i -> L.const_int i32_t i
    | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | A.CharLit c -> L.const_int i8_t (int_of_char c)
    | A.StringLit s -> L.const_stringz context s
    | A.Empty -> L.const_int i32_t 0
    | A.Variable s -> L.build_load (lookup fn input s) s builder
    | A.Printf (fmt, args) ->
        let args = (List.map (expr fn builder) args) in
        let args = (L.const_stringz context fmt) :: args in
        let args = Array.of_list args in
        L.build_call printf args "printf" builder
    | A.Uop (uop, e) -> (lluop uop) (expr fn builder e) "tmp" builder
    | A.Binop (e1, op, e2) ->
        (llop op) (expr fn builder e1) (expr fn builder e2) "tmp" builder
    | A.Assign (s, e) ->
        let e = expr fn builder e in
        ignore (L.build_store e (lookup fn output s) builder); e in

  let add_terminal builder f =
    match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (f builder) in

  (* Statement builder *)
  let rec stmt fn builder = function
    | A.Block body -> List.fold_left (stmt fn) builder body
    | A.Expr e -> ignore (expr fn builder e); builder
    | A.If (predicate, then_stmt, else_stmt) ->
        let merge_bb = abc "merge" fn in
        let then_bb = abc "then" fn in
        let else_bb = abc "else" fn in
        let cond = expr fn builder predicate in
        add_terminal (stmt fn (bae then_bb) then_stmt) (L.build_br merge_bb);
        add_terminal (stmt fn (bae else_bb) else_stmt) (L.build_br merge_bb);
        add_terminal builder (L.build_cond_br cond then_bb else_bb);
        bae merge_bb
    | A.While (predicate, body) ->
      let pred_bb = abc "while" fn in
      let body_bb = abc "while_body" fn in
      let merge_bb = abc "merge" fn in
      let value = expr fn (bae pred_bb) predicate in
      add_terminal (stmt fn (bae body_bb) body) (L.build_br pred_bb);
      add_terminal (bae pred_bb) (L.build_cond_br value body_bb merge_bb);
      add_terminal builder (L.build_br pred_bb);
      bae merge_bb
    | A.Switch (predicate, cases) ->
        let case = expr fn builder predicate in
        let merge_bb = abc "merge" fn in
        let switch_in = L.build_switch case merge_bb (List.length cases) builder in
        let rec iter i = function [] -> ()
          | (c, s) :: tail ->
              let onval = expr fn builder c in
              let case_bb = abc (Printf.sprintf "case_%d" i) fn in
              L.add_case switch_in onval case_bb;
              add_terminal (stmt fn (bae case_bb) s) (L.build_br merge_bb);
              iter (i + 1) tail in
        iter 1 cases;
        bae merge_bb 
    | A.For (name, iter, body) -> raise (Error "stop it")
    | A.State name -> raise (Error "not this one!")
    | A.Goto state -> raise (Error "don't be an idiot, goto isn't done yet") in

  (* FSM functions *)
  let fsms =
    let build_fsm fsm =
      (* Function initialization *)
      let fn =
        let types = [state_t; state_t; input_t; output_t] in
        let pointers = Array.of_list (List.map L.pointer_type types) in
        let ftype = L.function_type void_t pointers in
        L.define_function fsm.A.fsm_name ftype sake in

      (* Allocate the appropriate memory and build the function *)
      let builder = bae (L.entry_block fn) in
      let add_local m (t, n, _) = (* Local variable lazy allocation *)
        let alloca = L.build_alloca (lltype t) n builder in
        StringMap.add n alloca m in
      locals := List.fold_left add_local StringMap.empty fsm.A.fsm_locals;
      add_terminal (stmt fn builder (A.Block fsm.A.fsm_body)) L.build_ret_void;
      fn in
    List.map build_fsm program.A.fsms in

  (* Tick function definition *)
  let builder = bae (L.entry_block tick) in
  
  (* State allocation and modification *)
  let state = L.build_alloca state_t "state" builder in
  let ta = L.params tick in
  let fa = state :: (Array.to_list ta) in
  let call fsm = ignore (L.build_call fsm (Array.of_list fa) "" builder) in
  List.iter call fsms;

  (* Writing to user *)
  let write = abc "write" tick and ret = abc "ret" tick in
  let wa = [| ta.(0); state; L.size_of state_t |] and wb = bae write in
  let null = L.build_is_null wa.(0) "null" builder in
  add_terminal builder (L.build_cond_br null ret write);
  add_terminal (ignore (L.build_call memcpy wa "" wb); wb) L.build_ret_void;
  add_terminal (bae ret) L.build_ret_void;

  (* Enjoy :) *)
  sake
(* Note: evaluate assignments from right to left, not left to right *)
