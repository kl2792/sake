(*
 * The function translate converts a Sast.program to an Llvm.llmodule.
 *
 * Author: Kai-Zhan Lee
 * Credzz: Shalva Kohen for A.Switch structure and minor bug fixes.
 *)

module L = Llvm
module A = Sast

module StringMap = Map.Make(String)

exception ENOSYS of string
exception Bug of string

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
    | A.String -> L.pointer_type i8_t in
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
  let lldtype (t, _) = lltype t in
  let bae = L.builder_at_end context in
  let abc = L.append_block context in

  let zero = L.const_int i32_t 0
  and pos1 = L.const_int i32_t 1 in
  
  (* New types *)
  let input_t =
    let types = Array.of_list (List.map lldtype program.A.input) in
    L.struct_type context types in
  let output_t =
    let types = Array.of_list (List.map lldtype program.A.output) in
    L.struct_type context types in
  let state_t =
    let public =
      let fsms = List.map (fun _ -> lltype A.Int) program.A.fsms in
      let public = List.map (fun (t, _, _) -> lltype t) program.A.public in
      i32_t :: fsms @ public in (* _running variable *)
    let types = Array.of_list public in
    L.struct_type context types in

  (* External functions *)
  let printf =
    let ftype = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    L.declare_function "printf" ftype sake in
  let memcpy =
    let state_t_ptr = L.pointer_type state_t in
    let args = [| state_t_ptr; state_t_ptr; i64_t |] in
    let ftype = L.function_type void_t args in
    L.declare_function "memcpy" ftype sake in

  (* Debugging *)
  let debug = false in
  let init = L.const_int i32_t in
  let gsp s b = if debug then L.build_global_stringptr s s b else zero in
  let debug s l builder =
    if debug then
      let args = gsp ("\027[31m"^s^"\027[0m") builder :: l in
      List.iter L.dump_value args;
      ignore (L.build_call printf (Array.of_list args) "" builder)
    else () in

  (* Variable maps *)
  let imap =
    let rec imap i a = function [] -> a
      | (_, n) :: tail -> imap (i + 1) (StringMap.add n i a) tail in
    imap 0 StringMap.empty in
  let public = (* int StringMap : names -> struct indices *)
    let fsms = List.map (fun f -> (A.Int, f.A.fsm_name)) program.A.fsms in
    let public = List.map (fun (t, n, _) -> (t, n)) program.A.public in
    imap ((A.Int, "*running") :: fsms @ public)
  and input  = imap program.A.input
  and output = imap program.A.output in

  (* FSM-specific metadata *)
  let locals = ref StringMap.empty and states = ref StringMap.empty in

  (* Lookup function; search locals, local public, public, input/output *)
  let lookup fn io name builder =
    try StringMap.find name !locals with
      Not_found ->
        let fa = L.params fn
        and io_if v1 v2 = if io == input then v1 else v2
        and local = (L.value_name fn) ^ "_" ^ name (* local name in FSM *) in
        if StringMap.mem local public || StringMap.mem name public then
          let pub_val, pub_ptr = try StringMap.find local public, fa.(0) with
            Not_found -> StringMap.find name public, io_if fa.(1) fa.(0) in
          debug "lookup %s: public[%d]\n"
          [gsp name builder; init pub_val] builder;
          L.build_struct_gep pub_ptr pub_val name builder
        else
          let io_ptr = io_if fa.(2) fa.(3)
          and io_val = try StringMap.find name io with
            Not_found -> raise (Bug (Printf.sprintf "No variable: %s" name)) in
          debug "lookup %s: %s[%d]\n" [gsp name builder;
          gsp (if io == input then "input" else "output") builder;
          init io_val] builder;
          L.build_struct_gep io_ptr io_val name builder in              

  (* Expression builder *)
  let rec expr fn builder = function
    | A.IntLit i -> L.const_int i32_t i
    | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | A.CharLit c -> L.const_int i8_t (int_of_char c)
    | A.StringLit s -> L.build_global_stringptr s "string" builder
    | A.Empty -> L.const_int i32_t 0
    | A.Variable s ->
        let value = L.build_load (lookup fn input s builder) s builder in
        debug "access %s: %d\n" [gsp s builder; value] builder;
        value
    | A.Printf (fmt, args) ->
        let args = (List.map (expr fn builder) args) in
        let args = (L.build_global_stringptr fmt "fmt" builder) :: args in
        let args = Array.of_list args in
        L.build_call printf args "printf" builder
    | A.Uop (uop, e) -> (lluop uop) (expr fn builder e) "tmp" builder
    | A.Binop (e1, op, e2) ->
        (llop op) (expr fn builder e1) (expr fn builder e2) "tmp" builder
    | A.Assign (s, e) ->
        let e = expr fn builder e in
        ignore (L.build_store e (lookup fn output s builder) builder);
        debug "assign %s: %d\n" [gsp s builder; e] builder; e in

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
        add_terminal builder (L.build_cond_br cond then_bb else_bb);
        add_terminal (stmt fn (bae then_bb) then_stmt) (L.build_br merge_bb);
        add_terminal (stmt fn (bae else_bb) else_stmt) (L.build_br merge_bb);
        bae merge_bb
    | A.While (predicate, body) ->
        let pred_bb = abc "while" fn in
        let body_bb = abc "while_body" fn in
        let merge_bb = abc "merge" fn in
        let value = expr fn (bae pred_bb) predicate in
        add_terminal builder (L.build_br pred_bb);
        add_terminal (bae pred_bb) (L.build_cond_br value body_bb merge_bb);
        add_terminal (stmt fn (bae body_bb) body) (L.build_br pred_bb);
        bae merge_bb 
    | A.Switch (value, cases) ->
        let merge = abc "merge" fn in
        let switch = 
          let value = expr fn builder value in
          L.build_switch value merge (List.length cases) builder in
        let build_case (onval, body) =
          let case = abc "case" fn in
          let body = A.Block body in
          L.add_case switch (expr fn builder onval) case;
          add_terminal (stmt fn (bae case) body) (L.build_br merge) in
        List.iter build_case cases;
        bae merge
    | A.For (name, (start, stop, step), body) -> 
        let cond = A.Binop ((A.Variable name), A.Neq, (A.IntLit stop)) in
        let init = A.Expr (A.Assign (name, A.IntLit start)) in
        let increment =
          let value = A.Binop ((A.Variable name), A.Add, (A.IntLit step)) in
          A.Expr (A.Assign (name, value)) in
        let body = A.Block [body; increment] in
        let body = A.Block [init; A.While (cond, body)] in
        stmt fn builder body
    | A.State state ->
        let block, _ = try StringMap.find state !states with
          Not_found -> raise (Bug (Printf.sprintf "No state: %s" state)) in
        let term builder = stmt fn builder (A.Goto state) in
        add_terminal builder term;
        bae block
    | A.Goto state ->
        let _, value = try StringMap.find state !states with
          Not_found -> raise (Bug (Printf.sprintf "No state: %s" state)) in
        debug "goto %s: %d\n" [gsp state builder; L.const_int i32_t value] builder;
        ignore (expr fn builder (A.Assign (L.value_name fn, A.IntLit value)));
        add_terminal builder L.build_ret_void;
        builder
    | A.Halt ->
        ignore (expr fn builder (A.Assign ("*running", A.IntLit 0)));
        debug "halted %s\n" [gsp (L.value_name fn) builder] builder;
        add_terminal builder L.build_ret_void;
        builder in

  (* FSM functions *)
  let fsms =
    let build_fsm fsm =
      (* Function initialization *)
      let fn =
        let types = [state_t; state_t; input_t; output_t] in
        let pointers = Array.of_list (List.map L.pointer_type types) in
        let ftype = L.function_type void_t pointers in
        L.define_function fsm.A.fsm_name ftype sake in
      let init = abc "*init" fn and halt = abc "*halt" fn in

      (* Generate mapping of running & state name -> block / index *)
      let add_state m (n, i) = StringMap.add n (abc n fn, i) m in
      states := List.fold_left add_state StringMap.empty fsm.A.fsm_states;

      (* Halt if invalid state; use unique names for blocks *)
      let builder = bae halt in
      let ptr = L.build_struct_gep (L.params fn).(0) 0 "ptr" builder in
      debug "halting from %s\n" [gsp (L.value_name fn) builder] builder;
      ignore (L.build_store zero ptr builder);
      add_terminal builder (L.build_ret_void);

      (* Allocate locals and jump to the correct state *) 
      let builder = bae (L.entry_block fn) in
      let add_local m (t, n, e) = (* Local variable allocation *)
        let local = L.build_alloca (lltype t) n builder
        and e = expr fn builder e in
        debug "local %s: %d" [gsp n builder; e] builder;
        ignore (L.build_store e local builder);
        StringMap.add n local m in
      locals := List.fold_left add_local StringMap.empty fsm.A.fsm_locals;
      let bindings = StringMap.bindings !states in
      let switch =
        let state = lookup fn input (L.value_name fn) builder in
        let value = L.build_load state (L.value_name fn) builder in
        debug "state %s: %d\n" [gsp (L.value_name fn) builder; value] builder;
        L.build_switch value halt (List.length bindings) builder in
      let build_case (_, (block, value)) =
        let value = L.const_int i32_t value in
        L.add_case switch value block in
      List.iter build_case (("", (init, 0)) :: bindings);

      (* Build the function body; start with dead, loop in last state *)
      let builder = bae init in
      let body = A.Block fsm.A.fsm_body in
      let builder =
        try 
          let head = fst (List.hd fsm.A.fsm_states) in
          let first = fst (StringMap.find head !states) in
          L.builder_before context (L.build_br first builder) with
        Failure _ -> builder in
      add_terminal (stmt fn builder body) L.build_ret_void;
      fn in
    List.map build_fsm program.A.fsms in

  (* Tick function definition *)
  let tick =
    let types = [state_t; input_t; output_t] in
    let args = Array.of_list (List.map L.pointer_type types) in
    let ftype = L.function_type (L.pointer_type state_t) args in
    L.define_function (filename ^ "_tick") ftype sake in

  (* Metadata, essentially *)
  let ta = L.params tick and null = L.const_null (L.pointer_type state_t) in
  let reset  = abc "reset"  tick and check  = abc "check"  tick
  and update = abc "update" tick and halted = abc "halted" tick in

  (* Reset if input is NULL; otherwise, proceed as normal *)
  let builder = bae (L.entry_block tick) in
  let is_null = L.build_is_null ta.(1) "null" builder in
  debug "\ntick started\n" [] builder;
  add_terminal builder (L.build_cond_br is_null reset check);

  (* Reset *)
  let builder = bae reset in
  let store i v =
    let ptr = L.build_struct_gep ta.(0) i "reset" builder in
    debug "reset %d, %p: %d\n" [L.const_int i32_t i; ptr; v] builder;
    ignore (L.build_store v ptr builder) in
  let l = List.length program.A.fsms + 1 in
  let pub_iter i (_, _, e) = store (l + i) (expr tick builder e) in
  store 0 pos1; (* the _running variable *)
  List.iteri (fun i _ -> store (i + 1) zero) program.A.fsms; (* FSM states *)
  List.iteri pub_iter program.A.public; (* public variables *)
  add_terminal builder (L.build_ret null);

  (* Check if halted *)
  let builder = bae check in
  let running =
    let ptr = L.build_struct_gep ta.(0) 0 "ptr" builder in
    let run = L.build_load ptr "running" builder in
    L.build_icmp L.Icmp.Ne run zero "run" builder in
  debug "check _running: %d\n" [running] builder;
  add_terminal builder (L.build_cond_br running update halted);

  (* Allocate, initialize, modify, and update FSM state *)
  let builder = bae update in
  let state = L.build_alloca state_t "state" builder in
  let fa = Array.of_list (state :: (Array.to_list ta)) in (* FSM args *)
  let m1 = [| state; ta.(0); L.size_of state_t |]
  and m2 = [| ta.(0); state; L.size_of state_t |] in
  ignore (L.build_call memcpy m1 "" builder);
  List.iter (fun fsm -> ignore (L.build_call fsm fa "" builder)) fsms;
  ignore (L.build_call memcpy m2 "" builder);
  add_terminal builder (L.build_ret ta.(0));

  (* Halted: return 0 iff halted before tick was called *)
  add_terminal (bae halted) (L.build_ret null);

  (* Enjoy :) *)
  sake
