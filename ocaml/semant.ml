module A = Ast
module S = Sast
open Printf

module StringMap = Map.Make(String)

exception SemanticError of string



type t =
  | Bool_t | Int_t | Char_t | String_t
  | Enum_t of string (* just the name of the enum *)
  | Exception of string


let rec find_variable scope name =
try
  List.find (fun (s, _) -> s = name) scope.S.variables
with Not_found ->
  match scope.parent with
    Some(parent) -> find_variable parent name
  | _ -> raise Not_found


let require_integer e msg =
  if (e = Int_t) then ()
  else raise (SemanticError msg)


let report_duplicate exceptf list =
  let rec helper = function
  n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
  | _ :: t -> helper t
  | [] -> ()
  in helper (List.sort compare list)




let undeclared_identifier_error name =
    let msg = sprintf "undeclared identifier %s" name in
    raise (SemanticError msg)

let illegal_assignment_error =
    let msg = sprintf "illegal assignment" in
    raise (SemanticError msg)


let illegal_unary_operation_error =
    let msg = sprintf "illegal unary operator" in
    raise (SemanticError msg)


let illegal_binary_operation_error =
    let msg = sprintf "illegal binary operator" in
    raise (SemanticError msg)


let check_assign lvaluet rvaluet = match lvaluet with
          S.Bool when rvaluet = S.Int -> lvaluet
       (* | S.Bool when rvaluet = Int_t -> lvaluet *)
        | _ -> if lvaluet == rvaluet then lvaluet else 
            illegal_assignment_error




(**** Checking Global Variables ****)

let check_globals inp outp env = 
  let globals = inp @ outp in
report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);
 List.fold_left (fun lst (typ,name) -> (name,typ)::lst) env.S.scope.variables globals

(**** Checking Functions ****)

(*  if List.mem "print" (List.map (fun fd -> fd.fsm_name) fsms)
then raise (Failure ("function print may not be defined")) else (); *)

(* Function declaration for a named function *)
(*  let built_in_decls =  StringMap.add "print"
 { typ = Void; fname = "print"; formals = [(Int, "x")];
   locals = []; body = [] } (StringMap.add "printb"
 { typ = Void; fname = "printb"; formals = [(Bool, "x")];
   locals = []; body = [] } (StringMap.singleton "printbig"
 { typ = Void; fname = "printbig"; formals = [(Int, "x")];
   locals = []; body = [] }))
in *)


let check_fsm_decl fsms =
report_duplicate (fun n -> "duplicate fsm " ^ n)
(List.map (fun fd -> fd.fsm_name) fsms)


let check_fsm_locals fsm =
(**** Check FSM INSTANCE VARS: public and states ****)
report_duplicate (fun n -> "duplicate state " ^ n ^ " in " ^ fsm.fsm_name)
  (List.map fst fsm.fsm_states);

report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ fsm.fsm_name)
  (List.map snd fsm.fsm_locals)


let check_pubs pubs env =
report_duplicate (fun n -> "duplicate public " ^ n )
  (List.map snd pubs);
 List.fold_left (fun lst (typ,name,_) -> (name,typ)::lst) env.scope.S.variables pubs




let rec type_of_identifier scope name =
    let vdecl = try
      find variable scope name
    with Not_found -> undeclared_identifier_error name
  in
    let (_,typ) = vdecl in typ




let rec get_expr env = function (* A.expr *)
| S.BoolLit(bl) -> Bool_t
| S.CharLit(ch) -> Char_t
| S.IntLit(num) -> Int_t
| S.StringLit(name) -> String_t
| S.Variable(name) -> 
(*
    let (_,vl) =
      let var = try
          find_variable env.scope name
      with Not_found ->
          raise (SemanticError("undeclared identifier " ^ name))
      in
    var; vl
*)
    let var = try
        find_variable env.scope name
    with Not_found ->
        raise (SemanticError("undeclared identifier " ^ name))
    in 
      let (_,vl) = var in vl

(*        else ignore(env.scope.S.variables <- decl::env.scope.S.variables); S.Variable(name) *)

| S.Uop(op, e) ->
    let t = get_expr env e in
      (match op with
      S.Neg when t = Int_t -> Int_t
      | S.Not when t = Bool_t -> Bool_t
      | _ -> illegal_unary_operation_error
      )

| S.Binop(e1,op,e2) -> 
let t1 = expr env e1  and t2 = expr env e2 in
( match op with 
  | S.Add | S.Sub | S.Mul | S.Div when t1 = Int_t && t2 = Int_t -> Int_t
  | S.Eq | S.Neq | S.Lt | S.Le | S.Gt | S.Ge when t1 = t2 -> Bool_t
  | S.And | S.Or when t1 = Bool_t && t2 = Bool_t -> Bool_t
  | _  -> illegal_binary_operation_error
)

| S.Assign(name,exp) ->
    let lt = type_of_identifier env.scope name and rt = get_expr env exp in
    check_assign lt rt
  
| S.Printf(fmt, lst) -> ignore(List.map (get_expr env) lst);
| S.Empty -> S.Empty

(* OURS *)
let rec check_stmt env fsm = function (* stmts *)
| S.Block(s_list) ->

let sl =
  let env' =
    let scope' =
      { parent = Some(env.scope); variables = [] }
    in
    { env with scope = scope' }
  in
  List.map (fun s -> check_stmt env' fsm s) s_list
in sl  


(*
(* New scopes: parent is the existing scope, start out empty *)
let scope' = { parent = Some(env.scope); variables = [] } 
 in
         (* New environment: same, but with new symbol tables *)
let env' = { env with scope = scope' } in
         (* Check all the statements in the block *)
let s_list = List.map (fun s -> check_stmt env' fsm s) s_list
*)

(* | S.State(name) -> S.State(name) *)
| S.If(pred,sta,stb) -> 
    let e = get_expr env pred in
    ignore((match e with
      Int_t | Bool_t -> ()
      | _ -> raise (SemanticError("Illegal predicate type"))));
    ignore(check_stmt env fsm sta); ignore(check_stmt env fsm stb) (**)

| S.For(str,(na,nb,nc),stm) ->

    try
      List.find (fun (s, _) -> s = str) env.scope.S.variables
    with Not_found ->
    env.scope.S.variables <- (str,Int) :: env.scope.S.variables;
    
    ignore(require_integer na); ignore(require_integer nb); ignore(require_integer nc); (check_stmt env fsm stm)

| S.While(pred,stm) -> 
    let e = get_expr env pred in
    ignore((match e with
      Int_t | Bool_t -> ()
      | _ -> raise (SemanticError("Illegal predicate type"))));
    ignore(check_stmt env fsm stm) (**)

| S.Switch(exp, cases) -> 
    ignore(get_expr env exp); ignore(check_cases cases);

| S.Expr(e) -> ignore (get_expr env e) (**)

| S.Goto(label) ->
  try List.find label fsm.fsm_states
with Not_found -> raise (SemanticError "No such state exists")


and check_cases = function (* (expr * stmt) list *)
[] -> ()
| (e,s_list)::tl -> ignore(get_expr env e); ignore(
  let sl =
    let env' =
      let scope' =
        { parent = Some(env.scope); variables = [] }
      in
      { env with scope = scope' }
    in
    List.map (fun s -> check_stmt env' fsm s) s_list
  in sl  
); ignore(check_cases tl)


let check_body env fsm =
  check_stmt env fsm S.Block(fsm.fsm_body)

let check_semant env fsm =
  check_fsm_locals fsm;
  let states_list = List.map (fun (name,ind) -> name) fsm.fsm_states
in
  ignore(check_body env fsm);


(*
let check program =
  let sym_tab = {parents = None; variables = [] }
in
  let env = {scope=sym_tab} in
  let new_syms = {sym_tab with variables = check_globals program.S.input program.S.output env}
in
  let env = { env with scope=new_syms} in


  let new_syms = {sym_tab with variables = check_pubs program.S.public env}
in
  let env = { env with scope=new_syms} in
  check_fsm_decl program.S.fsms;

  List.iter (check_semant env) S.fsms

*)





let check program =
  let checked =
      let env2 =
          let new_syms1 = 
              let env1 = 
                  let new_syms = 
                      let env = 
                          let sym_tab = 
                          {parents = None; variables = [] } in
                      {scope=sym_tab} in
                  {sym_tab with variables = check_globals program.S.input program.S.output env} in
              { env with scope=new_syms} in
          {new_syms with variables = check_pubs program.S.public env1} in
      { env1 with scope=new_syms1} in
  check_fsm_decl program.S.fsms in
List.iter (check_semant env) program.S.fsms in ()

