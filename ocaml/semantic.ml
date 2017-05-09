module A = Ast
module S = Sast
open Printf

module StringMap = Map.Make(String)

exception SemanticError of string



let wrong_enum_error name =
    let msg = sprintf "undeclared enum value %s" name in
    raise (SemanticError msg)


let convert_type = function (* A.dtype *)
| A.Bool -> S.Bool
| A.Int -> S.Int
| A.Char -> S.Char
| A.String -> S.String
| A.Enum(name) -> S.Enum(name)


let get_uop = function (* A.uop *)
| A.Neg -> S.Neg
| A.Not -> S.Not

let get_op = function (* A.op *)
| A.Add -> S.Add
| A.Sub -> S.Sub
| A.Mul -> S.Mul
| A.Div -> S.Div
| A.Eq -> S.Eq
| A.Neq -> S.Neq
| A.Lt -> S.Lt
| A.Le -> S.Le
| A.Gt -> S.Gt
| A.Ge -> S.Ge
| A.And -> S.And
| A.Or -> S.Or



let rec find_val vl ind = function (* start at 1 *)
  | [] -> (-1)
  | [x] -> if(x=vl) then ind else find_val vl (ind+1) []
  | x::tl -> if(x=vl) then ind else find_val vl (ind+1) tl
  | _ -> (-1)


let look_for vl type_dec=
    find_val vl 1 type_dec.A.type_values

let rec is_there_res = function
  | [] -> (-1)
  | [x] -> if(x = (-1)) then is_there_res [] else x
  | x::tl -> if(x = (-1)) then is_there_res tl else x
  | _ -> (-1)

let rec look_in_states vl = function
  | [] -> (-1)
  | [(name,num)] -> if (name=vl) then num else look_in_states []
  | (name,num)::tl ->  if (name=vl) then num else look_in_states tl
  | _ -> (-1)

let rec get_expr sts program = function (* A.expr *)
| A.BoolLit(bl) -> S.BoolLit(bl)
| A.CharLit(ch) -> S.CharLit(ch)
| A.IntLit(num) -> S.IntLit(num)
| A.StringLit(name) -> S.StringLit(name)
| A.Variable(name) -> S.Variable(name)
| A.EnumLit(vl) -> 
  let result =
    let enum_search =
      List.map (look_for vl) program.A.types
    in
    is_there_res enum_search
  in if (result <> (-1)) then S.IntLit(result) else
  let is_state = look_in_states vl sts in
  if (is_state <> (-1)) then is_state else (wrong_enum_error vl)
| A.Access (outer,inner) -> S.Variable(outer ^ "_" ^ inner)
| A.Uop(u,exp) -> S.Uop((get_uop u),(get_expr sts program exp))
| A.Binop(e1,o,e2) -> S.Binop((get_expr sts program e1), (get_op o) ,(get_expr sts program e2))
| A.Assign(name,exp) -> S.Assign(name,(get_expr sts program exp))
| A.Printf(fmt, lst) -> S.Printf(fmt, (get_e_list sts program lst))
| A.Empty -> S.Empty
and get_e_list sts program = function (* expr list *)
[] -> []
| exp::tl -> (get_expr sts program exp)::(get_e_list sts program tl)


let rec do_stmt sts program = function (* stmts *)
| A.Block(s_list) -> S.Block(take_stmts sts program s_list)
| A.State(name) -> S.State(name)
| A.If(pred,sta,stb) -> S.If((get_expr sts program pred),(do_stmt sts program sta),(do_stmt sts program stb))
| A.For(str,na,nb,nc,stm) -> S.For(str,(na,nb,nc),(do_stmt sts program stm))
| A.While(pred,stm) -> S.While((get_expr sts program pred),(do_stmt sts program stm))
| A.Switch(exp, cases) -> S.Switch((get_expr sts program exp),(get_cases sts program cases))
| A.Expr(e) -> S.Expr(get_expr sts program e)
| A.Goto(label) -> S.Goto(label)
| A.Halt -> S.Halt
and take_stmts sts program = function (*stmt list*)
[] -> []
| stm::tl -> (do_stmt sts program stm)::(take_stmts sts program tl)
and get_cases sts program = function (* (expr * stmt) list *)
[] -> []
| (e,s_list)::tl -> ((get_expr sts program e),(take_stmts sts program s_list))::(get_cases sts program tl)


let rec take_in = function
[] -> []
| (typ,name)::tl -> ((convert_type typ),name)::(take_in tl)

let rec take_out = function
[] -> []
| (typ,name)::tl -> ((convert_type typ),name)::(take_out tl)

(* For when header can deal with Enums (UDTs) *)
let rec take_typ = function
[] -> []
| {A.type_name = name; A.type_values=vals}::tl -> {S.type_name = name; S.type_values = vals}::(take_typ tl)
(**)

(*

let rec make_enum_list num = function
| [] -> []
| name::tl -> (name,num)::(make_enum_list (num+1) tl)

let rec take_typ = function
| [] -> []
| {A.type_name = name; A.type_values=vals}::tl -> (name,(make_enum_list 0 vals))::(take_typ tl)

*)

let rec copy_locals sts program = function (*(dtype * string * expr) list*)
[] -> []
| (typ,var_name,expr)::tl -> ((convert_type typ),var_name,(get_expr sts program expr)):: (copy_locals sts program tl)


let rec get_states num = function (* body: stmt list *)
[] -> []
| A.State(name)::tl -> (name,num):: (get_states (num+1) tl)
| _::tl -> get_states num tl

let rec take_fsm program = function
[] -> []
| {A.fsm_name = name; A.fsm_public = pubs; A.fsm_locals = local; A.fsm_body =  body}::tl
    -> let sts = (get_states 1 body) in { S.fsm_name = name; S.fsm_locals = (copy_locals sts program local); S.fsm_states = sts; S.fsm_body = (take_stmts sts program body)}::(take_fsm program tl)


let rec take_pubs sts program name = function (*(dtype * string * expr) list*)
[] -> []
| (typ,var_name,expr)::tl -> ((convert_type typ),name ^ "_" ^ var_name,(get_expr sts program expr)):: (take_pubs sts program name tl)

let rec get_pubs sts program = function
[] -> []
| {A.fsm_name = name; A.fsm_public = pubs; A.fsm_locals = local; A.fsm_body =  body}::tl
    -> (take_pubs sts program name pubs) @ (get_pubs sts program tl)




(*
let convert = function
{A.input = i; A.output=o; A.types = typs; A.fsms = fsms} -> {S.input = take_in i; S.output = take_out o; S.public = S.public(get_pubs fsms); S.types = take_typ typs; S.fsms = take_fsm fsms}
| _ -> {S.input = (); S.output = (); S.public = (); S.types = (); S.fsms = ()}
*)
let convert i o typs fsms program = function
[] -> {S.input = take_in i; S.output = take_out o; S.public = get_pubs sts program fsms; S.types = take_typ typs; S.fsms = take_fsm program fsms}
(*| _ -> {S.input = (); S.output = (); S.public = (); S.types = (); S.fsms = ()}*)











let check program =
  convert program.A.input program.A.output program.A.types program.A.fsms program []
(* or convert program *)


(************************)

(*
let check program =
  let public = 
    let rec get_names a n = function [] -> a
      | (_, pn, _) :: t -> get_names ((n ^ "_" ^ pn) :: a) n t in
    let rec sast_pub_list a = function [] -> a
      | h :: t -> sast_pub_list (get_names a h.A.fsm_name h.A.fsm_public) t in
    sast_pub_list []  program.A.fsms in
  let slvalue (t, n) = t, n in
  let stype t = {S.type_name=t.A.type_name; S.type_values=t.A.type_values} in
  (* let svar (d, s, e) = d, s, e in *)
  let sfsm f = {
    S.fsm_name=f.A.fsm_name;
    S.fsm_states=["start"];
    (* S.fsm_locals=svar f.A.fsm_locals; *)
    S.fsm_locals=f.A.fsm_locals;
    S.fsm_body=f.A.fsm_body;
  } in
  {
    S.input = List.map slvalue program.A.input;
    S.output = List.map slvalue program.A.output;
    S.public = public;
    S.types = List.map stype program.A.types;
    S.fsms = List.map sfsm program.A.fsms;
  }

*)

(*
let autre program = 

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
  n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in
   
  (**** Checking Global Variables ****)

  let globals = program.A.input @ program.A.output in

  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)

(*  if List.mem "print" (List.map (fun fd -> fd.fsm_name) fsms)
  then raise (Failure ("function print may not be defined")) else (); *)

  report_duplicate (fun n -> "duplicate fsm " ^ n)
    (List.map (fun fd -> fd.fsm_name) fsms);

  (* Function declaration for a named function *)
(*  let built_in_decls =  StringMap.add "print"
     { typ = Void; fname = "print"; formals = [(Int, "x")];
       locals = []; body = [] } (StringMap.add "printb"
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] } (StringMap.singleton "printbig"
     { typ = Void; fname = "printbig"; formals = [(Int, "x")];
       locals = []; body = [] }))
   in *)

  let fsm_decls = List.fold_left (fun m fd -> StringMap.add fd.fsm_name fd m)
                         StringMap.empty fsms
  in

  let fsm_decl s = try StringMap.find s fsm_decls
       with Not_found -> raise (Failure ("unrecognized fsm " ^ s))

  let check_fsm fsm =
(**** Check FSM INSTANCE VARS: public and states ****)

    List.iter (check_not_void (fun n -> "illegal void public " ^ n ^
      " in " ^ fsm.fsm_name)) fsm.fsm_public;
    
    report_duplicate (fun n -> "duplicate public " ^ n ^ " in " ^ fsm.fsm_name)
      (List.map snd fsm.fsm_public);

    report_duplicate (fun n -> "duplicate state " ^ n ^ " in " ^ fsm.fsm_name)
      fsm.fsm_states;

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ fsm.fsm_name)) fsm.fsm_locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ fsm.fsm_name)
      (List.map snd fsm.fsm_locals);


      
  type translation_environment = {
    scope : symbol_table;   (* symbol table for vars *)
    in_switch : bool;
    case_labels : list ref; (* known case labels *)
    exception_scope : exception_scope; (* sym tab for exceptions *)
    state_labels : label list ref; (* labels on statements *)
    forward_gotos : label list ref; (* forward goto destinations *)
  }


  type symbol_table = {
    parent : symbol_table option;
    variables : variable_decl list
  }

  let rec find_variable (scope : symbol_table) name =
    try
      List.find (fun (s, _, _, _) -> s = name) scope.variables
    with Not_found ->
      match scope.parent with
        Some(parent) -> find_variable parent name
      | _ -> raise Not_found


  (* Type of each variable (global, formal, or local *)
  let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
  StringMap.empty (globals @ publics @ locals) I love that
  func.formals @ func.locals )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in




(************** THIS IS FAKE NEWS ***************)

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
  StringMap.empty (globals @ func.formals @ func.locals )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
  Literal _ -> Int
      | BoolLit _ -> Bool
      | Id s -> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
  (match op with
          Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
  | Equal | Neq when t1 = t2 -> Bool
  | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
  | And | Or when t1 = Bool && t2 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Unop(op, e) as ex -> let t = expr e in
   (match op with
     Neg when t = Int -> Int
   | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
           string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
             " = " ^ string_of_typ rt ^ " in " ^ 
             string_of_expr ex))
      | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et = expr e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           fd.typ
    in

    let check_bool_expr e = if expr e != Bool
     then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
  Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
           
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in

    stmt (Block func.body)
   
  in
  List.iter check_function functions
*)
