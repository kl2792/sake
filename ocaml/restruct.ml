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

let look_for vl type_dec=
  find_val vl 1 type_dec.A.type_values

let rec is_there_res = function
  | [] -> (-1)
  | [x] -> if(x = (-1)) then is_there_res [] else x
  | x::tl -> if(x = (-1)) then is_there_res tl else x

let rec look_in_states vl = function
  | [] -> (-1)
  | [(name,num)] -> if (name=vl) then num else look_in_states vl []
  | (name,num)::tl -> if (name=vl) then num else look_in_states vl tl

let rec get_expr sts program = function (* A.expr *)
  | A.BoolLit(bl) -> S.BoolLit(bl)
  | A.CharLit(ch) -> S.CharLit(ch)
  | A.IntLit(num) -> S.IntLit(num)
  | A.StringLit(name) -> S.StringLit(name)
  | A.Variable(name) -> S.Variable(name)
  | A.EnumLit(vl) -> 
    let result =
      let enum_search = List.map (look_for vl) program.A.types in
      is_there_res enum_search in
    if (result <> (-1))
      then S.IntLit(result)
      else
        let is_state = look_in_states vl sts in
        if (is_state <> (-1)) then S.IntLit(is_state) else (wrong_enum_error vl)
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
and take_stmts sts program = function (* stmt list *)
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

let rec take_typ = function
  [] -> []
  | {A.type_name = name; A.type_values=vals}::tl -> {S.type_name = name; S.type_values = vals}::(take_typ tl)

let rec copy_locals sts program = function (* (dtype * string * expr) list *)
  [] -> []
  | (typ,var_name,expr)::tl -> ((convert_type typ),var_name,(get_expr sts program expr)):: (copy_locals sts program tl)

let rec get_states num = function (* body: stmt list *)
  [] -> []
  | A.State(name)::tl -> (name,num):: (get_states (num+1) tl)
  | _::tl -> get_states num tl

let rec take_fsm sts program = function
  [] -> []
  | {A.fsm_name = name; A.fsm_public = _ ; A.fsm_locals = local; A.fsm_body =  body}::tl
      -> { S.fsm_name = name; S.fsm_locals = (copy_locals sts program local); S.fsm_states = (get_states 1 body); S.fsm_body = (take_stmts sts program body)}::(take_fsm sts program tl)

let rec take_pubs sts program name = function (* (dtype * string * expr) list *)
  [] -> []
  | (typ,var_name,expr)::tl -> ((convert_type typ),name ^ "_" ^ var_name,(get_expr sts program expr)):: (take_pubs sts program name tl)

let rec get_pubs sts program = function
  [] -> []
  | {A.fsm_name = name; A.fsm_public = pubs; A.fsm_locals = _ ; A.fsm_body =  _ }::tl
      -> (take_pubs sts program name pubs) @ (get_pubs sts program tl)

let rec muddle_it_all = function
  | [] -> []
  | [l1] -> l1
  | l1::tl -> l1 @ muddle_it_all tl

let yank_out_states fsm =
  get_states 1 fsm.A.fsm_body

let get_all_states fsms =
  let state_fam = List.map (yank_out_states) fsms in
  muddle_it_all state_fam

let convert i o typs fsms program = function
  | _ -> let all_sts = get_all_states fsms in {S.input = take_in i; S.output = take_out o; S.public = get_pubs [("",0)] program fsms; S.types = take_typ typs; S.fsms = take_fsm all_sts program fsms}

let transform program =
  convert program.A.input program.A.output program.A.types program.A.fsms program []