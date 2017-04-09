type op = Add | Sub | Mul | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or
type uop = Neg | Not
type dtype = (* built-in primitives + custom user type *)
  | Bool | Int | Char
  | Array of dtype * int
  | Enum of string (* just the name of the enum *)
type lvalue = dtype * string
type fsm_call = Tick | Reset
type expr = (* Note: Call ~ func_decl : Fsm_call ~ fsm_decl *)
  | BoolLit of bool
  | CharLit of char
  | IntLit of int
  | RangeLit of int * int * int (* only valid for bool, char, int *)
  | ArrayLit of expr list
  | StringLit of char list
  | Variable of string
  | Uop of uop * expr
  | Binop of expr * op * expr
  | Assign of string * expr
(*  | Call of string * expr list (* no functions *) *)
(*  | Fsm_call of string * fsm_call * expr list *)
  | Cond of expr * expr * expr
  | Empty
type stmt =
  | Block of stmt list
  | If of expr * stmt * stmt
  | For of string * expr * stmt
  | While of expr * stmt
  | Expr of expr
  | Switch of expr * (expr * stmt) list (* Q: Instead of expr should it be expr list? *)
  | Goto of string (* for FSM transitions *)
(*  | Return of expr (* for functions, but we're not doing functions *) *)
type cstmt = expr * stmt
type type_decl = {
  type_name   : string;
  type_values : string list;
}
type state_decl = {
  state_name  : string;
  state_start : bool;
  state_body  : stmt list;
}
type fsm_decl = {
  fsm_name : string;
  fsm_body : state_decl list;
}
(*type func_decl = {
  return  : dtype;
  name    : string;
  formals : lvalue list;
  locals  : lvalue list;
  body    : stmt list;
}*)
type program = {
  input : lvalue list;
  output: lvalue list;
  locals: lvalue list;
  types : type_decl list;
  fsms  : fsm_decl list;
  (*funcs : func_decl list;*)
}
