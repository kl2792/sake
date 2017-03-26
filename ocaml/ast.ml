type op = Add | Sub | Mul | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or
type uop = Neg | Not
type dtype = (* built-in primitives + custom user type *)
  | Bool | Int | Char
  | Array of dtype * int
  | Enum of string (* just the name of the enum *)
type lvalue = dtype * string
type literal = (* literal that is optionally an array; note that strings are arrays *)
  | BoolLit of bool
  | CharLit of char
  | IntLit of int
  | Range of int * int * int (* only valid for bool, char, int *)
  | ArrayLit of literal list
  | String of char list
type fsm_call = Tick | Reset
type expr = (* Note: Call ~ func_decl : Fsm_call ~ fsm_decl *)
  | Literal of literal
  | Variable of string
  | Uop of uop * expr
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Fsm_call of string * fsm_call * expr list
  | Cond of expr * expr * expr
  | Empty
type case =
  | CaseValue of expr (* Q: should expr be expr list? *)
  | CaseAny
type stmt =
  | Block of stmt list
  | If of expr * stmt * stmt
  | For of string * expr * stmt
  | While of expr * stmt
  | Expr of expr
  | Switch of expr * (case * stmt) list (* Q: Instead of expr should it be expr list? *)
  | Goto of string (* for FSM transitions *)
  | Return of expr (* for functions *)
type cstmt = case * stmt
type type_decl = {
  name  : string;
  types : string list;
}
type state_decl = {
  name  : string;
  start : bool;
  body  : stmt list;
}
type fsm_decl = {
  name : string;
  body : state_decl list;
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
