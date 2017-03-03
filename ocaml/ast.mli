type op = Add | Sub | Mul | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or
type uop = Neg | Not
type utype = (* user-defined types *)
  | Types of literal list (* arrays will be broken down *)
  | Enum of string list
type dtype = (* built-in primitives *)
  | Bool | Int | Char | Float
  | Array of dtype * int
  | Custom of utype
type lvalue = dtype * string
type literal = (* literal that is optionally an array; note that strings are arrays *)
  | Bool of bool
  | Char of char
  | Int of int
  | Float of float
  | Range of literal * literal * literal (* only valid for bool, char, int *)
  | Array of literal list (* arrays may not have arrays as members *)
type fsm_call = Create | Sim | Reach | Tick | Reset
type expr = (* Note: Call ~ func_decl : Fsm_call ~ fsm_decl *)
  | Literal of literal
  | Variable of lvalue
  | Uop of uop * expr
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Fsm_call of string * fsm_call * expr list
  | Tern of expr * expr * expr
  | Empty
type stmt = 
  | Block of stmt list
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Expr of expr
  | Goto of string
  | Return of expr (* an FSM may terminate by using 'return' *)
type type_decl = {
  name  : string;
  types : utype;
}
type state_decl = {
  name : string;
  body : stmt list;
}
type fsm_decl = {
  name   : string;
  locals : lvalue list;
  input  : lvalue list;
  output : lvalue list;
  body   : state_decl list;
}
type func_decl = {
  return  : dtype;
  name    : string;
  formals : lvalue list;
  locals  : lvalue list;
  body    : stmt list;
}
type program = {
  types : type_decl list;
  fsms  : fsm_decl list;
  funcs : func_decl list;
}
