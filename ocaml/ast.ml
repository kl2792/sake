type op = Add | Sub | Mul | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or
type uop = Neg | Not
type dtype = (* built-in primitives + custom user type *)
  | Bool | Int | Char | String
  | Array of dtype * int
  | Enum of string (* just the name of the enum *)
type lvalue = dtype * string
type fsm_call = Tick | Reset
type expr = (* Note: Call ~ func_decl : Fsm_call ~ fsm_decl *)
  | BoolLit of bool
  | CharLit of char
  | IntLit of int
  | StringLit of string
  | Escape of string
  | Range of int * int * int (* only valid for bool, char, int *)
  | ArrayLit of expr list
  | Variable of string
  | Uop of uop * expr
  | Binop of expr * op * expr
  | Assign of string * expr
(*  | Fsm_call of string * fsm_call * expr list *)
  | Print of string * expr list
  | Cond of expr * expr * expr
  | Empty
type stmt =
  | Block of stmt list
  | If of expr * stmt * stmt
  | For of string * expr * stmt
  | While of expr * stmt
  | Switch of expr * (expr * stmt) list
  | Ldecl of dtype * (string * expr) list (* local decls *)
  | Expr of expr
  | Goto of string (* for FSM transitions *)
type type_decl = {
  type_name   : string;
  type_values : string list;
}
type fsm_decl = {
  fsm_name  : string;
  fsm_states: string list;
  fsm_body  : stmt list;
}
type program = {
  input : lvalue list;
  output: lvalue list;
  public: (dtype * string * expr) list;
  types : type_decl list;
  fsms  : fsm_decl list;
}
