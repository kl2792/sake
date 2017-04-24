(* AST *)
type op = Add | Sub | Mul | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or
type uop = Neg | Not
type dtype = (* built-in primitives + custom user type *)
  | Bool | Int | Char | String
  | Enum of string (* just the name of the enum *)
type lvalue = dtype * string
type expr =
  | BoolLit of bool
  | CharLit of char
  | IntLit of int
  | StringLit of string
  | Variable of string
  | Access of string * string
  | Uop of uop * expr
  | Binop of expr * op * expr
  | Assign of string * expr
  | Printf of string * expr list
  | Empty
type stmt =
  | Block of stmt list
  | State of string
  | If of expr * stmt * stmt
  | For of string * int * int * int * stmt
  | While of expr * stmt
  | Switch of expr * (expr * stmt) list
  | Expr of expr
  | Goto of string (* for FSM transitions *)
type type_decl = {
  type_name   : string;
  type_values : string list;
}
type fsm_decl = {
  fsm_name  : string;
  fsm_public: (dtype * string * expr) list;
  fsm_locals: (dtype * string * expr) list;
  fsm_states: string list;
  fsm_body  : stmt list;
}
type program = {
  input : lvalue list;
  output: lvalue list;
  types : type_decl list;
  public: (dtype * string * expr) list;
  fsms  : fsm_decl list;
}
