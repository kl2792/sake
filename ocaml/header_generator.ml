(* 
 * The function translate converts a Sast.program to a string.
 *
 * Authors: Kai-Zhan Lee, Emma Etherington
 *)

module A = Sast

exception Error_thing

(* generate macro declarations with newlines for all types *)
let macros_of_types name types =
  let rec macros_of_type result = function
    | [] -> result ^ "\n"
    | dtype :: types ->
        let i = ref 0 in
        let macro a v = (* accumulator and value *)
          i := !i + 1; a ^ (Printf.sprintf "#define %s_%s_%s %d\n"
          name dtype.A.type_name v !i) in
        let macro = List.fold_left macro "" dtype.A.type_values in 
        macros_of_type (result ^ macro) types in
    macros_of_type "" types

(* generate string of macro declarations for all fsms' state variables *)
let macros_of_fsms name fsms =
  let macros a f =
    let macro a (v, i) =
      a ^ (Printf.sprintf "#define %s_%s_%s %d\n" name f.A.fsm_name v i) in
    a ^ "\n" ^ List.fold_left macro "" f.A.fsm_states in
  List.fold_left macros "" fsms

(* generate macro definitions from named AST *)
let macros_of_ast name ast =
  let types = macros_of_types name ast.A.types in
  let states = macros_of_fsms name ast.A.fsms in
    types ^ "\n" ^ states

let string_of_type = function
  | A.Int -> "int"
  | A.Char -> "char"
  | A.Bool -> "int"
  | A.String -> "char *"
  | A.Enum _ -> "int" 

(* generate input struct declarations *)
let input_struct_of_ast name fsms =  
  let var_of_tuple (t, n) = Printf.sprintf "\t%s %s;\n" (string_of_type t) n in
  let internals = String.concat "" (List.map var_of_tuple fsms.A.input) in 
    Printf.sprintf "struct %s_input {\n%s};\n" name internals

(* generate output struct declations *)
let output_struct_of_ast name fsms =
  let var_of_tuple (t, n) = Printf.sprintf "\t%s %s;\n" (string_of_type t) n in
  let internals = String.concat "" (List.map var_of_tuple fsms.A.output) in
    Printf.sprintf "struct %s_output {\n%s};\n" name internals

(* generate state struct declarations *)
let state_struct_of_ast name program = 	
  let var_of_fsm fsm = Printf.sprintf "\tint %s;\n" fsm.A.fsm_name in 
  let state_internals = List.map var_of_fsm program.A.fsms in
  let state_internals = String.concat "" state_internals in
  let var_of_public (t, n, _) = "\t" ^ (string_of_type t) ^ " " ^ n ^ ";\n" in
  let fsm_local_vars = List.map var_of_public program.A.public in 
  let fsm_local_vars = String.concat ";\n\t" fsm_local_vars in
  Printf.sprintf "struct %s_state {\n\tint _running;\n%s%s};\n"
    name state_internals fsm_local_vars

    (* generate the struct declarations from fsms in ast *) 
let structs_of_ast name ast = 
  let input_struct = input_struct_of_ast name ast 
  and output_struct = output_struct_of_ast name ast
  and state_struct = state_struct_of_ast name ast in 
  input_struct ^ "\n" ^ output_struct ^ "\n" ^ state_struct

(* generate prototype of tick function, given a name *)
let tick_prototype name =
  Printf.sprintf "struct %s_state *%s_tick(struct %s_state *, struct %s_input *, struct %s_output *);\n" 
    name name name name name

(* the ifdef ... endif guard *)
let header_guard name macros structs tick =
  let upper = name in
  Printf.sprintf "#ifndef __%s_H__\n#define __%s_H__\n\n%s\n%s\n%s\n#endif\n" 
    upper upper macros structs tick

(* convert named AST to header file *)
let translate name ast =
  let macros = macros_of_ast name ast
  and structs = structs_of_ast name ast 	
  and tick = tick_prototype name in
    header_guard name macros structs tick
