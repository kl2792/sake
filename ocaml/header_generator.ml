module A = Ast

(* generate enum declaration with newlines for all types *)
let enums_of_types name types =
  let enum_of_type name typ = 
    let values = List.map (fun s -> s.name) typ.types in
    let values = String.concat ", " values in
      Printf.sprintf "enum %s_%s_enum_t {%s};\n" name typ.name types
  in
  let enums = List.map (enum_of_type name) types in
    String.concat "" enums

(* generate string of enum declaration for all fsms' state variables *)
let enums_of_fsms name fsms =
  let enum_of_fsm name fsm = 
    let states = List.map (fun s -> s.body.name) fsm.body in
    let states = String.concat ", " states in
      Printf.sprintf "enum %s_%s_state_t {%s};\n" name fsm.name states
  in
  let enums = List.map (enum_of_fsm name) fsms in
    String.concat "" enums

(* generate enum declarations from named SAST *)
let enums_of_sast name sast =
  let types = enums_of_types name sast.types
      and states = enums_of_fsms name sast.fsms in
    types ^ "\n" ^ states

let string_of_type name = function
  | A.Int -> "int"
  | A.Char -> "char"
  | A.Bool -> "int"
  | A.Array -> "DON'T USE THIS"
  | A.Enum(string) -> Printf.sprintf "enum %s_%s_enum_t" name 

(* generate input struct declarations *)
let input_struct_of_sast name fsms =  (* your work here, please, emma *)
  let input_of_fsm name fsm = 
    let input_internals = List.map (fun s -> s.input.type ^ " " ^ s.input.name) fsm.input in 
    let input_internals = String.concat ";\n" input_internals in
       Printf.sprintf "struct %s_input {\n%s;\n};\n" name input_internals

(* generate output struct declations *)
let output_struct_of_sast name fsms =  (* your work here, please, emma *)
  let output_of_fsm name fsm = 
    let output_internals = List.map (fun s -> s.output.type ^ " " ^ s.output.name) fsm.input in 
    let output_internals = String.concat ";\n" output_internals in
       Printf.sprintf "struct %s_output {\n%s;\n};\n" name output_internals
       
(* generate state struct declarations *)
let state_struct_of_sast name fsms = 
  let state_of_fsm name fsm =
    let fsm_local_vars = List.map (fun s -> s.locals.type ^ " " ^ fsm.name ^ "_" ^  s.locals.name) fsm.locals
    let fsm_local_vars = String.concat ";\n" fsm_local_vars in
       Printf.sprintf "enum %s_%s_state_t %s;\n %s;\n" name fsm.name fsm.name fsm_local_vars
  in
  let state_internals = List.map (state_of_fsm name) fsms in
    let state_internals = String.concat "" state_internals in
       Printf.sprintf "struct %s_name {\n%s};\n" name state_internals 

(* generate the struct declarations from fsms in sast *) 
let structs_of_sast name sast = 
  let input_struct = input_struct_of_sast name sast.fsms 
    and output_struct = output_struct_of_sast name sast.fsms
    and state_struct = state_struct_of_sast name sast.fsms
  in 
    input_struct ^ "\n" ^ output_struct ^ "\n" ^ state_struct

(* generate prototype of tick function, given a name *)
let tick_prototype name =
  Printf.sprintf "int %s_tick(struct %s_state *,"
    ^ " struct %s_input *, struct %s_output *);\n" name name name name

let header_guard name enums structs tick =
  let upper = String.uppercase_ascii name in
    Printf.sprintf "#ifndef __%s_H__\n#define __%s_H__\n\n%s\n%s\n%s\n#endif"
      upper upper enums structs tick

(* convert named SAST to header file *)
let string_of_sast name sast =
  let enums = enums_of_sast name sast
      and structs = structs_of_sast name sast 
      and tick = tick_prototype name in
    header_guard name enums structs tick
