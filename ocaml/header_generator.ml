open Ast

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

let structs_of_sast name sast = name (* your work here, please, emma *)

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
