module A = Ast

(* generate macro declarations with newlines for all types *)
let macros_of_types name types =
  let rec macros_of_type result i = function
    | [] -> result ^ "\n"
    | dtype :: types ->
        let macro = Printf.sprintf "#define %s_%s_%s %d\n"
            name dtype.A.type_name (List.nth dtype.A.type_values i) i in
          macros_of_type (macro ^ result) (i + 1) types in
    macros_of_type "" 0 types

(* generate string of macro declarations for all fsms' state variables *)
let macros_of_fsms name fsms =
  let rec macros_of_fsm result i = function
    | [] -> result
    | fsm :: fsms ->
        let macro = Printf.sprintf "#define %s_%s_%s %d\n"
            name fsm.A.fsm_name (List.nth fsm.A.fsm_body i).A.state_name i in
        macros_of_fsm (macro ^ result) (i + 1) fsms in
    macros_of_fsm "" 0 fsms

(* generate macro definitions from named SAST *)
let macros_of_sast name sast =
	let types = macros_of_types name sast.A.types in
	let states = macros_of_fsms name sast.A.fsms in
		types ^ "\n" ^ states

let string_of_type = function
  | A.Int -> "int"
	| A.Char -> "char"
	| A.Bool -> "int"
	| A.Array(array_name, length) -> "DON'T USE THIS" (* TODO: implement array *)
	| A.Enum(type_name) -> "int"

(* generate input struct declarations *)
let input_struct_of_sast name fsms =  
	let input_internals = List.map (fun s -> (string_of_type (fst s)) ^ " " ^ (snd s)) fsms.A.input in 
	let input_internals = String.concat ";\n" input_internals in
	Printf.sprintf "struct %s_input {\n%s;\n};\n" name input_internals

(* generate output struct declations *)
let output_struct_of_sast name fsms =  
	let output_internals = List.map (fun s -> (string_of_type (fst s)) ^ " " ^ (snd s)) fsms.A.output in 
	let output_internals = String.concat ";\n" output_internals in
	Printf.sprintf "struct %s_output {\n%s;\n};\n" name output_internals


(* generate state struct declarations *)
let state_struct_of_sast name fsms = 
	let state_of_fsm name fsm =
            Printf.sprintf "int %s;\n" fsm.A.fsm_name 
	in
	    let state_internals = List.map (state_of_fsm name) fsms.A.fsms in
	    let state_internals = String.concat "" state_internals in
            let fsm_local_vars = List.map (fun s -> (*s.locals.types ^*) " " ^  (snd s)) fsms.A.locals in 
            let fsm_local_vars = String.concat ";\n" fsm_local_vars in
	        Printf.sprintf "struct %s_name {\n%s\n%s};\n" name state_internals fsm_local_vars

(* generate the struct declarations from fsms in sast *) 
let structs_of_sast name sast = 
	let input_struct = input_struct_of_sast name sast 
		and output_struct = output_struct_of_sast name sast
		and state_struct = state_struct_of_sast name sast
	in 
		input_struct ^ "\n" ^ output_struct ^ "\n" ^ state_struct

(**** END EMMA'S PART ***)

(* generate prototype of tick function, given a name *)
let tick_prototype name =
	Printf.sprintf "int %s_tick(struct %s_state *, struct %s_input *, struct %s_output *);\n"
		name name name name

(* the ifdef ... endif guard *)
let header_guard name macros structs tick =
	let upper = String.uppercase_ascii name in
	Printf.sprintf "#ifndef __%s_H__\n#define __%s_H__\n\n%s\n%s\n%s\n#endif"
			upper upper macros structs tick

			(* convert named SAST to header file *)
let string_of_sast name sast =
	let macros = macros_of_sast name sast
			and structs = structs_of_sast name sast 
			and tick = tick_prototype name in
	header_guard name macros structs tick
