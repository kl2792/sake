module A = Ast

(* generate enum declaration with newlines for all types *)
let enums_of_types name types = 
	let enum_of_type name dtype = 
		let values = List.map String.uppercase_ascii dtype.A.type_values in
		let values = String.concat ", " values in
			Printf.sprintf "enum %s_%s_enum_t {%s};\n" name dtype.A.type_name values
	in let enums = List.map (enum_of_type name) types in String.concat "" enums 

(* generate string of enum declaration for all fsms' state variables *)
let enums_of_fsms name fsms =
	let enum_of_fsm name fsm = 
		let string_of_state s = String.uppercase_ascii s.A.state_name in
		let states = List.map string_of_state fsm.A.fsm_body in
		let states = String.concat ", " states in
			Printf.sprintf "enum %s_%s_state_t {%s};\n" name fsm.A.fsm_name states
		in
	let enums = List.map (enum_of_fsm name) fsms in String.concat "" enums

(* generate enum declarations from named SAST *)
let enums_of_sast name sast =
	let types = enums_of_types name sast.A.types in
	let states = enums_of_fsms name sast.A.fsms in
		types ^ "\n" ^ states

let string_of_type name = function
	|	A.Int -> "int"
	| A.Char -> "char"
	| A.Bool -> "int"
	| A.Array(array_name, length) -> "DON'T USE THIS" (* TODO: implement array *)
	| A.Enum(type_name) -> Printf.sprintf "enum %s_%s_enum_t" name type_name 

(**** EMMA'S PART ****)

(* generate input struct declarations *)
let input_struct_of_sast name fsms =  
	(*let input_internals = List.map (fun s -> s.input.ty ^ " " ^ s.input.name) fsms.input in *)
	let input_internals = List.map (fun s -> (*(string_of_ty s.A.output) ^*) " " ^ s.A.input.A.name) fsms.A.input in 
	let input_internals = String.concat ";\n" input_internals in
	printf.sprintf "struct %s_input {\n%s;\n};\n" name input_internals

(* generate output struct declations *)
let output_struct_of_sast name fsms =  
	let output_internals = List.map (fun s -> (*s.output.ty ^*) " " ^ s.A.output.A.name) fsms.A.output in 
	let output_internals = String.concat ";\n" output_internals in
	Printf.sprintf "struct %s_output {\n%s;\n};\n" name output_internals


(* generate state struct declarations *)
let state_struct_of_sast name fsms = 
	let state_of_fsm name fsm =
		let fsm_local_vars = List.map (fun s -> (*s.locals.types ^*) " " ^ fsm.A.name ^ "_" ^  s.A.locals.A.name) fsm.A.locals in 
		let fsm_local_vars = String.concat ";\n" fsm_local_vars in
		Printf.sprintf "enum %s_%s_state_t %s;\n %s;\n" name fsm.A.name fsm.A.name fsm_local_vars
		in
	let state_internals = List.map (state_of_fsm name) fsms in
	let state_internals = String.concat "" state_internals in
	Printf.sprintf "struct %s_name {\n%s};\n" name state_internals 

(* generate the struct declarations from fsms in sast *) 
let structs_of_sast name sast = 
	let input_struct = input_struct_of_sast name sast 
		and output_struct = output_struct_of_sast name sast
		and state_struct = state_struct_of_sast name sast.fsms
	in 
		input_struct ^ "\n" ^ output_struct ^ "\n" ^ state_struct

(**** END EMMA'S PART ***)

(* generate prototype of tick function, given a name *)
let tick_prototype name =
	Printf.sprintf "int %s_tick(struct %s_state *, struct %s_input *, struct %s_output *);\n"
		name name name name

(* the ifdef ... endif guard *)
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
