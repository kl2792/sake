(* Generating the C header file and C++ LLVM code *)
let name = String.lowercase_ascii Sys.argv.(1) in 
  let header_name = name ^ ".h" and llvm_name = name ^ ".cpp" in 
  let in_channel = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel in_channel in 
  let program = Parser.program Scanner.token lexbuf in
  (* let sast = Semantic.check_program program in *)
  let header = Header_generator.string_of_sast name program (* sast *)
      and llvm = Llvm_generator.string_of_sast name program (* sast *) in
    Printf.fprintf (open_out header_name) "%s" header;
    Printf.fprintf (open_out llvm_name) "%s" llvm
