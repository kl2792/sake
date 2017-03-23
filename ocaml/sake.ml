(* Generating the C header file and C++ LLVM code *)
let header_name = Sys.argv.(1) ^ ".h" and llvm_name = Sys.argv.(1) ^ ".cpp" in 
   let in_channel = open_in Sys.argv.(1) in
   let lexbuf = Lexing.from_channel in_channel in 
   let program = Parser.program Scanner.token lexbuf in
   let sast = Semantic.check_program program in 
   let header = Header_generator.string_of_sast sast and llvm = LLVM_generator.string_of_sast sast 
   in Printf.fprintf (open_out header_name) "%s" header; Printf.fprintf (open_out llvm_name) "%s" llvm
