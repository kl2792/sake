(* Generating the C header file and C++ LLVM code *)
let name = (*String.lowercase_ascii *) Sys.argv.(2) in 
  let header_name = name ^ ".h" and llvm_name = name ^ ".ll" in 
  let in_channel = open_in Sys.argv.(1) in 
  let lexbuf = Lexing.from_channel in_channel in  
  let ast = Parser.program Scanner.token lexbuf in
  (* let sast = Semantic.check ast in *)
  let header = Header_generator.translate name ast (* sast *)
     and llvm = Llvm_generator.translate name ast (* sast *) in
      Printf.fprintf (open_out header_name) "%s" header;
      (*print_string (Llvm.string_of_llmodule llvm);*)
      Llvm_analysis.assert_valid_module llvm;
      Printf.fprintf (open_out llvm_name) "%s" (Llvm.string_of_llmodule llvm);
