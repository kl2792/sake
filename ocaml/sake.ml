(* Generating the C header file and C++ LLVM code *)
let name = Sys.argv.(1) in 
  let in_channel = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel in_channel in 
  let ast = Parser.program Scanner.token lexbuf in
  let llvm =  Llvm_generator.translate name ast (* sast *) in
        Llvm_analysis.assert_valid_module llvm;
        print_string (Llvm.string_of_llmodule llvm)

        (*Printf.fprintf (open_out llvm_name) "%s" llvm*)
