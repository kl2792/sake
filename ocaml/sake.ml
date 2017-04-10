(*let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
    let header = Header_generator.translate name ast in 
    Llvm_analysis.assert_valid_module header;
    print_string (Llvm.string_of_llmodule header)
*)
    
     let lexbuf = Lexing.from_channel stdin in 
      let ast = Parser.program Scanner.token lexbuf in
        let llvm =  Header_generator.translate "test" ast (* sast *) in
        print_string(llvm) 

    (*

    Semant.check ast
      match action with
          Ast -> print_string (Ast.string_of_program ast)
  | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
  | Compile -> let m = Codegen.translate ast in
    Llvm_analysis.assert_valid_module m;
        print_string (Llvm.string_of_llmodule m)


*)(*(* Generating the C header file and C++ LLVM code *)
let name = (*String.lowercase_ascii *) Sys.argv.(1) in 
  let header_name = name ^ ".h" and llvm_name = name ^ ".ll" in 
  let in_channel = open_in Sys.argv.(1) in 
  let lexbuf = Lexing.from_channel in_channel in  
  let ast = Parser.program Scanner.token lexbuf in
  (* let sast = Semantic.check_program program in *)
  let header = Header_generator.translate name ast (* sast *)
    (* and llvm = Llvm_generator.translate name ast (* sast *) *) in
      Printf.fprintf (open_out header_name) "%s" header;
      (*Printf.fprintf (open_out llvm_name) "%s" llvm *)*)
