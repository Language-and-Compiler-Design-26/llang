
open Llang.Parser_utils
open Llang.Interpreter
open Llang.Typing
open Llang.Compiler_llvm

let () = print_endline "Welcome to the L language REPL & Compiler! (in OCaml)"

let () =
  print_endline "Insert an expression. Ctrl+D to exit.";
  let rec loop () =
    print_string "> "; flush stdout;
    match read_line () with
    | s ->
        let ast = parse_string s in
        begin try
          Printf.printf "\n-- Parser --\n";
          unparse_ast ast |> print_endline;
          (try
            Printf.printf "\n-- Untyped Interpreter --\n\n";
            ast |> eval |> string_of_value |> Printf.printf "= %s\n%!";
            with Failure msg ->
              Printf.eprintf "Interpreter Error: %s\n%!" msg);
          begin try
            Printf.printf "\n-- Type Checking --\n\n";
            let ast_typed = type_synth ast in
            ast_typed |> Llang.Ast_typed.type_of |> string_of_btype |> Printf.printf "= %s\n%!";
            (try
                Printf.printf "\n-- LLVM --\n\n";
                Llang.Llvm_utils.string_of_program ast_typed |> print_endline;
                Llang.Llvm_utils.write_llvm_text_file ast_typed "output.ll";
                Llang.Llvm_api_utils.write_bitcode_file ast_typed "output.bc"
              with Failure msg ->
                Printf.eprintf "LLVM Text Error: %s\n%!" msg);
            with Failure msg ->
              Printf.eprintf "Type Checking Error: %s\n%!" msg
          end;
          with Failure msg ->
            Printf.eprintf "Unparser Error: %s\n%!" msg
        end;
        loop ()
    | exception End_of_file -> print_endline "\nGoodbye!"
  in
  loop ()