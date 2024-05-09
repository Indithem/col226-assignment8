let handle_input (ast: FrontEnd.Ast.statement) (program_list: Program.program) =
  match ast with
  | ClauseStatement clause -> 
    Queue.push clause program_list;
  | GoalStatement goal ->
    (*
    Queue.iter 
    (fun clause -> FrontEnd.Ast_printers.print_clause_ast clause)
    program_list;
    *)
    Program.main_program goal program_list;
;;

let main_loop () = 
  let program_list = Queue.create () in
  while true do
    print_string ">>> " ;
    try
    let line = read_line () in
      try
        let statement = FrontEnd.Parser.program FrontEnd.Lexer.logprog_parser (Lexing.from_string line) in
        handle_input statement program_list;
        with
        | Stdlib.Parsing.Parse_error ->
          begin
            let lexbufr = Lexing.from_string line in
            let curr = lexbufr.Lexing.lex_curr_p in
            let line = curr.Lexing.pos_lnum in
            let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
            let tok = Lexing.lexeme lexbufr in
            Printf.printf "\x1B[31mSyntax error at line %d, character %d, token %s\x1B[00m\n" line cnum tok;
          end
        | Failure s -> Printf.printf "\x1B[31mFailure: %s\x1B[00m\n" s
      with
      | End_of_file -> Printf.printf "\x1B[31mEnd of file\x1B[00m\n"; exit 0
  done
in
main_loop () ;;