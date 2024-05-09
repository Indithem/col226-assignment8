open FrontEnd.Ast;;


(*program is an Queue of clause_ast*)
type program = clause_ast Queue.t;;
type subs = Unification.Subst.substitution;;
include Unification.Subst;;

type main_return_t = subs Seq.t;;

let rec prolog_main (database:program) goal :main_return_t=
  if Sys.getenv_opt "DEBUG" = Some("1") then FrontEnd.Ast_printers.print_goal_ast goal 0;
  match goal with
    | OfCourse -> Seq.return StringMap.empty
    | Atomic t ->
      let single_clause clause = 
        if (Sys.getenv_opt "DEBUG")= Some("1") then FrontEnd.Ast_printers.print_clause_ast clause ;
        try
          let subs = Unification.Mgu.mgu clause.head t in
          match clause.body with
            | None -> Seq.return subs
            | Some goal_ast ->
              let new_goal = subst_goal_ast subs goal_ast in
              prolog_main database new_goal
        with
          | Unification.Mgu.NOT_UNIFIABLE -> Seq.empty
      in
      Seq.concat(
        Seq.map
        single_clause
        (Queue.to_seq database)
      )

    | Or (goal1, goal2) ->
      Seq.append
      (prolog_main database goal1)
      (prolog_main database goal2)
    | Not g1 ->
      let s1 = prolog_main database g1 in
      if Seq.is_empty s1 then
        Seq.return StringMap.empty
      else
        Seq.empty
    | And (g1, g2) ->
      let s1 = prolog_main database g1 in
      Seq.flat_map
      (fun subs -> 
        let s2 = prolog_main database (subst_goal_ast subs g2) in
        Seq.map
        (fun subs2 -> compose_substitutions subs subs2)
        s2        
      )
      s1


let print_subst sub =
  FrontEnd.Ast_printers.print_gaps 0;
  if StringMap.is_empty sub then
    print_endline "true."
  else
  StringMap.iter
  (fun x term ->
    print_string x;
    print_string " = ";
    FrontEnd.Ast_printers.print_term term 1;
  )
  sub

exception Unreachable;;
let main_program goal database =
  let result = prolog_main database goal in
  Seq.iter
  (fun subs ->
    print_endline "\na solution:";
    print_subst subs;
    )
  result;
  print_endline "\x1B[31m\nfail\x1B[0m"
    