open FrontEnd.Ast;;


(*program is an Queue of clause_ast*)
type program = clause_ast Queue.t;;
type subs = Unification.Subst.substitution;;
include Unification.Subst;;

type main_return_t = subs Seq.t;;

let rec prolog_main (database:program) goal (prev_subs_list:main_return_t):main_return_t=
  
  match goal with
    | OfCourse -> Seq.return StringMap.empty
    | Atomic t ->
      let single_clause clause = 
        let ret_seq = Seq.filter_map
        (fun sub ->
          try
            let new_sub = Unification.Mgu.mgu clause.head (subst sub t) in
            Some (new_sub)
          with
            | Unification.Mgu.NOT_UNIFIABLE -> None
        )
        prev_subs_list in
        match clause.body with
          | None -> ret_seq
          | Some goal_body -> 
            prolog_main database goal_body ret_seq
      in
      Seq.concat(
        Seq.map
        single_clause
        (Queue.to_seq database)
      )

    | _ -> Seq.empty

exception Unreachable;;
let main_program goal database =
  ()
    