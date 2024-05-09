open Ast;;

let print_gaps n =
  Printf.printf "\n";
  for _ = 1 to n do
    Printf.printf "  "
  done;
  if n > 0 then Printf.printf "|__"
;;

  
let rec print_term (t:term) p = 
  print_gaps p;
  match t with
  | Var v -> Printf.printf "Variable %s" v
  | Const_atom a -> Printf.printf "Atom '%s'" a
  | Const_int i -> Printf.printf "Int %d" i
  | K'ary {symbol; args} -> 
    Printf.printf "%d-ary term '%s'" (List.length args) symbol;
    List.iter (fun x -> print_term x (p+1)) args


let rec print_head_terms ht p= 
  print_gaps p;
  match ht with 
  | Var v -> Printf.printf "Variable %s" v
  | Const_atom a -> Printf.printf "Atom '%s'" a
  | Const_int i -> Printf.printf "Int %d" i
  | K'ary {symbol; args} -> 
    Printf.printf "%d-ary term '%s'" (List.length args) symbol;
    List.iter (fun x -> print_head_terms x (p+1)) args
  | Skip -> Printf.printf "Skip"

let rec print_goal_ast g p = match g with
  | Atomic t -> print_term t p
  | And (g1, g2) -> 
    print_gaps p;
    Printf.printf "And Goal";
    print_goal_ast g1 (p+1);
    print_goal_ast g2 (p+1)
  | Or (g1, g2) ->
    print_gaps p;
    Printf.printf "Or Goal";
    print_goal_ast g1 (p+1);
    print_goal_ast g2 (p+1)
  | Not g1 ->
    print_gaps p;
    Printf.printf "Not Goal";
    print_goal_ast g1 (p)
  | OfCourse ->
    print_gaps p;
    Printf.printf "OfCourse!"

let print_clause_ast c =
  let base = 1 in
  match c.body with
    | None -> 
      Printf.printf "\nFact:";
      print_head_terms c.head base
    | Some _ ->
      Printf.printf "\nRule: ";
      print_head_terms c.head base;
      Printf.printf "\nBody: ";
      print_goal_ast (Option.get c.body) base
  ;
;;

let print_appropriately s =
  begin
  match s with
  | ClauseStatement c -> print_clause_ast c
  | GoalStatement g -> 
    Printf.printf "\nGoal: ";
    print_goal_ast g 0;
  end;
  Printf.printf "\n"; 
;;