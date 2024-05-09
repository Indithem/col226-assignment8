(* abstract syntax tree for the logprog *)

type term =
  | Var of string
  | Const_atom of string
  | Const_int of int
  | K'ary of {symbol: string; args: term list}

type head_term = 
  | Var of string
  | Const_atom of string
  | Const_int of int
  | K'ary of {symbol: string; args: head_term list}
  | Skip

(*since we are using same name of Var, the following syntax 
  might be useful somewhere for distinguishing
  let m: term = Var "m" *)

type goal_ast = 
  | Atomic of term
  | And of goal_ast * goal_ast
  | Or of goal_ast * goal_ast
  | Not of goal_ast
  | OfCourse

type clause_ast =
{
  head: head_term;
  body: goal_ast option;
}
;;

type statement =
  | ClauseStatement of clause_ast
  | GoalStatement of goal_ast
;;
