open FrontEnd.Ast;;

module StringMap = Map.Make(String);;

(* As we are using a Map, they are always a valid functional relation *)
type substitution = term StringMap.t;;

open FrontEnd.Ast;;

exception Unconvertible;;
let rec head_term_to_term (h:head_term) :term = 
  match h with 
  | Var str -> Var str
  | K'ary {symbol=n; args= c;} -> 
      K'ary {symbol=n; args=List.map head_term_to_term c}
  | Const_atom str -> Const_atom str
  | Const_int i -> Const_int i
  | Skip -> raise Unconvertible
  ;;

let rec term_to_head_term (t:term) :head_term =
  match t with
  | Var str -> Var str
  | K'ary {symbol=n; args= c;} -> 
      K'ary {symbol=n; args=List.map term_to_head_term c}
  | Const_atom str -> Const_atom str
  | Const_int i -> Const_int i
  ;;

let rec subst (sub:substitution) (t:term) = 
  match t with
    | Var str -> 
      (match StringMap.find_opt str sub with
      | Some tree -> tree
      | None -> t)
    | K'ary {symbol=n; args= c;} -> 
        K'ary {symbol=n; args=List.map (subst sub) c}
    | _ -> t
  ;;

let rec subst_heads (sub:substitution) (h:head_term) :head_term= 
  match h with 
  | Var str -> 
    (match StringMap.find_opt str sub with
    | Some tree -> term_to_head_term tree
    | None -> h)
  | K'ary {symbol=n; args= c;} -> 
      K'ary {symbol=n; args=List.map (subst_heads sub) c}
  | _ -> h
  ;; 

let rec subst_goal_ast sub g =
  match g with
    | Atomic t -> Atomic (subst sub t)
    | And (g1, g2) -> And (subst_goal_ast sub g1, subst_goal_ast sub g2)
    | Or (g1, g2) -> Or (subst_goal_ast sub g1, subst_goal_ast sub g2)
    | Not g1 -> Not (subst_goal_ast sub g1)
    | OfCourse -> OfCourse

(*Finds the subsition s such that s(t) = s2(s1(t)) = (s1 o s2) (t)*)
let compose_substitutions (s1:substitution) (s2:substitution) :substitution= 
  let substitue_firsts s1 s2 = 
    StringMap.map
      (fun t -> subst s2 t)
      s1
  in
  let add_left_overs substitution s2 =
    StringMap.union
      (fun _ t1 _ -> Some t1)
      substitution
      s2
  in
  add_left_overs (substitue_firsts s1 s2) s2
;;
