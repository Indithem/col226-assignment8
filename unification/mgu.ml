open FrontEnd.Ast;;

exception NOT_UNIFIABLE;;
let mgu h t :Subst.substitution=
  let rec check_var_in_ht x ht = 
    match ht with
    | Var x when x=x -> raise NOT_UNIFIABLE
    | K'ary {args = a;_} -> List.iter (check_var_in_ht x) a
    | _ -> ()
  in
  let rec mgu_helper h t s :Subst.substitution= 
    match Subst.subst_heads s h, Subst.subst s t with
      | Skip, _ -> Subst.StringMap.empty

      | Var x, Var y when x = y -> Subst.StringMap.empty
      | Var x, Var y when x <> y -> Subst.StringMap.singleton x ((Var y):term)
      | Var x, K'ary k -> 
        check_var_in_ht x (Subst.term_to_head_term (K'ary k));
        Subst.StringMap.singleton x ((K'ary k):term)
      | K'ary k, Var y ->
        check_var_in_ht y (K'ary k);
        Subst.StringMap.singleton y (Subst.head_term_to_term (K'ary k))
      | Var x, t ->
        Subst.StringMap.singleton x t
      | t, Var x ->
        Subst.StringMap.singleton x (Subst.head_term_to_term t)

      | K'ary {symbol = s1; args = a1}, K'ary {symbol = s2; args = a2} when s1 = s2 -> 
        if s1 <> s2 || List.length a1 <> List.length a2 then raise NOT_UNIFIABLE else
          List.fold_left2
          (fun sk t1 t2 -> 
            if Option.is_some (Sys.getenv_opt "DEBUG") then (
            print_endline "t1:";
            FrontEnd.Ast_printers.print_head_terms t1 0;
            print_endline "t2:";
            FrontEnd.Ast_printers.print_term t2 0;);
            Subst.compose_substitutions sk (mgu_helper t1 t2 sk)
          )
          Subst.StringMap.empty a1 a2

      | h, t -> if Subst.term_to_head_term t <> h then raise NOT_UNIFIABLE else s
  in
  mgu_helper h t Subst.StringMap.empty;;