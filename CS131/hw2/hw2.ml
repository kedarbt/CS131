type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let rec parse_rule_list nt_symbol rules =  match rules with
	| [] -> []
	|(lhs, rhs)::other_rules -> if lhs = nt_symbol then (rhs::(parse_rule_list nt_symbol other_rules)) else parse_rule_list nt_symbol other_rules;;
	(* if the lhs matches the non terminal symbol, then append the rest of the rules to the right hand side *)

let convert_grammer (ss,rules) = (ss, parse_rule_list rules);; (*parse rules and format*)

 	
let rec match_acceptor rules check_rule accept deriv frag = match check_rule with
        | [] -> accept deriv frag (*rules have been exhausted, validate derivation through acceptor*)
	| _ -> match frag with 
		| [] -> None (*if there are rules left but no fragment, then this derivation is invalid*) 
		| prefix::suffix -> match check_rule with
			| (T expr)::rhs -> if prefix=expr then match_acceptor rules rhs accept deriv suffix else None (*check rest of fragment*)
			| (N expr)::rhs -> match_rules expr rules (rules expr) (match_acceptor rules rhs accept) deriv frag (*generate new acceptor for new suffix*)
			| [] -> None (*just to get rid of non-exhaustive pattern matching warning*)

and match_rules ss rules start_rule accept deriv frag =
         match start_rule with
                | [] -> None 
                |f_r::r_r ->
                        let match_result = (match_acceptor rules f_r accept (deriv@[ss, f_r]) frag) in
                                match match_result with
                                        | None -> match_rules ss rules r_r accept deriv frag (* the rule was invalid so head back into recursion*)
                                        | _ -> match_result (*return whatever the other function returned*)

let parse_prefix (ss, rules) accept frag = 
	let deriv = [] in
		match_rules ss rules (rules ss) accept deriv frag	
