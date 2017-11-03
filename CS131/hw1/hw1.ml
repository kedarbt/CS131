
let rec subset a b = match a with
	| [] -> true
	| h::t -> (List.mem h b) && (subset t b);; (*checks if the head of the list is in b and then if it is recursively searches*)

let equal_sets a b = (subset a b) && (subset b a);; (*if both are subsets of each other they are the same*)

let rec set_union a b = match a with (*check if the head of the list is in list b if not then append to the union set*)
	| [] -> b
	| h::t -> if (List.mem h b) then set_union t b else (h::(set_union t b));;

let rec set_intersection a b = match a with (*if the head of the list in is in list b append to the intersection set*)
	| [] -> []
	| h::t -> if (List.mem h b) then (h::(set_intersection t b)) else set_intersection t b;;

let rec set_diff a b = match a with (* if the head of the list is not in list b then append to the difference list*)
	| [] -> []
	| h::t -> if (List.mem h b) then set_diff t b else h::(set_diff t b);;

let rec computed_fixed_point eq f x = if (eq (f x) x) then x else computed_fixed_point eq f (f x);; (* if the computed fixed point isnt found then try f (f x) and so on *)

let rec computed_periodic_point eq f p x = match p with (* 0 or 1 means its a simple use of computed fixed point*)
	| 0 -> x
	| _ -> if eq x (f (computed_periodic_point eq f (p-1) (f x))) then x else (computed_periodic_point eq f p (f x));; (*other values means you have to compute the periodic point of f p (f x) *)

let rec while_away s p x = if (p x) then x::(while_away s p (s x)) else [];; (*if the predicate is true then keep recursing *)

let rec rle_decode lp = match lp with (*if number of reps is non-zero then recurse through the list and append one to the list*)
	| [] -> []
	| (reps, value)::t -> if reps > 0 then value::rle_decode ((reps-1,value)::t) else rle_decode(t);;

type ('nonterminal, 'terminal) symbol = (*symbol definition*)
  | N of 'nonterminal
  | T of 'terminal;; 

let rec type_match_eq_sets (x1,g1) (x2,g2) = equal_sets x1 x2;; (*fixes type errors*)

let rec is_terminal exp safe = match exp with (*if the expression is in the list of safe rules or if it is preceded by a T it is terminal*)
        | N exp -> List.mem exp safe (*check if the expression is in the safe list*)
        | T _ -> true;; (*if the expression starts with T then it is terminal*)

let rec is_safe rules safe = match rules with (* if rule is terminal then it is safe *)
        | [] -> true (*blank expression matches with terminal*)
        | h::t -> if not (is_terminal h safe) then false else (is_safe t safe);; (*if its not terminal then it is not a safe rule*)

let rec safe_rules safe orig = match orig with (* checks original rules and generates a safe list by calling is_safe which calls is_terminal*)
        | [] -> safe (*null rules match with terminal rules*)
        | h::t -> let exp = (fst h) in 
		if not (List.mem exp safe) && (is_safe (snd h) safe) then (safe_rules (exp::safe) t) else (safe_rules safe t);;

let rec type_match_safe_rules (safe, orig) = (safe_rules safe orig), orig;; (* types were messing up so these wrappers were needed *)

let rec find_correct_rules correct_rules original_rules = match original_rules with (* takes the original rules and uses helper functions to get rid of blind_alley_rules*)
        | [] -> []
	| h::t -> if not (is_safe (snd h) correct_rules) then find_correct_rules correct_rules t else h::(find_correct_rules correct_rules t);;

(*list of safe rules originally starts out as an empty list*)
let filter_blind_alleys g = 
	let correct_rules = [] in 
	(fst g), (find_correct_rules (fst (computed_fixed_point type_match_eq_sets type_match_safe_rules (correct_rules, (snd g)))) (snd g));;





