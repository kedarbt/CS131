let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

let accept_all derivation string = Some (derivation, string)

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let test_1 = ((parse_prefix awkish_grammar accept_all ["7"]) = Some ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "7"])], [])) 

type english_nonterminals =
  | Sentence | NP | VP | Noun | Verb | PP | P

let english = 
(Sentence,
  function 
	| Sentence -> [[N NP; N VP]]
    	| NP -> [[N Noun]]
    	| VP -> [[N Verb; N NP]; [N Verb]; [N Verb; N NP; N PP]; [N Verb; N PP]]
    	| Noun -> [[T "boy"]; [T "class"]]
	| Verb -> [[T "codes"];]
	| PP -> [[N P; N NP]]
	| P -> [[T "in"]]
)

let test_2 = ((parse_prefix english accept_empty_suffix ["boy"; "codes"; "in"; "class"])
  = Some
 ([(Sentence, [N NP; N VP]); (NP, [N Noun]); (Noun, [T "boy"]);
   (VP, [N Verb; N PP]); (Verb, [T "codes"]); (PP, [N P; N NP]); (P, [T "in"]);
   (NP, [N Noun]); (Noun, [T "class"])], []))
