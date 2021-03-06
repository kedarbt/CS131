let subset_test0 = subset [2;5] [2;7;5;6;9]
let subset_test1 = not (subset [1;6] [1;3;4;5;9])

let my_eq_set_test0 = equal_sets [2;1;3;2] [1;2;3;2]
let my_eq_set_test1 = not (equal_sets [7;8;9] [6;7;8])

let my_set_diff_test0 = equal_sets (set_diff [4;5;6;7] [5;6;7]) [4]
let my_set_diff_test1 = equal_sets (set_diff [1;2;3] [1;4;5]) [2;3]

let my_set_intersection_test0 = equal_sets (set_intersection [1;2] [3;4]) []
let my_set_intersection_test1 = equal_sets (set_intersection [1;2] [1]) [1]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x +. 0.0000000000000000001) 1.

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> -x) 2 (-1) = -1

let my_while_away_test0 = equal_sets (while_away ((+) 3) ((<) 10) 0) []

let my_rle_decode_test0 = equal_sets (rle_decode [0, 2.0; 2, 0.0]) [0.0; 0.0]

type my_nonterminals = 
	  | Expr | CS131 | COMSCI | HW1 | October

let my_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N October];
    Expr, [N Expr; N HW1; N Expr];
    Expr, [N CS131];
    Expr, [N COMSCI; N CS131];
    Expr, [N CS131; N COMSCI];
    CS131, [T"$"; N Expr];
    COMSCI, [T"++"];
    COMSCI, [T"--"];
    HW1, [T"+"];
    HW1, [T"-"];]

let my_grammar = Expr, my_rules

let my_filter_blind_alleys_test0 = filter_blind_alleys my_grammar = my_grammar
