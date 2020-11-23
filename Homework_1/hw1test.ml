(* The test cases used to test my homework 1 solutions
* All test cases below were written by me, Ian Conceicao, except
* otherwise marked Given.
* According to Xinyu Ma https://piazza.com/class/k52xkkxhhjs1i2?cid=49
* we may use spec test cases as well.
*)

(******* PROBLEM 1 *******)

(* Given *)
let my_subset_test0 = subset [] [1;2;3];;
let my_subset_test1 = subset [3;1;3] [1;2;3];;
let my_subset_test2 = not (subset [1;3;7] [4;1;3]);;

(*MINE*)
let my_subset_test3 = subset [1;2;3] [1;2;3];;
let my_subset_test4 = subset [1] [1;2;3];;
let my_subset_test5 = subset [1] [2;1;3;4];;
let my_subset_test6 = subset [1] [2;3;4;1];;
let my_subset_test7 = subset [] [];;
let my_subset_test8 = not (subset [1] []);;
let my_subset_test9 = not (subset [2;3;4] [3;4]);;

(******* PROBLEM 2 *******)

(*Given*)

let my_equal_sets_test0 = equal_sets [1;3] [3;1;3];;
let my_equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3]);;

(*MINE*)
let my_equal_sets_test1 = equal_sets [] [];;
let my_equal_sets_test2 = equal_sets [1] [1];;
let my_equal_sets_test3 = not(equal_sets [1] [1;2]);;
let my_equal_sets_test4 = not(equal_sets [] [2]);;

(******* PROBLEM 3 *******)

(*Given*)
let my_set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3];;
let my_set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3];;
let my_set_union_test2 = equal_sets (set_union [] []) [];;

(*MINE*)
let my_set_union_test3 = equal_sets(set_union [] []) [];;
let my_set_union_test4 = equal_sets(set_union [1] [2]) [1;2];;
let my_set_union_test5 = equal_sets(set_union [1;2] [2]) [1;2];;

(******* PROBLEM 4 *******)

(*Given*)
let my_set_intersection_test0 =
  equal_sets (set_intersection [] [1;2;3]) [];;
let my_set_intersection_test1 =
  equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3];;
let my_set_intersection_test2 =
  equal_sets (set_intersection [1;2;3;4] [3;1;2;4]) [4;3;2;1];;

  (*MINE*)
  let my_set_intersection_test3 =
    equal_sets (set_intersection [] []) [];;

  let my_set_intersection_test4 =
      equal_sets (set_intersection [2] [4;5;6]) [];;

  let my_set_intersection_test5 =
        equal_sets (set_intersection [4;6] [4;5;6]) [4;6];;

  (******* PROBLEM 5 *******)

(*Given*)

let my_set_diff_test0 = equal_sets (set_diff [1;3] [1;4;3;1]) [];;
let my_set_diff_test1 = equal_sets (set_diff [4;3;1;1;3] [1;3]) [4];;
let my_set_diff_test2 = equal_sets (set_diff [4;3;1] []) [1;3;4];;
let my_set_diff_test3 = equal_sets (set_diff [] [4;3;1]) [];;

(*MINE*)
let my_set_diff_test4 = equal_sets (set_diff [1;2;3] [1;4;3;1]) [2];;
let my_set_diff_test5 = equal_sets (set_diff [1;2;3;5;0] [1;4;3;1]) [2;5;0];;
let my_set_diff_test6 = equal_sets (set_diff [] [1;4;3;1]) [];;
let my_set_diff_test7 = equal_sets (set_diff [] []) [];;
let my_set_diff_test8 = equal_sets (set_diff [2;3;4] []) [2;3;4];;

(******* PROBLEM 6 *******)

(*Given*)
let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0;;
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity;;
let my_computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 10. = 1.;;
  let my_computed_fixed_point_test3 =
    ((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
  			 (fun x -> x /. 2.)
  			 10.)
     = 1.25);;

(*MINE*)
let my_computed_fixed_point_test4 =
  computed_fixed_point (=) (fun x -> x / 3) 100 = 0;;
  let my_computed_fixed_point_test4 =
    computed_fixed_point (=) (fun x -> x / 34) 40 = 0;;

(******* PROBLEM 7 *******)

(*Given*)
(* An example grammar for a small subset of Awk.  *)

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let my_awksub_test0 =
  filter_reachable awksub_grammar = awksub_grammar

let my_awksub_test1 =
  filter_reachable (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules)

let my_awksub_test2 =
  filter_reachable (Lvalue, awksub_rules) = (Lvalue, awksub_rules)

let my_awksub_test3 =
  filter_reachable (Expr, List.tl (List.tl awksub_rules)) =
    (Expr,
     [Expr, [N Expr; N Binop; N Expr];
      Expr, [N Lvalue];
      Expr, [N Incrop; N Lvalue];
      Expr, [N Lvalue; N Incrop];
      Lvalue, [T "$"; N Expr];
      Incrop, [T "++"];
      Incrop, [T "--"];
      Binop, [T "+"];
      Binop, [T "-"]])

let my_awksub_test4 =
  filter_reachable (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    (Expr,
     [Expr, [N Lvalue];
      Expr, [N Incrop; N Lvalue];
      Expr, [N Lvalue; N Incrop];
      Lvalue, [T "$"; N Expr];
      Incrop, [T "++"];
      Incrop, [T "--"]])

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let giant_test0 =
  filter_reachable giant_grammar = giant_grammar

let giant_test1 =
  filter_reachable (Sentence, List.tl (snd giant_grammar)) =
    (Sentence,
     [Quiet, []; Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]])

let giant_test2 =
  filter_reachable (Quiet, snd giant_grammar) = (Quiet, [Quiet, []])

(*MINE*)
type test1_nonterminals = A | B | C | D
let test1_rules =
[A, [N B; N C];
A, [N C; N B];
A, [T"1"; T"1"];
B, [N C];
C, [N C];
D,[N A];
]
let test1_grammar = D, test1_rules

let my_filter_reachable_test3 = filter_reachable test1_grammar = test1_grammar
let my_filter_reachable_test4 = filter_reachable (B, test1_rules) =
(B,
[B, [N C];
C, [N C]])
let my_filter_reachable_test5 = filter_reachable (A, test1_rules) =
  (A,
  [A, [N B; N C];
  A, [N C; N B];
  A, [T"1"; T"1"];
  B, [N C];
  C, [N C]])
