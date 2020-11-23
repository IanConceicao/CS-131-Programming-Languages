(*this file contains solutions to Homework 1*)
(*All solutions written by: Ian Conceicao 505153981*)

(******* PROBLEM 1 *******)

(*returns true if element a is in list b*)
let contains a b =
List.exists (fun c -> c=a) b;;

let rec subset a b =
match a with
| [] -> true
| left::right -> if not (contains left b) then false
else subset right b;;

(******* PROBLEM 2 *******)

let equal_sets a b =
(subset a b) && (subset b a);;

(******* PROBLEM 3 *******)

let rec set_union a b =
match a with
| [] -> b
| left::right -> if contains left b then set_union right b
else set_union right (left::b);;

(******* PROBLEM 4 *******)

let rec set_intersection_r a b c =
match a with
| [] -> c
| left::right -> if contains left b
then set_intersection_r right b (left::c)
else set_intersection_r right b c;;

let set_intersection a b =
set_intersection_r a b [];;

(******* PROBLEM 5 *******)
let rec set_diff_r a b c =
match a with
| [] -> c
| left::right -> if contains left b
then set_diff_r right b c
else set_diff_r right b (left::c);;

let set_diff a b =
set_diff_r a b [];;

(******* PROBLEM 6 *******)

let rec computed_fixed_point eq f x =
if eq (f x) x
then x
else computed_fixed_point eq f (f x);;

(**** PROBLEM 7 ****)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* Get different sides of a rule *)
let rule_left (a, _) = a;;
let rule_right (_, b) = b;;

(*Get All non terminal Symbols given a RHS of a rule*)
let rec listOfChildren rhs =
match rhs with
| [] -> []
| N head::tail -> head::(listOfChildren tail)
| T head::tail -> listOfChildren tail;;

(*Given a ruleset and a list of what is currentlyReachable,
* this function returns a updated list of  reachable symbols
*)
let rec get_current_reachable rules currentReachables =
match rules with
| [] -> currentReachables
| head::tail -> (*pop rule off*)

let currentSymbol = rule_left head in

(*If symbol on left is reachable, add symbols on right as reachable*)
if contains currentSymbol currentReachables then
  let newSymbols = listOfChildren (rule_right head) in
  get_current_reachable tail (set_union currentReachables newSymbols)
else
  get_current_reachable tail currentReachables;;

(*Returns ALL reachable symbols (nonterminal)*)
(*Runs the function above, but for each find!*)
let rec all_reachable rules currentReachables =
  let updated = get_current_reachable rules currentReachables in

  if equal_sets updated currentReachables
  then currentReachables
  else all_reachable rules updated;;

(*Remove unreachables*)
let remove_unreachables rules start =
  let allReachables = all_reachable rules [start] in
  List.filter (fun a -> contains (rule_left a) allReachables) rules;;

  let filter_reachable g =
  let start = rule_left g in
  let rules = rule_right g in
  (start, remove_unreachables rules start);;
