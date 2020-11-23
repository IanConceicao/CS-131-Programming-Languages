(*Homework 2 CS 131, Ian Conceicao*)

(*** Types ***)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal


(*** PROBLEM 1: Convert Grammar From Old to NEW ***)

(*Creates a new function that takes a nonterminal and returns the alternative list*)
let rec newRules oldRules nonterminal =
	match oldRules with
	| [] -> [] 
	| head::tail -> (*Pops off a grammar*)
		if (fst head) = nonterminal then (*Creates the logic of the function*)
			(snd head)::(newRules tail nonterminal)
		else 
			newRules tail nonterminal;;

let convert_grammar graml = 
	match graml with
	| (start, rules) ->
		(start, (newRules rules));; (*Curry to get a function*)

(***PROBLEM 2: List the leaves encountered in a parse tree**)

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list (* Node is a nonterminal and list of other parse_tree elemnts*)
  | Leaf of 'terminal	(* Leaf is just a terminal *)

let rec findLeaves nodes = 
	match nodes with 
	| [] -> []
	| head::tail ->
		match head with
			| Leaf leaf-> leaf::(findLeaves tail)
			| Node (anotherNode, subtree) -> (findLeaves subtree) @ (findLeaves tail);;

let parse_tree_leaves tree = findLeaves [tree];;

(*** Problem 3: Make Matcher ****)

(* Given a RHS of a rule it will try the rule on the frag*)
let rec send_nonterminals grammar altListList acceptor frag =
	match altListList with
	| [] -> None (* No Acceptable match is found between all of the lists, game over*)
	| head::tail -> (*Pops of part of an alternate list*)
		match check_frags grammar head acceptor frag with
		| None -> send_nonterminals grammar tail acceptor frag (*Try again with next part of same altlist*)
		| result -> result (*Return result of acceptor*)

and check_frags grammar altList acceptor frag =
	match altList with
	| [] -> acceptor frag (* Ran out of rules, if this returns must've found a grammar match! *)
	| _ -> (*Take the rule*)
		match frag with
		| [] -> None (* Trying to match a rule with an empty frag list, that won't work*)
		| head::tail -> (*Pop off frag*)
			match altList with
			(*If the rule is a nonterminal, you must account for that nonterminals set of rules, so call
				send_nonterminals again.  Use the acceptor to handle the rest of the terms by calling 
				check_frags again.*)
			| (N nonterminal)::rest -> (send_nonterminals grammar (grammar nonterminal)
						               (check_frags grammar rest acceptor) frag)
			| (T terminal)::rest -> 				
				if terminal = head then 
					(check_frags grammar rest acceptor tail) (* Rest of the rule list has to be match rest of terms in frag *)
				else None (*Must have taken the wrong route*);;
			
(*Returns a Matcher*)
let make_matcher gram = 
	match gram with 
	| (startSymbol, grammar) -> 
		fun accept frag -> send_nonterminals grammar (grammar startSymbol) accept frag;;

(** PROBLEM 4 **)

(* Ensures the matcher matches with the entire string *)
let empty_acceptor suffix = 
	match suffix with
	| [] -> Some []
	| _ -> None;;

(*Nearly identical as send_nonterminals, but it returns rule of grammar that precedes it*)
let rec new_send_nonterminals grammar altListList acceptor frag =
	match altListList with
	| [] -> None 
	| head::tail -> 
		match new_check_frags grammar head acceptor frag with
		| None -> new_send_nonterminals grammar tail acceptor frag
		| Some result -> Some (head::result) (* Return accepted rule ontop of what the acceptor gave *)

and new_check_frags grammar altList acceptor frag =
	match altList with
	| [] -> acceptor frag 
	| _ -> 
		match frag with
		| [] -> None 
		| head::tail -> 
			match altList with
			| (N nonterminal)::rest -> (new_send_nonterminals grammar (grammar nonterminal)
										(new_check_frags grammar rest acceptor) frag)
			| (T terminal)::rest -> 				
				if terminal = head then 
					(new_check_frags grammar rest acceptor tail)
				else None;;

(* Takes an altList like [N noun; T "word"] or any*)
(* Rules are the rules we followed in making the matcher *)
let rec next_rule altList rules  =
	match altList with
	| [] -> (rules, []) (* Base case *)
	| head::tail ->  (* Pop off a symbol, could be terminal or nonterminal *)
		match next_symbol head rules with
		| (rulesLeft, nodeCreated) ->  (* *)
			match next_rule tail rulesLeft with (* Horizontally continue *)
			| (thoseRules, nodesToFinish) -> (thoseRules, nodeCreated::nodesToFinish) 

and next_symbol symbol rules = 
	match symbol with 
	| (N nonterminal) -> (
		match rules with	
		| [] -> ([], Node (nonterminal, [])) (*Case of singleton list was parsed*)
		| head::tail -> 
			match next_rule head tail with (* Nonterminal, must follow to children nodes before returning*)
			| (rulesLeft,listOfChildren) -> (rulesLeft, Node (nonterminal, listOfChildren)))
	| (T terminal) -> (
		match rules with
		| [] -> ([], Leaf terminal) (* No more rules, we followed all of them *)
		| head::tail -> (rules, Leaf terminal));; (* Don't pop off a rule for lhs because terminal rules 
															are not in list of rules *)
let make_parser gram = 
	match gram with 
	| (startSymbol, grammar) ->
		fun frag -> 
			match new_send_nonterminals grammar (grammar startSymbol) empty_acceptor frag with
			| None | Some [] -> None
			| Some result -> (* List of rules used in making the matcher *)
				match next_rule [N startSymbol] result with
					| (_,right) -> (* We don't care about the rules portion, it should be empty anwyways *)
						match right with
						| [] -> None
						| head::tail -> Some head;;






