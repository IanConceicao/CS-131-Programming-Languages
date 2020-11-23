
let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type english_nonterminals = 
    Sentence | Noun | Verb | Adjective | Adverb | Article | Exclamation

let english_grammar = 
    (Sentence, function
        | Sentence -> [[N Article; N Noun; N Verb; N Exclamation]]
        | Noun -> [ [N Adjective; T"boy"];
                    [N Adjective; T"girl"];
                    [T"boy"];
                    [T"girl"]]
        | Verb -> [[T"ran"; N Adverb];
                    [T"fell"; N Adverb];
                    [T"ran"];
                    [T"fell"]]
        | Adjective -> [[T"small"];
                        [T"fat"]]
        | Adverb -> [[T"quickly"];
                    [T"suddenly"]]
        | Article -> [[T"The"]]
        | Exclamation -> [[T"."];
                         [T"!"]])


let make_matcher_test = ((make_matcher english_grammar accept_all 
                            ["The";"small";"boy";"ran";"quickly";"!"]) = Some [])
let make_matcher_test2 = ((make_matcher english_grammar accept_all 
                            ["The";"girl";"ran";"."]) = Some [])
let make_matcher_test3 = ((make_matcher english_grammar accept_all 
                            ["The";"girl";"ran";".";"+"]) = Some ["+"])

let frag = ["The";"small";"boy";"ran";"quickly";"!"]
let make_parser_test = 
    match make_parser english_grammar frag with
    | Some tree -> parse_tree_leaves tree = frag
    | _ -> false




