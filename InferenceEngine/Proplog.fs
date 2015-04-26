module Proplog

type expression =
    |Conjunction of expression * expression
    |Disjunction of expression * expression
    |Implication of expression * expression
    |Biimplication of expression * expression
    |Negation of expression
    |Literal of string

let negate = function
    |Negation x -> x
    |x -> Negation x

let rec isClause = function
    | Disjunction (a,b) -> (isClause a) && (isClause b)
    | Negation (Literal a) -> true
    | Literal a -> true
    | _ -> false

let rec isCNF = function
    | Conjunction (a,b) ->
        let isCNFsub e =
            match e with
            | Conjunction _ -> isCNF e
            | _ -> isClause e
        (isCNFsub a) && (isCNFsub b)
    | Literal _ -> true
    | Negation (Literal _) -> true
    | _ -> false

let PL_Resolve set1 set2 =
    let res = Set.union set1 set2
    let rec PL_Resolve' input output =
        match input with
        | [] -> output
        | h::t ->
            match List.exists (fun e -> h = negate e) output with
            | false -> PL_Resolve' t (h::output)
            | true -> PL_Resolve' t (List.filter (fun e -> not (h = negate e)) output)
    Set.ofList (PL_Resolve' (Set.toList res) List.empty)

let rec PrettyPrint = function
    | Conjunction (a,b) -> sprintf "(%O) ^ (%O)" (PrettyPrint a) (PrettyPrint b)
    | Disjunction (a,b) -> sprintf "(%O) v (%O)" (PrettyPrint a) (PrettyPrint b)
    | Implication (a,b) -> sprintf "(%O) -> (%O)" (PrettyPrint a) (PrettyPrint b)
    | Biimplication (a,b) -> sprintf "(%O) <-> (%O)" (PrettyPrint a) (PrettyPrint b)
    | Negation a -> sprintf "-(%O)" (PrettyPrint a)
    | Literal s -> sprintf "%s" s