module Proplog
type Clause = Disjunction of Set<Lit>
and Lit =
    |Atom of string
    |NAtom of string

type RState = State of Clause * List<Clause>
type RAction = Action of Clause * Clause           

let complement = function
    |Atom s1, NAtom s2 -> s1=s2
    |NAtom s1, Atom s2 -> s1=s2
    |_ -> false
let negate = function
    |Atom s -> NAtom s
    |NAtom s -> Atom s
let negated = function
    |Atom _ -> false
    |NAtom _ -> true
let hasComplement e set =
    Set.exists (fun x -> complement(e,x)) set
let Resolve s1 s2 =
    match s1,s2 with
    |Disjunction(s1),Disjunction(s2) ->
        let union = Set.union s1 s2
        let resolvent = Set.filter (fun x -> not (hasComplement x union)) union
        Disjunction(resolvent)