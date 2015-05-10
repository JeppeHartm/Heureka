open System.IO
open System.Text

open Proplog
open Heureka.Problem

open Microsoft.FSharp.Text.Lexing
let parseAtom (str:string) =
    match str.[0] with
    | '-' -> NAtom(str.Substring 1)
    | _ -> Atom(str)
let fromHorn (strings:string[]) =
    match strings.Length with
    | x when x > 1 ->
        let first = strings.[0]
        let second = strings.[1]
        let head = parseAtom first
        let tailAsSet = Set.remove second (Set.remove first (Set.ofArray (strings))) 
        let tail = Set.map (fun x -> (negate (parseAtom(x)))) tailAsSet
        Disjunction(Set.add head tail)
    | x when x = 1 -> Disjunction(set [(parseAtom (strings.[0]))])
    | _ -> failwith "empty clause"
let parseLine (x:string) =
    let cons = x.Split(' ')
    fromHorn(cons)
let readSample file =
    let lines = File.ReadAllLines(file)
    List.ofArray (Array.map parseLine lines)
let createResolutionProblem = function
    | sentence, [] -> failwith "Trivial request; No knowledge base (TR01)"
    | sentence, knowledgebase ->
        let init = State(sentence,[])
        let actions x = //fun (State(s,l)) -> (List.map (fun x (State(y,al)) -> State(Resolve(x,y),x::al)) (List.concat [|l;knowledgebase|]))
            match x with
            |State(s,l) -> List.map (fun y -> Action(s,y)) (l@knowledgebase)
        let trans_model ((State(s,l)),y) = 
            match y with
            |Action(e1,e2) -> State(Resolve e1 e2,s::l)
        let goal_test = function
            |State(Disjunction(x),_) when x.IsEmpty -> true
            |_ -> false
        let step_cost = function
            |_,Action(_,Disjunction x) -> x.Count
        let heuristic : RState -> int = function
            |State(Disjunction x,_) -> x.Count
        new Problem<RState,RAction>(init,actions,trans_model,goal_test,step_cost,heuristic)

let PrettyPrint dis =
    let rec pprint = function
        |[] -> ""
        |(Atom s)::t -> sprintf "%s %s" s (pprint t)
        |(NAtom s)::t -> sprintf "-%s %s" s (pprint t)
    pprint (Set.toList dis)


[<EntryPoint>]
let main args = 
    let input = readSample "breakfast.prop"//List.map parseString (List.ofSeq(File.ReadLines("breakfast.prop")))
    //let input = List.map parseString (List.ofArray args)
    match input with
    | [] -> 0
    | sentence::kb ->
        let resolveProblem = createResolutionProblem(sentence,kb)
        let res = Heureka.Search.Recursive_BFS resolveProblem
        match res:Result<List<Node<RState,_>>> with
        |Soln (list,Number x) ->
            printf "Solution found with cost %i" x
            List.iter (fun (x:Node<RState,_>) ->
                    match x.State with
                    |State(Disjunction s,_) -> printf "%s\n" (PrettyPrint s)) list
        |_->printf "no solution found"
        0
