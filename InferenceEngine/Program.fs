open System.IO
open System.Text

open Proplog
open Heureka.Problem

open Microsoft.FSharp.Text.Lexing

let parseString (text:string) =
   let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
   try
       ProplogParser.Main ProplogLexer.tokenize lexbuf
   with e ->
        let pos = lexbuf.EndPos
        printfn "Error near line %d, character %d\n" pos.Line pos.Column
        failwith "parser termination (PT01)"
let inspectInput(s,kb) =
    match isClause(s),(List.forall isClause kb) with
    | true,true -> 0
    | _ -> failwith "Not CNF; Some expression is not a clause (NC01)"
let createResolutionProblem = function
    | sentence, [] -> failwith "Trivial request; No knowledge base (TR01)"
    | sentence, knowledgebase ->
        let init = sentence
        let actions = fun _ -> (List.map (fun x y -> Resolve(x,y)) knowledgebase)
        let trans_model = fun (x:expression,y:expression->expression) -> y x
        let goal_test = function
            |Empty -> true
            |_ -> false
        let step_cost = fun (a,b) -> 1
        let heuristic : expression -> int = function
            |exp -> Set.count (exp.GetLiterals)
        new Problem<expression,expression->expression>(init,actions,trans_model,goal_test,step_cost,heuristic)
[<EntryPoint>]
let main args = 
    let input = List.map parseString (List.ofArray args)
    match input with
    | [] -> 0
    | sentence::kb ->
        ignore (inspectInput(sentence,kb))
        let resolveProblem = createResolutionProblem(sentence,kb)
        let res = Heureka.Search.Recursive_BFS resolveProblem
        match res:Result<List<Node<expression,_>>> with
        |Soln (list,Number x) ->
            printf "Solution found with cost %i" x
            List.iter (fun (x:Node<expression,_>) -> printf "%s" (PrettyPrint (x.State))) list
        |_->printf "no solution found"
        0
