module Heureka.Search

open Heureka.Problem

let rec getSuccessors = function
    | _,_,[],output -> output
    | problem,node,action::t, output -> getSuccessors (problem,node,t,(ChildNode problem node action)::output)
     
let rec RBFS problem node f_limit =
    match problem.Goal_Test(node.state()) with
    | true -> Soln(node,f_limit)
    | false ->
        let successors = getSuccessors(problem,node,problem.Actions(node),List.empty)
        match successors.IsEmpty with
        |true -> Fail f_limit
        |false ->
            
let Recursive_BFS problem =
    RBFS (problem,Node(problem.Initial_State()),infinity)

