module Heureka.Search
type Node(state) =
    member n.state() = state
type ChildNode(problem,node,action) 

let rec getSuccessors = function
    | _,_,[],output -> output
    | problem,node,action::t, output -> getSuccessors problem node t (ChildNode(problem,node,action)::output)

let rec RBFS problem node f_limit =
    match problem.Goal_Test(node.state()) with
    | true -> return Solution(node)
    | false ->
        let successors = getSuccessors problem node
let Recursive_BFS = function
    | problem -> RBFS problem Node(problem.Initial_State()) infinity //

