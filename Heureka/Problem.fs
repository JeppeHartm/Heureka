module Heureka.Problem
(*
 *The implementation of Node<'a,'b> where 'a denotes a state type and 'b an action type.
 *It takes the parent and action arguments as options since the initial node doesn't have either.
 *
 *)
type Node<'a,'b>(state:'a,parent:option<Node<'a,'b>>,action:option<'b>,cost:int) =
    let mutable _path_cost:int = 0
    let mutable _cost_est:int = 0
    do 
        _path_cost<-cost
        _cost_est<-cost
    member x.State = state
    member x.Parent = parent
    member x.Action = action
    member x.Path_Cost = _path_cost
    member x.Set_Path_Cost c = _path_cost<-c
    member x.Cost_Est = _cost_est
    member x.Set_Cost_Estimate c = _cost_est<-c
    new (state) = Node<'a,'b>(state,None,None,0)

type Problem<'a,'b> (init:'a,actions:'a -> List<'b>,trans_model:'a*'b -> 'a,goal_test:'a->bool,step_cost:'a*'b -> int,heuristic:'a -> int) =
    member x.Initial_State = init
    member x.Actions = actions
    member x.Result = trans_model
    member x.Goal_Test = goal_test
    member x.Step_Cost = step_cost
    member x.H = heuristic

type Result<'a> =
    | Soln of 'a * Natural
    | Fail of Natural
and Natural =
    | Number of int
    | Infinity

let ChildNode (problem:Problem<'a,'b>) (node:Node<'a,'b>) (action:'b) = 
    new Node<'a,'b>(problem.Result(node.State,action),Some node,Some action,node.Path_Cost + problem.Step_Cost(node.State,action))