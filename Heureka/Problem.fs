module Heureka.Problem

type Node<'a,'b>(state:'a,parent:option<Node<'a,'b>>,action:'b,cost:int) =
    let mutable _cost:int = 0
    do 
        _cost<-cost
    member x.State = state
    member x.Parent = parent
    member x.Action = action
    member x.Path_Cost = _cost
    member x.Set_Cost c = _cost<-c

type Problem<'a,'b> (init:'a,actions:'a -> List<'b>,trans_model:'a*'b -> 'a,goal_test:'a->bool,step_cost:'a*'b -> int) =
    member x.Initial_State = init
    member x.Actions = actions
    member x.Result = trans_model
    member x.Goal_Test = goal_test
    member x.Step_Cost = step_cost

let ChildNode (problem:Problem<'a,'b>) (node:Node<'a,'b>) (action:'b) = 
    new Node<'a,'b>(problem.Result(node.State,action),Some(node),action,node.Path_Cost + problem.Step_Cost(node.State,action))