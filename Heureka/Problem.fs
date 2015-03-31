module Heureka.Problem

type Node (state) =
    let mutable parent = None
    let mutable action = None
    let mutable path_cost = 0
    member n.State = state
    member n.Parent = parent
    member n.Action = action
    member n.Path_Cost = cost
let ChildNode problem node action =

type Problem (init,actions,trans_model,goal_test,path_costs) =
    member x.Initial_State() = init
    member x.Actions (node) = actions
    member x.Transition (node,action) = trans_model
    member x.Goal_Test (state) = goal_test
    member x.Path_Cost (