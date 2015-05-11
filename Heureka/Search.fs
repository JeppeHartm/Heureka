module Heureka.Search
open Heureka.Problem



(*
 *Recursive Best First Search for the Heureka project for the introductory course in artificial intelligence
 *at the Technical University of Denmark. The algorithm is a functional adaptation of the RBFS algorithm found
 *in Artificial Intelligence - A Modern Approach by Russell and Norvig, 3rd edition.
 *
 *Note: a slight trivial change made to the algorithm is sorting the successor nodes after ascending cost estimate.
 *      Also, the algorithm has been changed to return the traversed nodes of the solution as a list instead of 
 *      only the node containing the goal state
 *)
let rec private getSuccessors = function
    | _,_,[],output -> output
    | problem,node,action::t, output -> getSuccessors (problem,node,t,(Problem.ChildNode problem node action)::output)
let private max: int*int->int = function
    | a,b when a > b -> a
    | a,b -> b
let rec private updateSuccessors:Problem<'a,'c>*Node<'a,'b>*List<Node<'a,'b>>*List<Node<'a,'b>> -> List<Node<'a,'b>> = function
    | _,_,[],output -> output
    | problem,node,s::rest,output -> 
        s.Set_f(max((problem.H(s.State)+s.g),node.f))
        updateSuccessors(problem,node,rest,s::output)
let rec private sortSuccessor : Node<_,_>*List<Node<_,_>>->List<Node<_,_>> = function
    | e,[] -> [e]
    | e,h::t when e.f < h.f -> e::h::t
    | e,h::t -> h::sortSuccessor(e,t)
let rec private sortSuccessors = function
    | [],output -> output
    | e::t,output -> sortSuccessors(t,sortSuccessor(e,output))
let private gt: Node<_,_>*Natural -> bool = function
    | _,Infinity -> false
    | a,Number x -> a.f > x
let private fmin = function
    | Number a, Number b -> Number (min a b)
    | Number a, _ -> Number a
    | _ , Number b -> Number b
    | _ -> Infinity
let rec private RBFS (problem:Problem<'a,'b>) (node:Node<'a,'b>) f_limit =
    let rec successorLoop = function
    | _,[], f_limit -> Fail f_limit
    | _,a::_, f_limit when gt(a,f_limit) -> Fail (Number (a.f)) 
    | node,a::t, f_limit ->
        let alt = 
            match t with
            | [] -> f_limit
            | a::tt -> Number (a.f)
        let res = RBFS problem a (fmin(f_limit,alt))
        match res with
        | Soln (l,f) -> Soln (node::l,f)
        | _ -> successorLoop(node,t,f_limit)
    match problem.Goal_Test (node.State) with
    | true -> Soln([node],f_limit)
    | false ->
        let successors = getSuccessors(problem,node,problem.Actions(node.State),List.empty)
        match successors.IsEmpty with
        |true -> Fail f_limit
        |false -> 
            let successors = updateSuccessors(problem,node,successors,List.empty)
            let sorted = sortSuccessors(successors,[])
            successorLoop(node,sorted,f_limit)
let Recursive_BFS (problem:Problem<'a,'b>) =
    RBFS problem (new Node<'a,'b>(problem.Initial_State)) Natural.Infinity


        
(*let Astar (problem:Problem<'a,'b>) =
    let node = (new Node<'a,'b>(problem.Initial_State))
    let frontier = (new System.Collections.Generic.KeyValuePair<int,'a>(node.f,node.State))::[]
    let explored = set []
    let rec Astar' (problem:Problem<'a,'b>) (explored:Set<'a>) (frontier:List<System.Collections.Generic.KeyValuePair<int,'a>>) =
        match List.sortBy (fun (kvp:System.Collections.Generic.KeyValuePair<int,'a>) -> kvp.Key) frontier with
        | [] -> Fail Infinity
        | kvp::newFrontier ->
            let st = new Node<'a,'b>(kvp.Value;
            if (problem.Goal_Test(st)) then (Soln(node,Infinity)) else
            let newExplored = Set.add (st) explored
            let flist = new System.Collections.Generic.List<System.Collections.Generic.KeyValuePair<int,'a>>();
            flist.AddRange(newFrontier)
            List.iter (fun a ->
                        let child = ChildNode problem node a
                        if not(List.exists (fun (kvp:System.Collections.Generic.KeyValuePair<int,'a>) -> kvp.Value = child.State) newFrontier ) && not(explored.Contains child.State) then
                            flist.Add(new System.Collections.Generic.KeyValuePair<int,'a>(child.f,child.State))
                        else if (List.exists (fun (kvp:System.Collections.Generic.KeyValuePair<int,'a>) -> kvp.Value = child.State) newFrontier ) && List.forall (fun (kvp:System.Collections.Generic.KeyValuePair<int,'a>) -> not(kvp.Value=child.State) || (kvp.Key> (child.f))  ) newFrontier then
                            flist.RemoveAt(flist.FindIndex(System.Predicate((fun (kvp:System.Collections.Generic.KeyValuePair<int,'a>) -> kvp.Value = child.State))))
                            flist.Add(new System.Collections.Generic.KeyValuePair<int,'a>(child.f,child.State))
                        ) (problem.Actions(node.State))
            Astar' problem newExplored (List.ofSeq flist)        
    Astar' problem explored frontier*)

    