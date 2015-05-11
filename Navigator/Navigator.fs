namespace Navigator
open Heureka.Problem
open System
open System.IO
module Problem =
    
    type Road = Road of Coordinate * string * Coordinate
    and Coordinate = Point of int * int
        with override x.ToString() = 
                match x with
                |Point(a,b) -> sprintf "(%i,%i)" a b
    let private is a b = 
        match a,b with
        |Point (a,b), Point (c,d) when a = c && b = d -> true
        |_ -> false
    let private chofun x = function
        |Road (a,b,c) when a = x -> Some(Road(a,b,c))
        |_ -> None

    let navigatorProblem (data:List<Road>) (a:Coordinate) (b:Coordinate) = 
        let init = a
        let goal_test = is b
        let actions data = function
            |a -> List.choose (chofun a) data
        let trans_model = function
            |_,Road(_,_,c) -> c
        let dist (Point(x1,y1)) (Point(x2,y2)) =
            let xdif = Math.Abs(x2-x1)
            let ydif = Math.Abs(y2-y1)
            (int)(Math.Sqrt((float)(xdif*xdif+ydif*ydif)))
        let step_cost = function
            |a,Road(_,_,b) -> dist a b
        new Problem<Coordinate,Road>(init,actions data,trans_model,goal_test,step_cost,dist b)