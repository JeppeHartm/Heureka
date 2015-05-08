// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace Navigator
open Heureka.Search
open Heureka.Problem
open Problem
module Driver =
    [<EntryPoint>]
    let main argv = 
        let generateMapData = [
            Road(Point(0,0),"1",Point(100,0));
            Road(Point(100,0),"2",Point(100,100));
            Road(Point(0,0),"3",Point(25,25));
            Road(Point(25,25),"4",Point(75,75));
            Road(Point(75,75),"5",Point(100,100))]
        let problem = Problem.navigatorProblem generateMapData (Point(0,0)) (Point(100,100))
        let res = Heureka.Search.Recursive_BFS problem
        let getList = function
            | Soln (l,_) -> l
            | Fail _ -> []
        let rec getOutput = function
            | [] -> "end"
            | h::t -> sprintf "(%s) -> %s" (h.State) (getOutput t)

        printf "%O" (getOutput (getList res))
        ignore (System.Console.Read());
        0 // return an integer exit code
