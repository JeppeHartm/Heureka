// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace Navigator
open Heureka.Search
open Heureka.Problem
open System.IO
open Problem
module Driver =
    let parseLine (line:string) =
        let content = line.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        let name = content.[2]
        let p1 = Point(System.Convert.ToInt32(content.[0]),System.Convert.ToInt32(content.[1]))
        let p2 = Point(System.Convert.ToInt32(content.[3]),System.Convert.ToInt32(content.[4]))
        Road(p1,name,p2)
    let readSample file =
        let lines = File.ReadAllLines(file)
        List.ofArray (Array.map parseLine lines)
    [<EntryPoint>]
    let main argv = 
        let generateMapData = readSample "copenhagentest.navi"
        let problem = Problem.navigatorProblem generateMapData (Point(10,70)) (Point(80,70))
        let res = Heureka.Search.Recursive_BFS problem
        let getList:Result<List<Node<_,_>>>->List<Node<_,_>> = function
            | Soln (l,_) -> l
            | Fail _ -> []
        let rec getOutput:List<Node<_,_>>->string = function
            | [] -> "end"
            | h::t -> sprintf "(%s) -> %s" (h.State.ToString()) (getOutput t)

        printf "%O" (getOutput (getList res))
        ignore (System.Console.Read());
        0 // return an integer exit code
