open System.IO
open System.Text

open Proplog
open ProplogLexer
open ProplogParser

let parseString (text:string) =
   let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
   try
       ProplogParser.Main ProplogLexer.tokenize lexbuf
   with e ->
        let pos = lexbuf.EndPos
        printfn "Error near line %d, character %d\n" pos.Line pos.Column
        failwith "parser termination"

let _ = 
    printfn "Write a sentence in Propositional logic using the operators '-', 'v', '^', '<-', '->', '<->' and parentheses: "
    let e = parseString " a ^ b <-> -c -> d"
    //System.Console.ReadLine()
    printfn "%O" (PrettyPrint e)
    printfn "is clause: %O" (isClause e)
    printfn "is cnf: %O" (isCNF e)
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
