namespace MathInterpreter

open System
open MathInterpreter.Lexer
open MathInterpreter.Parser

module Program =

    [<EntryPoint>]
    let main argv =
        printfn "Math Interpreter (type 'exit' to quit)"
        let rec loop () =
            printf ">>> "
            let input = Console.ReadLine()
            if input.ToLower() = "exit" then
                ()
            else
                match Lexer.lex input with
                | Ok tokens ->
                    try
                        let result, parseTree, _ = parseStatement tokens
                        printfn "Result: %f" result
                        printfn "Parse Tree:\n%s" (visualizeParseTree parseTree "")
                    with
                    | ParserException msg ->
                        printfn "Parser Error: %s" msg
                    | ex ->
                        printfn "Error: %s" ex.Message
                | Error errMsg ->
                    printfn "Lexer Error: %s" errMsg
                loop ()
        loop ()
        0
