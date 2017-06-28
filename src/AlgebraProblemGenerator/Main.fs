module Main

[<EntryPoint>]
let main argv = 
    Parser.tests
    printfn "%A" argv
    0 // return an integer exit code

