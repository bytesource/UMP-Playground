open Ump

let program = 
    DecrementCounter.toUmp { ExampleConfig = "test" }

DecrementCounter.myRequest
|> Ump.run program
|> Async.RunSynchronously
|> fun output -> printfn "%A" output
