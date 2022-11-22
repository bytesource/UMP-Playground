namespace Ump

type Ump<'initArg, 'Model, 'Command, 'Event, 'Output> = {
    Init: 'initArg -> 'Model * 'Command list
    Update: 'Event -> 'Model -> 'Model * 'Command list
    Perform: 'Command -> Async<'Event>
    Output: 'Model -> 'Output
}


module Result = 
    let output result = 
        match result with
        | Ok _ -> Ok ()
        | Error s -> Error s

    let bindUpdate updatef event result = 
        match result with
        | Ok model -> updatef event model
        | Error err -> Error err, []


module Ump = 

    [<AutoOpen>]
    module Internal = 

        [<Struct>]
        // struct - per iteration: 1 stack allocation + 1 frame copy
        type ProgramState<'Model, 'Command, 'Event> = {
            Model: 'Model
            Commands: 'Command list
            Events: 'Event list
        }


        // Events are processed before Commands.
        // Events run sequentially.
        // Commands run in parallel.
        // In practice, Ump.update will return 
        // one Command at a time when it needs to run
        // them sequentially. 
        let rec runLoop ump pState = 
            match pState.Commands, pState.Events with
            | [], [] -> 
                async.Return (ump.Output pState.Model)

            // Process events:
            | _, event :: remainingEvents -> 
                let (newModel, nextCommands) = ump.Update event pState.Model

                let newState = {
                    Model = newModel
                    Commands = nextCommands |> List.append pState.Commands
                    Events = remainingEvents
                }

                runLoop ump newState

            // Process commands:
            | _, [] -> 
                async {
                    let commandsAsync = 
                        // Run commands
                        pState.Commands |> List.map ump.Perform

                    let! newEvents = commandsAsync |> Async.Parallel

                    let newState = {
                        Model = pState.Model
                        Commands = []
                        // NOTE [me]:
                        // Save new Events to DB in 'perform', then continue with loop
                        // Add time lag for events we need to wait for
                        // (e.g. downloading tracking) -- Emailer.fs for details.
                        // If time lag is in future, return zero new commands 
                        // (Event data already saved)
                        Events = newEvents |> Array.toList
                    }

                    return! runLoop ump newState
                }


    /// Runs a program using the provided initial argument.
    /// The returned Model is the final state of the Model when the program exited.
    /// Infinite loops are possible when Update generates Effects on every iteration.
    /// This allows the program to support interactive applications, for example.
    let run (ump: Ump<'initArg, 'Model, 'Command, 'Event, 'Output>) (initArg: 'initArg) = 
        let (model, commands) = ump.Init initArg

        let state = {
            Model = model
            // NOTE [me]:
            // We should probably start with our latest Events
            Events = []
            Commands = commands
        }

        runLoop ump state


    /// Creates a test function from the init and update functions.
    /// The returned function takes initArg and events and returns
    /// a model and command list that you can test against expected values.
    let createTest init update = 
        let test initArg events = 
            let init = init initArg
            // Ignore previous commands
            let update (model, _) event = 
                update event model

            events
            |> List.fold update init

        test