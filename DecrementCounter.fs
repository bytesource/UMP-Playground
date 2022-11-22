module DecrementCounter

open System

open Ump

type Request = {
    CounterId: Guid
    Amount: int
}

let myRequest = {
    CounterId = Guid.NewGuid()
    Amount = 0
}

type Error = 
    | LoadFailed of message: string
    | CounterNotFound
    | CounterWouldGoNegative
    | SaveFailed of message: string


// UMP Start
// Command
type Command = // of <what I need to run the command>
    | LoadState of counterId: Guid
    | SaveState of counterId: Guid * count: int

// Event
type Event =  // of <the result I get back from a command>
    | StateLoaded of Result<int option, string>
    | StateSaved of Result<unit, string>

// Current State?
// Maybe I start with the order? type Model = { Order: OrderRecord }
type Model = { Request: Request }

// NOTE [Kasey]:
// Init really just gets things prepped and started for update. 
// It converts an initial argument into the model that will be used by update. 
// Usually I will do basic request validation in init if it is not done by some other part of the infrastructure.
// NOTE [me]: If validation fails, the model would be Error msg.
let init request = 
    Ok { Request = request }, [ LoadState request.CounterId ]

let update event model = 
    match event with // NOTE: I think I only need to pass model along (not wrapped in Result)
    | StateLoaded (Error s) -> // OrderFetched (Error s) -> pass (model, [fetchOrder])
        Error (LoadFailed s), []

    | StateLoaded (Ok None) -> 
        Error CounterNotFound, []

    | StateLoaded (Ok (Some oldCount)) -> // OrderFetched (Ok data) -> let next events = Next -> Next; (model, [ nextEvent, ... ])
        let count = oldCount - model.Request.Amount
        if count < 0 then 
            Error CounterWouldGoNegative, []
        else // Error in original code
            let newRequest = { model.Request with Amount = count }

            Ok { model with Request = newRequest }
            , [ SaveState (model.Request.CounterId, count) ]

    | StateSaved (Error s) -> 
        Error (SaveFailed s), []

    | StateSaved (Ok ()) -> 
        Ok model, []


// Perform
// ===============================
// Here begins the side-effect area of the code. 
// I also try to keep effects very focused to doing one single thing, 
// with all the config and data needed as parameters, 
// so that they can have the possibility of being reused.

type EffectConfig = {
    ExampleConfig: string
    // Other items such as:
    // -- Connection strings
    // -- Endpoint URLs
    // -- Loggers
}

// NOTE [me]:
// Big questions: How to handle commands that need to be bundled?
// We need perform to take a command list.
let perform config command = 
    match command with
    | LoadState counterId -> 
        // Simulate DB call
        printfn $"~~~~~~~~~~~~~~~~~Inside perform: StateLoaded"
        async {
            let exConfig = config.ExampleConfig
            let rand = new Random()
            do! Async.Sleep 30
            let count = rand.Next(0, 100)
            return StateLoaded (Ok (Some count))
        }

    | SaveState (counterId, count) -> 
        printfn $"~~~~~~~~~~~~~~~~~Inside perform: StateSaved"
        async {
            do! Async.Sleep 30
            return StateSaved (Ok ())
        }


// let bulkPerform config (commands: Command list) = 

// A couple of notes. 
// This simple implementation will not error. 
// But actual code might have try/catch, log exceptions, etc. 
// Anything you might normally need to do when you call a side effect.
module Query = 
    let counterState id = $"Select {id} from Model"

module Stmt = 
    let saveCounter id counter = $"Save {id} {counter}"

module Fx = 
    module Sql = 
        let readFirst config query msg =
            async {
                let c = config.ExampleConfig
                // do something
                return msg (Ok (Some 2))

            }

        let write config stmt msg = 
            async {
                let c = config.ExampleConfig
                // do something
                return msg (Ok ())
            }


let perform2 fxConfig effect =
    match effect with
    | LoadState counterId ->
        let query = Query.counterState counterId
        // Fx.Sql.readFirst<int> fxConfig query StateLoaded
        Fx.Sql.readFirst fxConfig query StateLoaded
    | SaveState (counterId, counter) ->
        let stmt = Stmt.saveCounter counterId counter
        Fx.Sql.write fxConfig stmt StateSaved


// Final pieces
// ==============================



let toUmp config = {
    Init = init
    Update = Result.bindUpdate update
    Perform = perform config
    Output = Result.output
}