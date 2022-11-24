
// Kasey: Msgs are not necessarily events. 
// They are to notify the workflow what happened with a side effect. 

// Side effects are not necessarily something that advances the workflow. 
// It could just be reading from a file, verifying something
// with an integration before making changes, etc.

// UMP steps are NOT decision steps. 
// They are side effect (often IO) steps. 
// -- The workflow might need multiple side effects
//     before the next decision can be made. OR
// -- It might be able to complete several decision steps 
//    before the next side effect is needed.

// From DB: Events all succeeded

// Handle events...


// To DB: Events might have succeeded or failed.
// -- Failure event needs the following info: Error message or record, Order ID

type OrderId = OrderId of string
type WarehouseId = WarehouseId of string
type Order = { OId: OrderId; Addr: string; PId: string }


type AppError = AppError of string
module AppError = 
    let explain (AppError e) = e

type OrderStatus = 
    | Processing
    | AwaitingShipping

module Data = 
    type WarehouseOrderCreated = { Id: OrderId; WhId: WarehouseId }
    type TrackingFetched = { Id: OrderId; Number: string; Route: string }
    type StatusSet = { Id: OrderId; Status: OrderStatus }

    // Name, Address, LineItems, Route
    type SendWarehouseOrder = { Id: OrderId; SWHData: string }


[<RequireQualifiedAccess>]
type Batch = 
    | FetchTrackingNumber


// ======================= Test

type EventError = { Id: OrderId; Reason: string }

[<RequireQualifiedAccess>]
type SuccessEvent = 
    // What data do I GET from the task to store in DB?
    | OrderSaved
    | WarehouseOrderCreated of Data.WarehouseOrderCreated
    | TrackingNumberFetched of Data.TrackingFetched

[<RequireQualifiedAccess>]
type FailureEvent = 
    // What data do I GET from the task to store in DB?
    | WarehouseOrderCreated of EventError
    | TrackingNumberFetched of EventError

[<RequireQualifiedAccess>]
type Event = 
    | Success of SuccessEvent
    | Failure of FailureEvent

type FromDbEvent = { Order: Order; FromDb: SuccessEvent }

// TODO: Think about shortening type names like Data.Event.WarehouseOrderCreated
// TODO: Get back to Sanofan repo, store first order and event using JSON.
// ======================= Test

type GroupedEffect = 
    | FetchTrackingNumber of OrderId


type SingleEffect = 
    // What data do I need to PROVIDE for a single task?
    | FetchEvents
    | CreateWarehouseOrder of Data.SendWarehouseOrder
    //| MarkOrderStatus of OrderId * OrderStatus
    //| SaveEvents of Event.ToDb list

[<RequireQualifiedAccess>]
type Effect = 
    | Single of SingleEffect
    | Grouped of GroupedEffect


[<RequireQualifiedAccess>]
type Msg = 
    // What data do I GET from the finished task?
    | EventsFetched of Result<FromDbEvent list, AppError>
    // NOTE: 三泰createOrder不支持批量创建，调一次只能创建一个订单
    | WarehouseOrderCreated of Result<Result<Data.WarehouseOrderCreated, EventError>, AppError>
    //| MarkedProcessing of OrderId
    // NOTE: Tracking number might not be available yet.
    | TrackingNumbersFetched of Result<Data.TrackingFetched option list, AppError>
    //| MarkedAwaitingShipping of Result<unit, AppError>
    //| EventsSaved of Result<unit, Error>



type Model = {
    Orders: Map<OrderId, Order>
    Processed: Event list
}

module Result = 

    let toBool res = 
        match res with
        | Ok _ -> true
        | Error _ -> false


    let separateValues resList =
        let separator (pass, fail) res = 
            match res with
            | Ok value -> (value :: pass, fail)
            | Error err -> (pass, err :: fail)

        resList |> List.fold separator ([], [])


module List = 

    let separateBy pred l = 
        let separator (pass, fail) e =
            if pred e 
            then (e :: pass, fail)
            else (pass, e :: fail)

        l |> List.fold separator ([], [])


module Map = 

    let addToList key v (table: Map<'k, 'v list>) = 
        match table |> Map.tryFind key with
        | Some list -> table |> Map.add key (v :: list)
        | None -> table |> Map.add key [ v ]


let update msg (state: Model) = 
    match msg with
    | Msg.EventsFetched (Ok events) -> 
        // Fill orders
        // For each event, add effect to effect list

        let orders = 
            events
            |> List.map (fun { Order = o } -> o.OId, o)
            |> Map.ofList

        let effects = 
            events
            |> List.map (fun { Order = o; FromDb = e } -> 
                match e with
                | SuccessEvent.OrderSaved -> 
                    Some (CreateWarehouseOrder { Id = o.OId; SWHData = "hello event" } |> Effect.Single)

                | SuccessEvent.WarehouseOrderCreated data -> 
                    Some (FetchTrackingNumber data.Id |> Effect.Grouped)

                | SuccessEvent.TrackingNumberFetched _ -> None
            )
            |> List.choose id

        let newState = { state with Orders = orders }

        newState, effects

    | Msg.WarehouseOrderCreated (Ok result) -> 

        match result with 
        | Ok data -> state, [ FetchTrackingNumber data.Id |> Effect.Grouped ]

        | Error err -> 
            let failureEvent = 
                err
                |> FailureEvent.WarehouseOrderCreated
                |> Event.Failure

            { state with Processed = failureEvent :: state.Processed }, []


    | Msg.TrackingNumbersFetched (Ok results) -> 
        let successes = results |> List.choose id

        // As this is the final Msg, also store successful events.
        let events = 
            successes
            |> List.map (fun data -> SuccessEvent.TrackingNumberFetched data |> Event.Success)

        // No new effects are generated, so we also store all successful events at this point so that 
        // they can be stored to the DB.
        let newState = { state with Processed = events @ state.Processed }

        newState, []
     
    | Msg.EventsFetched (Error appError)
    | Msg.WarehouseOrderCreated (Error appError)
    | Msg.TrackingNumbersFetched (Error appError) -> 
        // Log error
        printfn "An error occured: %s" (appError |> AppError.explain)

        state, []




[<RequireQualifiedAccess>]
module Effect = 

    type Config = {
        FetchEvents: unit -> Async<Msg>
        CreateWarehouseOrder: Data.SendWarehouseOrder -> Async<Msg>
        FetchTrackingNumbers: OrderId list -> Async<Msg>
    }



// Build final UMP
    let perform config (effects: Effect list) = 

        let categorize (single: Async<Msg> list, bulk: Map<Batch, GroupedEffect list>) effect = 
            match effect with 
            | Effect.Single e -> 
                match e with
                | FetchEvents -> 
                    let e = config.FetchEvents ()
                    (e :: single, bulk)
                    
                | CreateWarehouseOrder data -> 
                    let e = config.CreateWarehouseOrder data
                    (e :: single, bulk)

            | Effect.Grouped e -> 
                match e with
                | FetchTrackingNumber _ -> 
                    let updatedBulk = bulk |> Map.addToList Batch.FetchTrackingNumber e
                    (single, updatedBulk)

        let singleEffectsAsync, bulkEffects = 
            effects
            |> List.fold categorize (List.empty, Map.empty)

        let catBulk (KeyValue(k ,(values: GroupedEffect list))) : Async<Msg> =
            match k with 
            | Batch.FetchTrackingNumber -> 
                let ids = values |> List.map (fun e -> 
                  match e with
                  | FetchTrackingNumber id -> id)
                printfn "Orders Ids used to fetch tracking numbers: %A" ids

                config.FetchTrackingNumbers ids

        let bulkEffects = 
            bulkEffects
            |> Seq.map catBulk
            |> List.ofSeq


        bulkEffects @ singleEffectsAsync



type Ump<'initConfig, 'Model, 'Effect, 'Msg, 'Output> = {
    Init: 'initConfig -> 'Model * 'Effect list
    Update: 'Msg -> 'Model -> 'Model * 'Effect list
    Perform: 'Effect list -> Async<'Msg> list
    Output: 'Model -> 'Output
}

module Ump = 

    [<AutoOpen>]
    module Internal = 

        [<Struct>]
        // Struct: 1 stack allocation per iteration
        type ProgramState<'Model, 'Effect, 'Msg> = {
            Model: 'Model 
            Effects: 'Effect list
            Msg: 'Msg list
        }

        // Events are processed before Commands.
        // Events run sequentially.
        // Commands run in parallel.
        // In practice, Ump.update will return 
        // one Command at a time when it needs to run
        // them sequentially. 
        let rec runLoop ump pState = 
            match pState.Effects, pState.Msg with
            | [], [] -> 
                async.Return (ump.Output pState.Model)

            // Process messages:
            | _, msg :: msgTail -> 
                let (newModel, nextEffects) = ump.Update msg pState.Model

                let newPState = {
                    Model = newModel
                    Effects = nextEffects
                    Msg = msgTail
                }

                runLoop ump newPState

            // Process effects: 
            | effects, _ -> 
                async {
                    let! newMessages = 
                        effects 
                        |> ump.Perform
                        |> Async.Parallel

                    let newPState = {
                        pState with 
                            Effects = []
                            Msg = newMessages |> List.ofArray
                    }
                     
                    return! runLoop ump newPState
                }


    /// Runs a program using the provided initial argument.
    /// The returned Model is the final state of the Model when the program exited.
    /// Infinite loops are possible when Update generates Effects on every iteration.
    /// This allows the program to support interactive applications, for example.
    /// NOTE: unit for now is a placeholder for a future config record. 
    let run (ump: Ump<'initConfig, 'Model, 'Effect, 'Msg, 'Output>) (initConfig: 'initConfig) = 
        let (model, effects) = ump.Init initConfig

        let pState = {
            Model = model
            Msg = []
            Effects = effects
        }

        runLoop ump pState


// Testing
// ===================
module Testing = 

    let order1 = { OId = OrderId "1"; Addr = "Street A"; PId = "P123" }
    let order2 = { OId = OrderId "2"; Addr = "Street B"; PId = "S123" }
    let order3 = { OId = OrderId "3"; Addr = "Street C"; PId = "S234" }

    open Data
    let whOrder1 = { Id = OrderId "1"; WhId = WarehouseId "Wh1"}
    let whOrder2 = { Id = OrderId "2"; WhId = WarehouseId "Wh2"}
    let tracking1 = { Id = OrderId "3"; Number = "Track1234"; Route = "USPS" }
    // let whOrder : Data.Event.WarehouseOrderCreated = { Id = OrderId "1"; WhId = WarehouseId"Wh1"}


    let fromDbEvents : FromDbEvent list= 
        [
            { Order = order1; FromDb = SuccessEvent.WarehouseOrderCreated whOrder1 } 
            { Order = order2; FromDb = SuccessEvent.WarehouseOrderCreated whOrder2 } 
            { Order = order3; FromDb = SuccessEvent.TrackingNumberFetched tracking1 }
        ]

    let effectConfig : Effect.Config = {
        FetchEvents = (fun () -> 
            async { return Msg.EventsFetched (Ok fromDbEvents)})

        CreateWarehouseOrder = (fun data -> 
            async {
                return Msg.WarehouseOrderCreated (Ok (Ok { Id = data.Id; WhId = WarehouseId "whid1"}))
            })

        FetchTrackingNumbers = (fun ids -> 
            async {
                let msg = 
                    ids 
                    |> List.map (fun id -> Some { Id = id; Number = "tracking"; Route = "route"})
                    |> Ok
                    |> Msg.TrackingNumbersFetched

                return msg
            })
    }

    let model = {
        Orders = Map.empty
        Processed = []
    }

    let initDummyConfig = "Dummy Config"

    let init initConfig = model, [ FetchEvents |> Effect.Single ]

module Main = 
    let toUmp config = {
        Init = Testing.init
        Update = update
        Perform = Effect.perform config
        Output = id
    }

let ump = 
    Main.toUmp Testing.effectConfig

Ump.run ump Testing.initDummyConfig
|> Async.RunSynchronously
|> fun m -> printfn "Model: %A" m








