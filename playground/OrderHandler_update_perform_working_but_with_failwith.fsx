
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


module Test = 

    type ServerError = ServerError of string
    type EventError = { Id: OrderId; Reason: string }

    type AppError = 
        | Server of ServerError
        | Event of EventError

    module Usage = 
        // Msg
        type MsgSingle = Result<OrderId, AppError>  // If there's one result, the error might be both, Server and Event related
        type MsgSingleAlt = Result<Result<OrderId, EventError>, ServerError>
        type MsgMulti = Result<Result<OrderId, EventError> list, ServerError>


type AppError = AppError of string
module AppError = 
    let explain (AppError e) = e

type OrderStatus = 
    | Processing
    | AwaitingShipping

module Data = 
    module Event = 
        type WarehouseOrderCreated = { Id: OrderId; WhId: WarehouseId }
        type TrackingFetched = { Id: OrderId; Number: string; Route: string }
        type StatusSet = { Id: OrderId; Status: OrderStatus }


// Name, Address, LineItems, Route
type SendWarehouseOrderEffect = { Id: OrderId; SWHData: string }


// ======================= Test

type EventError = { Id: OrderId; Reason: string }

[<RequireQualifiedAccess>]
type SuccessEvent = 
    // What data do I GET from the task to store in DB?
    | OrderSaved
    | WarehouseOrderCreated of Data.Event.WarehouseOrderCreated
    | TrackingNumberFetched of Data.Event.TrackingFetched

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


// ======================= Test


[<RequireQualifiedAccess>]
type Effect = 
    // What data do I need to PROVIDE for a single task?
    | FetchEvents
    | CreateWarehouseOrder of SendWarehouseOrderEffect
    | FetchTrackingNumber of OrderId
    //| MarkOrderStatus of OrderId * OrderStatus
    //| SaveEvents of Event.ToDb list


[<RequireQualifiedAccess>]
type Msg = 
    // What data do I GET from the finished task?
    | EventsFetched of Result<FromDbEvent list, AppError>
    // NOTE: 三泰createOrder不支持批量创建，调一次只能创建一个订单
    | WarehouseOrderCreated of Result<Result<Data.Event.WarehouseOrderCreated, EventError>, AppError>
    //| MarkedProcessing of OrderId
    // NOTE: Tracking number might not be available yet.
    | TrackingNumbersFetched of Result<Data.Event.TrackingFetched option list, AppError>
    //| MarkedAwaitingShipping of Result<unit, AppError>
    //| EventsSaved of Result<unit, Error>



type State = {
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


let getEffects (state: State) msg = 
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
                    Some (Effect.CreateWarehouseOrder { Id = o.OId; SWHData = "hello event" })

                | SuccessEvent.WarehouseOrderCreated data -> 
                    Some (Effect.FetchTrackingNumber data.Id)

                | SuccessEvent.TrackingNumberFetched _ -> None
            )
            |> List.choose id

        let newState = { state with Orders = orders }

        newState, effects

    | Msg.WarehouseOrderCreated (Ok result) -> 

        match result with 
        | Ok data -> state, [ Effect.FetchTrackingNumber data.Id ]

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


// Testing
// ===================

let order1 = { OId = OrderId "1"; Addr = "Street A"; PId = "P123" }
let order2 = { OId = OrderId "2"; Addr = "Street B"; PId = "S123" }
let order3 = { OId = OrderId "3"; Addr = "Street C"; PId = "S234" }

open Data.Event
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

let state = {
    Orders = Map.empty
    Processed = []
}


module Effect = 

    type Config = {
        FetchEvents: unit -> Async<Msg>
        CreateWarehouseOrder: SendWarehouseOrderEffect -> Async<Msg>
        FetchTrackingNumbers: OrderId list -> Async<Msg>
    }

    let config = {
        FetchEvents = (fun () -> async { return Msg.EventsFetched (Ok fromDbEvents)})
        CreateWarehouseOrder = (fun data -> async {
                    return Msg.WarehouseOrderCreated (Ok (Ok { Id = data.Id; WhId = WarehouseId "whid1"}))
                })
        FetchTrackingNumbers = (fun ids -> async {
            return Msg.TrackingNumbersFetched (Ok [ None; Some { Id = OrderId "1"; Number = "tracking"; Route = "route" }])
        })
    }

    [<RequireQualifiedAccess>]
    type Batch = 
        | FetchTrackingNumber

    // TODO: Try adding function types to Msg type definition
    let perform config (effects: Effect list) = 

        let categorize (single: Async<Msg> list, bulk: Map<Batch, Effect list>) effect = 
            match effect with 
            | Effect.FetchEvents -> 
                let e = config.FetchEvents ()
                (e :: single, bulk)
                
            | Effect.CreateWarehouseOrder data -> 
                let e = config.CreateWarehouseOrder data
                (e :: single, bulk)

            | Effect.FetchTrackingNumber id -> 
                let updatedBulk = bulk |> Map.addToList Batch.FetchTrackingNumber effect
                (single, updatedBulk)

        let singleEffectsAsync, bulkEffects = 
            effects
            |> List.fold categorize (List.empty, Map.empty)

        let catBulk (KeyValue(k ,(values: Effect list))) : Async<Msg> =
            match k with 
            | Batch.FetchTrackingNumber -> 
                let ids = values |> List.map (fun e -> 
                  match e with
                  | Effect.FetchTrackingNumber id -> id
                  | _ -> failwith $"Event: %A{e} cannot be part of 'FetchTrackingNumber' bulk action")
                config.FetchTrackingNumbers ids

        let bulkEffects = 
            bulkEffects
            |> Seq.map catBulk
            |> List.ofSeq


        bulkEffects @ singleEffectsAsync


[ Effect.FetchEvents ]
|> Effect.perform
|> Async.Parallel
|> Async.RunSynchronously
|> Array.iter (printfn "Msg: %A")



