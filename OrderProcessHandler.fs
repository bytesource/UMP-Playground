module OrderProcessHandler

type OrderId = { OrderId: string }
type PackageId = { PackageId: string }
type Order = { OrderId: string; LineItems: string list; PaymentId: string }

type ValidatedWarehouseOrder = { OrderId: OrderId }


type FromWarehouse = {
    OrderId: OrderId
    PackageId: PackageId
}

module Data = 
    module Wh = 
        type SendOrder = { OrderId: OrderId; Address: string; LineItems: string list }

        type GetOrder = {
            OrderId: OrderId
            PackageId: PackageId
        }

        type Tracking = { OrderId: OrderId; Number: string; Route: string}

type Error = Error of string

type PrimKey = {
    Id: OrderId
    Timestamp: int64
}

// IDEA: Only load successfull Events from DB


type OkEventData = 
    // What do I get back
    | OrderFetched                 of Order
    | WarehouseOrderValidated      of ValidatedWarehouseOrder
    | WarehouseOrderSent           of Data.Wh.GetOrder
    | TrackingNumberFetched        of Data.Wh.Tracking
    | OrderUpdatedProcessing       
    | OrderUpdatedAwaitingShipping 
    | OrderUpdatedCompleted        
    | PayPalTrackingSent        
    | UspsTrackingSent        

type ErrorEventData<'error> = 
    // What do I get back
    | OrderFetched                 of 'error
    | WarehouseOrderValidated      of 'error
    | WarehouseOrderSent           of 'error
    | TrackingNumberFetched        of 'error
    | OrderUpdatedProcessing       of 'error
    | OrderUpdatedAwaitingShipping of 'error
    | OrderUpdatedCompleted        of 'error
    | PayPalTrackingSent           of 'error
    | UspsTrackingSent             of 'error
// type Event = Event of Common * EventType 

type OkEvent = PrimKey * OkEventData
type ErrorEvent = PrimKey * ErrorEventData<Error>

type Event = Result<string, Error>

type SuccessEvent = OkEvent of Event
type FailureEvent = OkEvent of Event

[<RequireQualifiedAccess>]
module Event = 

    let separate (events: Event list) = 


    // let getTimestamp (Event (common, _)) = common.Timestamp
    let getTimestamp (key, _)= key.Timestamp


    let getOrderId (key, _) = key.OrderId


    let mostRecent events = 
        events
        // The higher the Unix timestamp, the more recent the date.
        |> List.maxBy getTimestamp

    
    let tryGetOrder orderId okEvents = 
        let orderOfEvent event = 
            match event with
            // Sucessful OrderFetched event with matching order ID
            | (id, _), OrderFetched order when id = orderId -> order
            
            // This should never happen
            | _ -> failwith $"No order found for order ID #{orderId}"

        okEvents 
        |> List.pick orderOfEvent


// TODO: Code first Msg->Effect->Msg step in UMP.
// TODO: Think about what data the FIRST Effect needs.
// TODO: Integrate the FIRST Event with the database (table schema, JSON in and out)

[<RequireQualifiedAccess>]
module Events =

    let byOrder events = 
        events
        |> List.groupBy Event.getOrderId


    let mostRecentByOrder (events: Event list) = 
        events
        |> byOrder
        |> List.map (fun (_, events) -> events |> Event.mostRecent)


type MsgError = { Ids: Set<OrderId>; Reason: string }


[<RequireQualifiedAccess>]
type Msg = 
    | EventsFetched of Result<Event list, MsgError> // Loaded from DB
    | WarehouseOrdersValidated of Result<Warehouse list, MsgError>
    | WarehouseOrdersSent of Result<unit, MsgError>
    | TrackingNumbersFetched of Result<TrackingInfo list, MsgError>
    | OrderStatusChanged of Result<OrderStatus, MsgError>
    | PayPalTrackingSent of Result<Result<TrackingInfo, string> list, MsgError>

[<RequireQualifiedAccess>]
type Cmd = 
    | FetchLatestEvents
    | SetStatusProcessing of OrderId list
    // Could include Async effect, e.g., via third-party address validation.
    | ValidateWarehouseOrders of Order List
    | SendWarehouseOrders of Warehouse list
    | SetStatusAwaitingShipping of OrderId list
    | FetchTrackingNumbers of OrderId list
    | SetStatusCompleted of OrderId list
    | SendTrackingPayPal of TrackingInfo list


type Model = {
    NewEvents: Event list
    Processed: Result<Event, Error> list
    RecentEvents: OkEvent list // Added after 1st event (OrdersFetched)
}


let init = 
    Ok {
        New = List.empty
        Processed = Result<Event, Error> list
    },
    [ Cmd.FetchLatestEvents ]


// TODO: Next: Start with update() function. 
// TODO: Think about How to handle the Error case 
// (maybe just fill up WfState.ErrorEvents)
// let update event state = 

open Ump
open Serilog

type CommandConfig = {
    Logger: ILogger
    DbConnection: string
    ShopUrl: string
    // ...
}


let update msg state = 
    match msg with
    | Msg.EventsFetched (Ok events) -> 
        let newState = { state with RecentEvents = events }
        let commands = 
            match events with
            | Ok events' -> 
                events'
                |> List.collect (fun e -> 
                    match e with
                    | Event.OrderFetched { Id = id; Data = order } ->  newState, [ Cmd.SetStatusProcessing ]
                    | Event.WarehouseOrderSent { Id = id; Data = order } ->  newState, [ Cmd.SetStatusProcessing ]
                    | Event.TrackingNumberFetched { Id = id; Data = order } ->  newState, [ Cmd.SetStatusProcessing ]
                    | Event.OrderUpdateProcessing { Id = id; Data = order } ->  newState, [ Cmd.SetStatusProcessing ]  
                    | Event.OrderUpdateAwaitingShipping { Id = id; Data = order } ->  newState, [ Cmd.SetStatusProcessing ]
                    | Event.OrderUpdateCompleted { Id = id; Data = order } ->  newState, [ Cmd.SetStatusProcessing ]    
                    | Event.PayPalTrackingSent { Id = id; Data = order } ->  newState, [ Cmd.SetStatusProcessing ]     
                )

            | Error err -> []

        newState, commands


    | Msg.EventsFetched (Error errs) -> state, (Error errs )




    // TODO: Running OrderIds -> Failed OrdersFetched Event -> Put in Failed Events
    // TODO: Add PayPal with Result<Result<x, y> list>
    // TODO: Write out simple types for all Events involved.
        

(*
let perform config cmdFunc command = 
    match command with
    // Load order events from DB
    | FetchLatestEvents -> 
       async {
            return cmdFunc.FetchOrders ()
       }
*)


