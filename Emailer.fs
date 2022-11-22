module Emailer

// Model is the workflow state. 
// Effect represents side effects. 
// Msg is actually analogous to events, but for side effect results
// (e.g. something loaded or saved or failed).
// https://fsharp.slack.com/archives/C04BJKUP2/p1667493976170949?thread_ts=1666771263.493029&cid=C04BJKUP2

open System

type Settings = {
    SendFrom: string
    SubjectTemplate: string
    SendLimitSecond: int
    Today: DateTime
}

type DueItem = {
    ItemId: int
    EmailAddress: string
    NotificationHtml: string
}

type Email = {
    From: string
    To: string
    Subject: string
    Body: string
    Completed: int list
}

type Command = 
    | FindDueItems of DateTime
    | ScheduleSendNow
    | ScheduleSend of DateTimeOffset
    | SendEmail of Email
    | MarkCompleted of int list

type Event = 
    | DueItems of Result<DueItem list, unit>
    | TimeToSend of DateTimeOffset
    | EmailSent of Result<int list, unit>
    | Marked of Result<unit, unit>

type WfState = {
    Settings: Settings
    CurrentTime: DateTimeOffset
    ToSend: Email list list
    ToMark: Set<int>
}


module Logic = 

    let toEmail settings (emailAddress, groupedItems) = 
        {
            From    = settings.SendFrom
            Subject = settings.SubjectTemplate
            To      = emailAddress
            Body = 
                groupedItems
                |> List.map (fun item -> "&bull;" + item.NotificationHtml)
                |> String.concat "<br>"

            Completed = 
                groupedItems
                |> List.map (fun item -> item.ItemId)
        }


    let batch settings items = 
        items 
        |> List.groupBy (fun item -> item.EmailAddress)
        |> List.map (toEmail settings)
        |> List.chunkBySize settings.SendLimitSecond


let init settings =
    Ok {
        Settings = settings
        CurrentTime = DateTimeOffset.MinValue
        ToSend = []
        ToMark = Set.empty
    }, [FindDueItems settings.Today]

let update event state = 
    match event with 
    | DueItems (Ok items) -> 
        let toSend = Logic.batch state.Settings items

        Ok { state with ToSend = toSend },
        [ ScheduleSendNow ]

    | TimeToSend now -> 
        let newState = { state with CurrentTime = now }

        match newState.ToSend with 
        | [] -> Ok newState, []

        | batch :: remaining -> 
            let toMark = 
                batch
                |> List.collect (fun x -> x.Completed)
                |> Set.ofList

            Ok { newState with ToSend = remaining; ToMark = toMark },
            [ for email in batch do SendEmail email]

    | EmailSent (Ok itemIds) -> 
        let items = Set.ofList itemIds
        let toMark = Set.difference state.ToMark items

        Ok {state with ToMark = toMark },
        [ MarkCompleted itemIds ]

    | Marked (Ok ()) -> 
        let commands = 
            // No emails to mark, but emails to send
            if Set.isEmpty state.ToMark && not (List.isEmpty state.ToSend)
            then 
                let nextSend = state.CurrentTime.AddSeconds(1.0)
                [ ScheduleSend nextSend]
            else 
                []

        Ok state, commands

    | DueItems (Error ())
    | EmailSent (Error ())
    | Marked (Error ()) -> 
        Error (), []



open Serilog
open Ump

type CommandConfig = {
    Logger: ILogger
    // This is also where you would put things like:
    // -- Connection strings
    // -- Endpoint URLs
    // -- Credentials
    // -- Anything else your side effects need
}


let perform config command = 
    let makeItem itemId emailAddress notification = 
        {
            ItemId = itemId
            EmailAddress = emailAddress
            NotificationHtml = notification
        }

    match command with 
    | ScheduleSendNow -> 
        async { return TimeToSend DateTimeOffset.Now }

    | ScheduleSend time -> 
        let span = time - DateTimeOffset.Now
        let sleepTime = Math.Ceiling(span.TotalMilliseconds) |> int

        async {
            do! Async.Sleep sleepTime
            return TimeToSend DateTimeOffset.Now
        }

    | FindDueItems date -> 
        async {
            do! Async.Sleep 20 // Simulate latency
            return DueItems (
                // Dummy data. In a real app would be a DB call
                Ok [
                    makeItem 123 "stefan@sanofan.com" "Thanks for your order"
                    makeItem 456 "jonas@sanofan.com" "Thanks for your order"
                    makeItem 789 "luka@sanofan.com" "Thanks for your order"
                ]
            )
        }

    | SendEmail email -> 
    // How does this email record result in a list
    // of completed emails?
        async {
            do! Async.Sleep 20
            return EmailSent (Ok email.Completed)
        }

    | MarkCompleted itemIds -> 
    // Probably a DB call
        async {
            do! Async.Sleep 10
            return Marked (Ok ())
        }


let ump config = {
    Init = init
    Update = Result.bindUpdate update
    Perform = perform config
    Output = id
}

module Main = 
  open System
  open Serilog  
  open Serilog.Sinks.SystemConsole

  let main argv = 
    let config : CommandConfig = {
        Logger = 
            LoggerConfiguration()
            // Needs dotnet add package Destructurama.FSharp
                .Destructure.FSharpTypes()
                .WriteTo.Console()
                .MinimumLevel.Debug()
                .CreateLogger()
    }

    // String literals for demo purposes.
    // These should be loaded from configuration.
    let settings : Settings = {
        SendFrom = "Notificator <notify@example.com>"
        SubjectTemplate = "[My App] New Notifications"
        SendLimitSecond = 1
        Today = DateTime.Today
    }

    let ump = ump config

    // AKA decorator pattern in OO
    let loggedPerform command = 
        config.Logger.Debug("command {@Command}", box command)
        async {
            let! result = ump.Perform command
            config.Logger.Debug("result {@Result}", box result)
            return result
        }

    // Kasey: Later declaration of ump shadows previous one
    // Helps my simple brain 
    let ump = { ump with Perform = loggedPerform }

    Ump.run ump settings
    |> Async.RunSynchronously
    |> fun output -> config.Logger.Debug("output {@Output}", box output)

    #if Debug
        Console.WriteLine("Press ENTER to exit...")
        Console.ReadLine() |> ignore
    #endif







