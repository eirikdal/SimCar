// Learn more about F# at http://fsharp.net
module SimCar

open Agent
open DayAhead
open System
open System.Threading
open System.IO
open SynchronizationContext
open Models
open Message
open PHEV
open BRP 
open PowerNode
open PostalService
open FileManager
open Transformer
open System.Windows
open Grid

#nowarn "25"

let mutable schedule : dayahead -> realtime -> (MailboxProcessor<Message> * Message) list -> int -> unit = BRP.Action.schedule_greedy

let test_dayahead iter agents = 
    let tick n = 
        agents
        |> Tree.send (Update(n)) // inform agents that new tick has begun
        |> Tree.send_reply RequestModel // request model from agents

    let realtime = Array.init(96) (fun i -> tick i)

    let updated_realtime = 
        realtime
        |> Array.map (Tree.foldr Util.update) // right-fold over tree, applying the update function (inorder traversal)
    
    let rec shave n rt = 
        syncContext.RaiseDelegateEvent dayaheadProgress rt
        if n > 0 then 
            shave (n-1) (rt |> DayAhead.Shifted.shave 0.3 0.95)

    shave iter updated_realtime

//    syncContext.RaiseDelegateEvent progressPnode updated_realtime      
//    syncContext.RaiseDelegateEvent progressTotal moving_dayahead

let kill agents = 
    agents
    |> Tree.send (Kill)
    |> ignore

// main control flow of the simulator
let run day agents compute_dayahead =
    let tick n = 
        agents
        |> Tree.send (Update(n)) // inform agents that new tick has begun
        |> (fun x -> syncContext.RaiseDelegateEvent jobDebug <| sprintf "Tick %i" n; x)
        |> Tree.send_reply RequestModel // request model from agents

    syncContext.RaiseDelegateEvent jobProgress <| "----------------"
    syncContext.RaiseDelegateEvent jobProgress <| sprintf "Day %d:\n" day

    let realtime = Array.init(96) (fun i -> tick ((day*96) + i))

    // right-fold over tree, applying the update function (inorder traversal)
    let updated_realtime = 
        realtime
        |> Array.map (Tree.foldr Util.update) 

    let pnodes = 
        realtime
        |> Array.map (Tree.foldr Util.fold_pnodes)

    let phevs = 
        realtime
        |> Array.map (Tree.foldr Util.fold_phevs)

//    let moving_dayahead = 
//        updated_realtime
//        |> moving_average
//        |> Array.ofSeq
    
    if compute_dayahead then
        IO.write_doubles <| FileManager.file_prediction <| Parsing.parse_dayahead (List.ofArray pnodes)
        IO.write_doubles <| FileManager.file_dayahead <| Parsing.parse_dayahead (List.ofArray phevs)
//        syncContext.RaiseEvent updateEvent <| dayahead
    else
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "PHEVs\t\t %f" (phevs |> Array.map Energy.toFloat |> Array.sum)
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "PowerNodes\t %f" (pnodes |> Array.map Energy.toFloat |> Array.sum)
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Total\t\t %f" (updated_realtime |> Array.map Energy.toFloat |> Array.sum)
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "PAR\t\t %f" ((Array.max updated_realtime) / (Array.average updated_realtime))

        let (Model(BRP( { dayahead=dayahead }))) = postalService.send_reply("brp", RequestDayahead)

        let dayahead = Array.init(96) (fun i -> dayahead ((day*96) + i))
        // Raise events
        syncContext.RaiseDelegateEvent dayaheadProgress dayahead
        syncContext.RaiseDelegateEvent progressPhev phevs
        syncContext.RaiseDelegateEvent progressPnode pnodes
        syncContext.RaiseDelegateEvent progressTotal updated_realtime