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

let compute_dayahead day agents = 
    let tick n = 
        agents
        |> Tree.send (Update(n)) // inform agents that new tick has begun
        |> (fun x -> syncContext.RaiseDelegateEvent jobDebug <| sprintf "Tick %i" n; x)
        |> Tree.send_reply RequestModel // request model from agents

    syncContext.RaiseDelegateEvent jobProgress <| "------------------------------------"
    syncContext.RaiseDelegateEvent jobProgress <| sprintf "Day %d:\n" day

    let realtime = Array.init(96) (fun i -> tick ((day*96) + i))

    let pnodes = 
        realtime
        |> Array.map (Tree.foldr Util.fold_pnodes)
        |> Array.map Energy.toFloat

    let phevs = 
        realtime
        |> Array.map (Tree.foldr Util.fold_phevs)
        |> Array.map Energy.toFloat

    IO.write_doubles <| FileManager.file_prediction <| List.ofArray pnodes
    IO.write_doubles <| FileManager.file_dayahead <| List.ofArray phevs

// main control flow of the simulator
let run day agents =
    let tick n = 
        agents
        |> Tree.send (Update(n)) // inform agents that new tick has begun
        |> (fun x -> syncContext.RaiseDelegateEvent jobDebug <| sprintf "Tick %i" n; x)
        |> Tree.send_reply RequestModel // request model from agents

    syncContext.RaiseDelegateEvent jobProgress <| "------------------------------------"
    syncContext.RaiseDelegateEvent jobProgress <| sprintf "Day %d:\n" day

    let realtime = Array.init(96) (fun i -> tick ((day*96) + i))

    // right-fold over tree, applying the update function (inorder traversal)
    let total = Array.map (Tree.foldr Util.update) realtime
    let pnodes = Array.map (Tree.foldr Util.fold_pnodes) realtime
    let phevs = Array.map (Tree.foldr Util.fold_phevs) realtime
    let trf_delta = Array.map (Tree.foldr Util.fold_trf_delta) realtime
    let trf_filtered = Array.map (Tree.foldr Util.fold_trf_filter) realtime
    
//        syncContext.RaiseEvent updateEvent <| dayahead
    let phevs_sum = Math.Round(Energy.toFloat <| Array.sum phevs,3)
    let pnodes_sum = Math.Round(Energy.toFloat <| Array.sum pnodes,3)
    let total_max = Math.Round(Energy.toFloat <| Array.max total,3)
    let total_avg = Math.Round(Energy.toFloat <| Array.average total,3)
    let total_sum = Math.Round(Energy.toFloat <| Array.sum total,3)
    let trf_delta = Math.Round(Energy.toFloat <| Array.sum trf_delta,3)
    let trf_filtered = Math.Round(Energy.toFloat <| Array.sum trf_filtered,3)
    let par = Math.Round(total_max / total_avg,3)
    
    // Raise events
    syncContext.RaiseDelegateEvent jobProgress <| sprintf "PHEVs\t\t %.2f" phevs_sum
    syncContext.RaiseDelegateEvent jobProgress <| sprintf "PowerNodes\t %.2f" pnodes_sum 
    syncContext.RaiseDelegateEvent jobProgress <| sprintf "TrfDelta\t\t %.2f" trf_delta
    syncContext.RaiseDelegateEvent jobProgress <| sprintf "TrfFiltered\t\t %.2f" trf_filtered
    syncContext.RaiseDelegateEvent jobProgress <| sprintf "Total\t\t %.2f" total_sum
    syncContext.RaiseDelegateEvent jobProgress <| sprintf "PAR\t\t %.2f" par

    let (Dayahead(dayahead)) = postalService.send_reply("brp", RequestDayahead)

    let dayahead = Array.init(96) (fun i -> Energy.toFloat <| dayahead ((day*96) + i))
    let dayahead_sum = Math.Round(Array.sum dayahead,3)
    let dif = Math.Round(dayahead |> Array.map2 (fun x y -> abs(Energy.toFloat(x) - y)) total |> Array.sum,3)
    let ratio = Math.Round(dif / total_sum,3)

    syncContext.RaiseDelegateEvent jobProgress <| sprintf "Dayahead\t %.2f" dayahead_sum
    syncContext.RaiseDelegateEvent jobProgress <| sprintf "Diff\t\t %.2f" dif
    syncContext.RaiseDelegateEvent jobProgress <| sprintf "Ratio\t\t %.2f" ratio

    syncContext.RaiseDelegateEvent dayaheadProgress dayahead
    syncContext.RaiseDelegateEvent progressPhev phevs
    syncContext.RaiseDelegateEvent progressPnode pnodes
    syncContext.RaiseDelegateEvent progressTotal total

    { phevs_sum=phevs_sum; pnodes_sum=pnodes_sum; total_max=total_max;
        total_avg=total_avg; total_sum=total_sum; par=par;
        dayahead_sum=dayahead_sum; dif=dif; ratio=ratio;
        trf_delta=trf_delta; trf_filtered=trf_filtered }
