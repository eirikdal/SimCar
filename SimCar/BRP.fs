module BRP

#nowarn "25"

open Agent
open Message
open Models
open System
open System.Collections
open System.Threading
open PostalService
open SynchronizationContext

//let gen charge tick = 
//    match charge with 
//    | Charge(_,energy,ttl,rate) ->
//        Seq.unfold (fun (state, energy, i) -> 
//                        if (i > 96) then None
//                        elif energy > 0.0 && i > tick then Some(rate, (state,energy-rate,i+1))
//                        else Some(0.0, (state, 0.0, i+1))) (0.0, Energy.toFloat energy, 0)

//let sum_arrays (arrays : float array array) = 
//    let mutable _array = Array.init (96) (fun f -> 0.0)
//    for i in 0 .. 95 do
//        for j in 0 .. arrays.Length do
//            _array.[i] <- _array.[i] + arrays.[j].[i]
//    _array

// creates an intention graph for each of the PHEVs
//        let intentions = 
//            queue.ToArray()
//            |> Array.map (fun msg -> msg :?> Message)
//            |> Array.filter (fun msg -> match msg with | ReplyTo(Charge(_,_,_,_),_) -> true | _ -> false)
//            |> Array.map (fun (ReplyTo(Charge(name,energy,ttl,rate) as charge,reply)) -> 
//                Array.ofSeq <| gen charge tick)
//            |> sum_arrays
//            |> Array.sum
//
//module Action = 
//    let schedule(dayahead, prediction, queue : Queue, tick) =
//        let intentions = 
//            queue.ToArray()
//            |> Array.map (fun msg -> msg :?> Message)
//            |> Array.filter (fun msg -> match msg with | ReplyTo(Charge(_,_,_,_),_) -> true | _ -> false)
//            |> Array.sortBy (fun (ReplyTo(Charge(_,_,ttl,_),_)) -> ttl)
//
//        let sum_of_intentions = 
//            intentions
//            |> Array.fold (fun ac (ReplyTo(Charge(name,energy,ttl,rate),_)) -> ac+energy) 0.0<kWh>
//
//        let reserve energy t = 
//            intentions |> Array.scan (fun balance transactionAmount -> balance + transactionAmount) energy 
//        let reserve energy i = 
//            if i > 96 then 
//                None
//            elif i > tick && energy > 0.0<kWh> then
//                let dif = dayahead i - prediction i
//                Some(dif, (energy-dif,i+1))
//            else
//                Some(0.0<kWh>, (energy,i+1))
//        
//        Seq.unfold (fun (energy, i) -> reserve energy i) (sum_of_intentions, 0) |> Array.ofSeq

module Action = 
    let reserve ac energy rate from = 
        if ac > 0.0<kWh> then
            let accepted = if energy > rate then rate else energy
            postalService.send(from, Charge_Accepted(accepted))                
            ac - rate
        else
            postalService.send(from, Charge_Accepted(0.0<kWh>))
            ac

    let rec reduce_queue queue = 
        [for msg in queue do 
            match msg with 
            | Charge_Intentions(_, msg) ->
                yield! reduce_queue msg
            | Charge(_,_,_,_) -> yield! [msg]
            | Charge_OK(_) ->
                yield! []]
//        queue
//        |> Array.ofList
//        |> Array.filter (fun msg -> match msg with | ReplyTo(Charge(_,_,_,_),_) -> true | _ -> false)

    let schedule_reactive (dayahead : dayahead) (prediction : realtime) queue tick = 
        queue
        |> List.sortBy (fun (Charge(_,_,ttl,_)) -> ttl)
        |> List.fold (fun ac (Charge(from,energy,_,rate)) -> reserve ac energy rate from) (dayahead (tick) - prediction (tick))
        |> ignore

    let schedule_greedy dayahead prediction queue tick =
        reduce_queue queue
        |> List.iter (fun (Charge(from,energy,_,rate)) -> reserve 1.0<kWh> energy rate from |> ignore)

let brp_agent brp schedule = Agent.Start(fun agent ->
    let queue = new Queue() 
    let rec loop (BRP({ children=children; } as brp_args) as brp) (intentions : Message list) schedule (tick : int) = async {
        let! (msg : Message) = 
            if (intentions.Length = children.Length && queue.Count > 0) then
                async { return queue.Dequeue() :?> Message }
            else
                agent.Receive()

        match msg with
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                if children.Length > intentions.Length then
                    queue.Enqueue(msg)
//                    printfn "RequestModel received"
                    return! loop brp intentions schedule tick
                else
//                    printfn "BRP responding to RequestModel"
                    syncContext.RaiseEvent jobDebug <| "BRP responding to RequestModel"
                    reply.Reply(Model(brp))
                    return! loop brp [] schedule tick
            | RequestDayahead ->
                reply.Reply(Model(brp))
                return! loop brp [] schedule tick
        | Update(tick) -> 
            return! loop brp [] schedule tick
        | Dayahead(dayahead) ->
            return! loop <| BRP({ brp_args with dayahead=dayahead }) <| intentions <| schedule <| tick
        | Prediction(realtime) ->
            return! loop <| BRP({ brp_args with realtime=realtime }) <| intentions <| schedule <| tick
        | Schedule(schedule) ->
            return! loop brp intentions schedule tick
        | Model(brp) -> 
            return! loop brp intentions schedule tick
        | Charge_Intentions(from, messages) -> 
//            printfn "Received charges from %s: %d > %d" from (intentions.Length+1) children.Length
            if intentions.Length + 1 >= children.Length then
                syncContext.RaiseEvent jobDebug <| "BRP got charges"
                let messages = (msg :: intentions) |> List.collect (fun x -> match x with | Charge_Intentions(_,msgs) -> msgs) |> Action.reduce_queue
                schedule brp_args.dayahead brp_args.realtime messages tick
//                printfn "Excess energy %f" (Energy.toFloat test)
//                printfn "BRP got charges"

                return! loop brp (msg :: intentions) schedule tick
            else
                return! loop brp (msg :: intentions) schedule tick
        | Reset -> return! loop brp intentions schedule tick
        | _ -> 
            syncContext.RaiseEvent error <| Exception("BRP: Not implemented yet")

        return! loop brp intentions schedule tick}    

    loop brp [] schedule 0)