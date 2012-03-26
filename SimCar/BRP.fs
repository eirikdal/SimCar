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

module Action = 
    let prediction' = FileManager.Parsing.parse_dayahead_file(FileManager.file_prediction)

    let reserve ac energy rate from = 
        if ac > 0.0<kWh> && ac > rate then
            let accepted = if energy > rate then rate else energy
            postalService.send(from, Charge_Accepted([accepted]))                
            ac - accepted
        else
            postalService.send(from, Charge_Accepted([0.0<kWh>]))
            ac

    let rec create_plan remaining (avail : (int * energy) list) (plan : (int * energy) list) rate = 
        if avail.Length > 0 then
            let (tick, max) = List.max avail
            let avail' = avail |> List.filter (fun (tick',_) -> if tick = tick' then false else true)
            
            let rate' = if remaining > 0.0<kWh> then rate else 0.0<kWh>
            create_plan (remaining - rate') avail' ((tick,rate')::plan) rate
        else
            // for each tick, increase prediction by rate at tick t
            plan |> List.iter (fun (tick,_) -> prediction'.[tick] <- prediction'.[tick] + (rate |> Energy.toFloat))
            plan |> List.sortBy (fun (tick,_) -> tick) |> List.unzip |> snd

    let schedule_greedy (dayahead : dayahead) (prediction : realtime) queue tick = 
        queue
        |> List.iter 
            (fun (Charge(from,energy,ttl,rate)) ->
                let dayahead' = [for i in tick .. ttl do yield dayahead(i)]
                let pred_test' = [for i in tick .. ttl do yield prediction'.[i]]

                let avail = [for i in tick .. ttl do yield i,(dayahead(i) - (prediction'.[i] |> Energy.ofFloat))]

                if energy > 0.0<kWh> then 
                    printfn ""

                let plan = create_plan energy avail [] rate
                postalService.send(from, Charge_Accepted(plan)))

    let schedule_proactive (dayahead : dayahead) (prediction : realtime) queue tick = 
        let (Charge(_,_,_,T)) = queue |> List.minBy (fun (Charge(_,_,_,rate)) -> rate)
        let sum_of_int = queue |> List.sumBy (fun (Charge(_,energy,_,_)) -> energy)
        let difs = [for i in tick .. (96*(1+((tick)/96))-1) do yield (dayahead(i) - prediction(i))] 
        let sum_difs = List.sum difs
        let sum_ratio = (sum_of_int) / (List.sum difs)
        let k = if sum_ratio > 1.0 then 1.0 else sum_ratio
        let test = dayahead(tick) - prediction(tick)
        let accepted = k*(dayahead(tick)-prediction(tick))
        queue 
        |> List.sortBy (fun (Charge(_,_,ttl,_)) -> ttl)
        |> List.fold (fun ac (Charge(from,energy,_,rate)) -> reserve ac energy rate from) (accepted)
        |> ignore

    let schedule_reactive (dayahead : dayahead) (prediction : realtime) queue tick = 
        queue
        |> List.sortBy (fun (Charge(_,_,ttl,_)) -> -ttl)
        |> List.fold (fun ac (Charge(from,energy,_,rate)) -> reserve ac energy rate from) (dayahead (tick) - prediction (tick))
        |> ignore

    let schedule_none dayahead prediction queue tick =
        Message.reduce_queue queue
        |> List.iter (fun (Charge(from,energy,_,rate)) -> reserve (Energy.ofFloat infinity) energy rate from |> ignore)

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
                let messages = (msg :: intentions) |> List.collect (fun x -> match x with | Charge_Intentions(_,msgs) -> msgs) |> Message.reduce_queue
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