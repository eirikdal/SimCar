module BRP

#nowarn "25"

open Agent
open Message
open Models
open System
open System.Collections.Generic
open System.Threading
open PostalService
open SynchronizationContext

module Action = 
    let prediction' = FileManager.Parsing.parse_dayahead_file(FileManager.file_prediction)
    let dayahead' = FileManager.Parsing.parse_dayahead_file(FileManager.file_dayahead)
    
    let reserve ac energy rate phev trf = 
        if ac > 0.0<kWh> && ac > rate then
            let accepted = if energy > rate then rate else energy
            postalService.send(phev, Charge_Accepted([accepted]))                
            ac - accepted
        else
            postalService.send(phev, Charge_Accepted([0.0<kWh>]))
            ac

    let rec create_plan remaining (avail : (int * energy) list) (plan : (int * energy) list) rate = 
        if avail.Length > 0 then
            let (tick, max) = List.maxBy (fun (_,v) -> v) avail
            let avail' = avail |> List.filter (fun (tick',_) -> if tick = tick' then false else true)
            
            let rate' = if remaining >= rate then rate else if remaining > 0.0<kWh> then remaining else 0.0<kWh>
            create_plan (remaining - rate') avail' ((tick,rate')::plan) rate
        else
            // for each tick, increase prediction by rate at tick t
            plan |> List.iter (fun (t,_) -> prediction'.[t] <- prediction'.[t] + (rate |> Energy.toFloat))
            plan |> List.sortBy (fun (tick,_) -> tick) |> List.unzip |> snd

    let schedule_greedy (dayahead : dayahead) (prediction : realtime) queue tick = 
        queue
        |> List.iter 
            (fun (trf,Charge(from,energy,ttl,rate)) ->
                let avail = [for i in tick .. ttl do yield i,((dayahead'.[i] |> Energy.ofFloat) - (prediction'.[i] |> Energy.ofFloat))]

                let plan = create_plan energy avail [] rate
                postalService.send(from, Charge_Accepted(plan)))

    let schedule_proactive (dayahead : dayahead) (prediction : realtime) queue tick = 
        let (_,Charge(_,_,_,T)) = queue |> List.minBy (fun (_,Charge(_,_,_,rate)) -> rate)
        let sum_of_int = queue |> List.sumBy (fun (_,Charge(_,energy,_,_)) -> energy)
        let difs = [for i in tick .. (96*(1+((tick)/96))-1) do yield (dayahead(i) - prediction(i))] 
        let sum_difs = List.sum difs
        let sum_ratio = (sum_of_int) / (List.sum difs)
        let k = if sum_ratio > 1.0 then 1.0 else sum_ratio
        let test = dayahead(tick) - prediction(tick)
        let accepted = k*(dayahead(tick)-prediction(tick))
        queue 
        |> List.sortBy (fun (_,Charge(_,_,ttl,_)) -> ttl)
        |> List.fold (fun ac (trf,Charge(from,energy,_,rate)) -> reserve ac energy rate from trf) (accepted)
        |> ignore

    let schedule_reactive (dayahead : dayahead) (prediction : realtime) queue tick = 
        queue
        |> List.sortBy (fun (_,Charge(_,_,ttl,_)) -> -ttl)
        |> List.fold (fun ac (trf,Charge(from,energy,_,rate)) -> reserve ac energy rate from trf) (dayahead (tick) - prediction (tick))
        |> ignore

    let schedule_none dayahead prediction queue tick =
        queue
        |> List.iter (fun (trf,Charge(from,energy,_,rate)) -> reserve (Energy.ofFloat infinity) energy rate from trf |> ignore)

module Agent =
    module Centralized = 
        let create_brp_agent brp = Agent.Start(fun agent ->
            let queue = new Queue<Message>() 
    
            let rec loop (BRP({ children=children; } as brp_args) as brp) (intentions : Message list) schedule (tick : int) waiting = async {
                let! (msg : Message) = 
                    if (not waiting && queue.Count > 0) then
                        async { return queue.Dequeue() }
                    else
                        agent.Receive()

                match msg with
                | ReplyTo(replyToMsg, reply) ->
                    match replyToMsg with
                    | RequestModel ->
                        if waiting then
                            queue.Enqueue(msg)
                            return! loop brp intentions schedule tick waiting
                        else
                            reply.Reply(Model(brp))
                            return! loop brp [] schedule tick false
                    | RequestDayahead ->
                        reply.Reply(Dayahead(brp_args.dayahead))
                        return! loop brp [] schedule tick waiting
                | Update(tick) -> 
                    return! loop brp [] schedule tick true
                | Dayahead(dayahead) ->
                    return! loop <| BRP({ brp_args with dayahead=dayahead }) <| intentions <| schedule <| tick <| waiting
                | Prediction(realtime) ->
                    return! loop <| BRP({ brp_args with realtime=realtime }) <| intentions <| schedule <| tick <| waiting
                | Schedule(schedule) ->
                    return! loop brp intentions schedule tick waiting
                | Model(brp) -> 
                    return! loop brp intentions schedule tick waiting
                | Charge_Intentions(messages) -> 
                    if intentions.Length + 1 >= children.Length then
                        let messages' = (msg :: intentions) |> Message.reduce_queue
                        schedule brp_args.dayahead brp_args.realtime messages' tick

                        return! loop brp [] schedule tick false
                    else
                        return! loop brp (msg :: intentions) schedule tick waiting
                | Reset -> return! loop brp intentions schedule tick waiting
                | Charge_OK(_,_,_) ->
                    return! loop brp (msg :: intentions) schedule tick waiting
                | Kill ->
                    syncContext.RaiseDelegateEvent jobProgress <| sprintf "Agent %s: Exiting.." "BRP"
                | _ -> 
                    return! loop brp intentions schedule tick waiting }

            loop brp [] (Action.schedule_none) 0 false)
    module Decentralized = 
        module Random = 
            let create_brp_agent brp = Agent.Start(fun agent ->
                let rec loop (BRP({ children=children; } as brp_args) as brp) (tick : int) = async {
                    let! (msg : Message) = agent.Receive()

                    match msg with
                    | ReplyTo(replyToMsg, reply) ->
                        match replyToMsg with
                        | RequestModel ->
                            reply.Reply(Model(brp))
                            return! loop brp tick
                        | RequestDayahead ->
                            reply.Reply(Dayahead(brp_args.dayahead))
                            return! loop brp tick
                    | Update(tick) -> 
                        return! loop brp tick
                    | Dayahead(dayahead) ->
                        return! loop <| BRP({ brp_args with dayahead=dayahead }) <| tick
                    | Prediction(realtime) ->
                        return! loop <| BRP({ brp_args with realtime=realtime }) <| tick
                    | Schedule(_) ->
    //                    raise <| Exception("Decentralized BRP agent does not support scheduling")
                        return! loop brp tick
                    | Model(brp) -> 
                        return! loop brp tick
                    | Reset -> return! loop brp tick
                    | Kill ->
                        syncContext.RaiseDelegateEvent jobDebug <| sprintf "Agent %s: Exiting.." "BRP"
                    | _ ->
                        return! loop brp tick}    

                loop brp 0)
        module Mixed = 
            let create_brp_agent brp = Agent.Start(fun agent ->
                let rec loop (BRP({ children=children; realtime=realtime } as brp_args) as brp) (tick : int) (predictions : energy array) = async {
                    let! (msg : Message) = agent.Receive()

                    match msg with
                    | ReplyTo(replyToMsg, reply) ->
                        match replyToMsg with
                        | RequestModel ->
                            reply.Reply(Model(brp))
                            return! loop brp tick predictions
                        | RequestDayahead ->
                            reply.Reply(Dayahead(brp_args.dayahead))
                            return! loop brp tick predictions
                    | RequestMixed(name, ttl) ->
//                        reply.Reply(Mixed(problist))
                        let window = [for i in 0 .. (ttl-tick) do yield predictions.[i]]
                        let (Strategy(strategy)) = postalService.send_reply(name, Mixed(window))
                        for i in 0 .. (List.length strategy-1) do 
                            predictions.[i] <- predictions.[i] + strategy.[i]
                        return! loop brp tick predictions
                    | Update(tick) -> 
                        [|for i in 0 .. 95 do yield predictions.[i] <- realtime(tick+i)|] |> ignore
                        return! loop brp tick predictions
                    | Dayahead(dayahead) ->
                        return! loop <| BRP({ brp_args with dayahead=dayahead }) <| tick <| predictions
                    | Prediction(realtime) ->
                        return! loop <| BRP({ brp_args with realtime=realtime }) <| tick <| predictions
                    | Schedule(_) ->
    //                    raise <| Exception("Decentralized BRP agent does not support scheduling")
                        return! loop brp tick predictions
                    | Model(brp) -> 
                        return! loop brp tick predictions
                    | Reset -> return! loop brp tick predictions
                    | Kill ->
                        syncContext.RaiseDelegateEvent jobDebug <| sprintf "Agent %s: Exiting.." "BRP"
                    | _ ->
                        return! loop brp tick predictions}    

                loop brp 0 (Array.zeroCreate(96)))