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
    let reserve ac energy rate phev = 
        if ac > 0.0<kWh> && ac > rate then
            let accepted = if energy > rate then rate else energy
            postalService.send(phev, Strategy([accepted]))                
            ac - accepted
        else
            postalService.send(phev, Strategy([0.0<kWh>]))
            ac

    let rec create_plan_reactive remaining avail (plan : energy array) rate = 
        if remaining > 0.0<kWh> then 
            match avail with 
            | (i,en)::t -> 
                let rate' = if remaining >= rate && en > 0.0<kWh> then rate else if remaining > 0.0<kWh> && en > 0.0<kWh> then remaining else 0.0<kWh>
                plan.[i] <- rate'
                create_plan_reactive (remaining-rate') t plan rate
            | _ -> 
                remaining, plan
        else    
            remaining, plan

    let rec create_plan_proactive remaining avail (plan : energy array) rate = 
        if remaining > 0.0<kWh> then 
            match avail with 
            | (i,en)::t -> 
                let rate' = if remaining >= rate then rate else if remaining > 0.0<kWh> then remaining else 0.0<kWh>
                plan.[i] <- rate'
                create_plan_proactive (remaining-rate') t plan rate
            | _ -> 
                plan
        else    
            plan

    module Reactive = 
        let schedule (dayahead : dayahead) (prediction : realtime) queue tick = 
            queue
            |> List.iter 
                (fun (Charge(from,energy,ttl,rate)) ->
                    let avail, plan = [for i in tick .. ttl do yield ((i-tick),dayahead.[i] - prediction.[i]), 0.0<kWh>] |> List.unzip

                    let mutable rem, plan = create_plan_reactive energy avail (Array.ofList plan) rate

//                    if rem > 0.0<kWh> then
//                        for i in (plan.Length-1) .. -1 .. 0 do 
//                            if rem > 0.0<kWh> && plan.[i] = 0.0<kWh> then 
//                                plan.[i] <- rate
//                                rem <- rem - rate

                    Array.iteri (fun i x -> prediction.[tick+i] <- prediction.[tick+i] + x) plan
                    postalService.send(from, Strategy(List.ofArray plan)))

    module Proactive =
        let schedule (dayahead : dayahead) (prediction : realtime) queue tick = 
            queue
            |> List.iter 
                (fun (Charge(from,energy,ttl,rate)) ->
    //                let avail = [for i in tick .. ttl do yield i,((dayahead'.[i] |> Energy.ofFloat) - (prediction'.[i] |> Energy.ofFloat))]
                    let avail, plan = [for i in tick .. ttl do yield ((i-tick),dayahead.[i] - prediction.[i]), 0.0<kWh>] |> List.unzip

                    let rem, plan = create_plan_reactive energy (List.sortBy (fun (i,energy) -> -energy) avail) (Array.ofList plan) rate
                    Array.iteri (fun i x -> prediction.[tick+i] <- prediction.[tick+i] + x) plan
                    postalService.send(from, Strategy(List.ofArray plan)))

    module Average = 
        let schedule (dayahead : dayahead) (prediction : realtime) queue tick = 
            let (Charge(_,_,_,T)) = queue |> List.minBy (fun (Charge(_,_,_,rate)) -> rate)
            let sum_of_int = queue |> List.sumBy (fun (Charge(_,energy,_,_)) -> energy)
            let difs = [for i in tick .. (96*(1+((tick)/96))-1) do yield (dayahead.[i] - prediction.[i])] 
            let sum_difs = List.sum difs
            let sum_ratio = (sum_of_int) / (List.sum difs)
            let k = if sum_ratio > 1.0 then 1.0 else sum_ratio
            let test = dayahead.[tick] - prediction.[tick]
            let accepted = k*(dayahead.[tick]-prediction.[tick])
            queue 
            |> List.sortBy (fun (Charge(_,_,ttl,_)) -> ttl)
            |> List.fold (fun ac (Charge(from,energy,_,rate)) -> reserve ac energy rate from) (accepted)
            |> ignore

    module None = 
        let schedule dayahead prediction queue tick =
            queue
            |> List.iter (fun (Charge(from,energy,_,rate)) -> reserve (Energy.ofFloat infinity) energy rate from |> ignore)

module Agent =
    module Centralized =  
        let create_brp_agent brp phevs = Agent.Start(fun agent ->
            let queue = new Queue<Message>() 
    
            let rec loop (BRP({ children=children; } as brp_args) as brp) (intentions : Message list) schedule (tick : int) phevs waiting = async {
                let! (msg : Message) = 
                    if (not waiting && queue.Count > 0) then
                        async { return queue.Dequeue() }
                    else
                        agent.Receive()

                match msg with
                | ReplyTo(replyToMsg, reply) ->
                    match replyToMsg with
                    | RequestModel ->
                        if waiting && phevs <> 0 then
                            queue.Enqueue(msg)
                            return! loop brp intentions schedule tick phevs waiting
                        else
                            reply.Reply(Model(brp))
                            return! loop brp [] schedule tick phevs false
                    | RequestDayahead ->
                        reply.Reply(Dayahead(brp_args.dayahead))
                        return! loop brp [] schedule tick phevs waiting
                | Update(tick) -> 
                    return! loop brp [] schedule tick phevs true
                | Dayahead(dayahead) ->
                    return! loop <| BRP({ brp_args with dayahead=dayahead }) <| intentions <| schedule <| tick <| phevs <| waiting
                | Prediction(realtime) ->
                    return! loop <| BRP({ brp_args with realtime=realtime }) <| intentions <| schedule <| tick <| phevs <| waiting
                | Schedule(schedule) ->
                    return! loop brp intentions schedule tick phevs waiting
                | Model(brp) -> 
                    return! loop brp intentions schedule tick phevs waiting
                | Reset -> return! loop brp intentions schedule tick phevs waiting
                | Charge(_,_,_,_) -> 
                    if intentions.Length + 1 >= phevs then
                        schedule brp_args.dayahead brp_args.realtime (List.filter (function | Charge(_,_,_,_) -> true | _ -> false) (msg::intentions)) tick

                        return! loop brp [] schedule tick phevs false
                    else
                        return! loop brp (msg :: intentions) schedule tick phevs waiting
                | Demand(_,_,_) ->
                    if intentions.Length + 1 >= phevs then
                        schedule brp_args.dayahead brp_args.realtime (List.filter (function | Charge(_,_,_,_) -> true | _ -> false) (msg::intentions)) tick

                        return! loop brp [] schedule tick phevs false
                    else
                        return! loop brp (msg :: intentions) schedule tick phevs waiting
                | Kill ->
                    syncContext.RaiseDelegateEvent jobProgress <| sprintf "Agent %s: Exiting.." "BRP"
                | _ -> 
                    printfn "ffs"
                    return! loop brp intentions schedule tick phevs waiting }

            loop brp [] (Action.None.schedule) 0 phevs false)
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
        module Predictions = 
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
                    | RequestPredictions(name, ttl) ->
//                        reply.Reply(Predictions(problist))
                        let window = [for i in 0 .. (ttl-tick) do yield predictions.[i]]
                        let (Strategy(strategy)) = postalService.send_reply(name, Predictions(window))
                        for i in 0 .. (List.length strategy-1) do 
                            predictions.[i] <- predictions.[i] + strategy.[i]
                        return! loop brp tick predictions
                    | Update(tick) -> 
                        [|for i in 0 .. 95 do yield predictions.[i] <- realtime.[tick+i]|] |> ignore
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