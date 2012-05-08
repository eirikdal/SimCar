module PHEV

#nowarn "25"

open System
open System.Collections.Generic
open System.Threading
open SynchronizationContext
open Message
open Agent
open Models
open PostalService
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
//open Node
let mutable rand = new System.Random()
//let syncContext = SynchronizationContext.CaptureCurrent()

module Action = 
    let send_intention (phev_args : PhevArguments) ttl =
        let intention, wait = 
            if phev_args.intentions.Length = 0 then
                Charge(phev_args.name, Energy.ofFloat (float (phev_args.capacity - phev_args.battery)), ttl, phev_args.rate), true
            else
                match phev_args.intentions with
                | h::t -> Charge_OK(phev_args.name, h, -1), false
                | [] -> Charge_OK(phev_args.name, 0.0<kWh>,-1), false

        postalService.send(phev_args.parent, intention)
        wait

    let create_intention ({ battery=battery; capacity=capacity; rate=rate }) tick ttl : float<kWh> list = 
        let rec gen intentions remaining : int list =
            let r = rand.Next(tick,ttl)
            let exists = intentions |> List.exists (fun x -> r=x)
            if remaining > 0.0<kWh> && exists && (tick-ttl) > intentions.Length then 
                gen intentions remaining
            else if remaining > 0.0<kWh> && not exists then
                gen (r :: intentions) (remaining - rate)
            else 
                intentions
        let intentions = gen [] (capacity-battery)
        
        [for i in tick .. ttl do if List.exists (fun x -> x=i) intentions then yield rate else yield 0.0<kWh>]

    let generate_intention ({ battery=battery; capacity=capacity; rate=rate }) probs : float<kWh> list = 
        let rec gen problist intentions rem = 
            match problist with 
            | h::t -> if rand.NextDouble() <= h && rem > 0.0<kWh> then gen t (rate::intentions) (rem-rate) else gen t ((0.0<kWh>)::intentions) rem
            | _ -> intentions

        gen probs [] (capacity-battery)

    let charge (phev_args : PhevArguments) =
        // PHEV stayed home, charge battery if less than full
        PHEV(phev_args.charge())

    let leave name (phev_args : PhevArguments) tick =  
        // If first time running, calculate and cache the distributions
        let ({ profile=(FloatProfile(_,dist_list)) } as phevArgs) = 
            { phev_args with profile=phev_args.profile.float_profile() }

        let r = rand.NextDouble()
        // try to find a distribution that matches the random number r
        let dist = dist_list |> Seq.tryFind (fun ({dist=dist}) -> r < (Seq.nth (tick%96) dist))
                       
        // if a distribution was found, let the PHEV leave with the corresponding duration of the distribution
        match dist with
        | Some d ->   
            //syncContext.RaiseEvent jobDebug <| sprintf "PHEV %s left with chance %f and probability %f" name r (Seq.nth (tick%96) d.dist) 
            PHEV(phevArgs.leave(tick, d.duration))
        | None ->
            PHEV({ phevArgs with left=(-1);})

    let find_ttl (histogram : int array) tick =
        let tick' = (tick%96)
        let nbhood = (tick'+ 76)

        // finding the mode
        [for i in tick' .. nbhood do yield histogram.[(i%96)]] 
        |> List.fold (fun ((i, cur), ac) x -> if x > ac then ((i+1,i),x) else ((i+1,cur),ac)) ((0,0),0) 
        |> fst 
        |> snd
        |> (+) tick


module Agent = 
    module Centralized = 
        let create_phev_agent _p = Agent<Message>.Start(fun agent ->
            let queue = new Queue<Message>() 
            let test = new MathNet.Numerics.Statistics.Histogram()
    
            let rec loop (PHEV({ name=name; parent=parent; histogram=histogram } as phev_args) as phev) waiting = async {
                let! (msg : Message) = 
                    if not waiting && queue.Count > 0 then
                        async { return queue.Dequeue() }
                    else
                        agent.Receive()
                match msg with
                | ReplyTo(replyToMsg, reply) ->
                    match replyToMsg with
                    | RequestModel ->
                        if not waiting then
                            reply.Reply(Model(phev))
                            return! loop phev false
                        else
                            queue.Enqueue(msg)
                            return! loop phev waiting
                | Model(phev) ->
                    return! loop phev waiting
                | Charge_Accepted(accepted) ->
                    let phevArgs = { phev_args with intentions=accepted }
            
                    let wait_for_reply = Action.send_intention phevArgs (-1)
            
                    return! loop (PHEV(phevArgs)) wait_for_reply
                | Charge_OK(_,_,_) ->
                    return! loop (Action.charge phev_args) false
                | Update(tick) ->
                    let ttl = Action.find_ttl histogram tick 
                    let wait_for_reply = Action.send_intention phev_args ttl
            
                    if phev_args.name = "phev_827" then
                        syncContext.RaiseDelegateEvent phevBattery phev_args.battery
                        if phev_args.duration > 0 then
                            syncContext.RaiseDelegateEvent phevStatus 1.0
                        else 
                            syncContext.RaiseDelegateEvent phevStatus 0.0

                    if phev_args.duration <= 0 then
                        return! loop <| Action.leave name phev_args tick <| true
                    else
                        return! loop <| PHEV(phev_args.drive()) <| true
                | Reset -> 
                    return! loop <| PHEV({ phev_args with battery=phev_args.capacity; duration=(-1) }) <| false
                | Kill ->
                    printfn "Agent %s: Exiting.." name
                | _ -> 
                    syncContext.RaiseEvent error <| Exception("PHEV: Not implemented yet")

                    return! loop phev waiting }
            loop _p false)
    module Decentralized = 
        module Mixed = 
            let create_phev_agent _p = Agent<Message>.Start(fun agent ->
                let queue = new Queue<Message>() 
                let test = new MathNet.Numerics.Statistics.Histogram()
    
                let rec loop (PHEV({ name=name; parent=parent; histogram=histogram } as phev_args) as phev) waiting tick = async {
                    let! (msg : Message) = 
                        if not waiting && queue.Count > 0 then
                            async { return queue.Dequeue() }
                        else
                            agent.Receive()
                    match msg with
                    | ReplyTo(replyToMsg, reply) ->
                        match replyToMsg with
                        | RequestModel ->
                            if not waiting then
                                reply.Reply(Model(phev))
                                return! loop phev false tick
                            else
                                queue.Enqueue(msg)
                                return! loop phev waiting tick
                    | Model(phev) ->
                        return! loop phev waiting tick
                    | Charge_OK(_,_,_) ->
                        return! loop (Action.charge phev_args) false tick
                    | Update(tick) ->
                        postalService.send("brp", RequestMixed(name))
                        return! loop phev waiting tick
                    | Mixed(problist) ->
                        let ttl = Action.find_ttl histogram tick 
                        let intentions = Action.generate_intention phev_args problist

                        let msg = 
                            match intentions with
                            | h::t -> Charge_OK(phev_args.name, h, ttl)
                            | [] -> Charge_OK(phev_args.name, 0.0<kWh>,ttl)
//                        printfn "PHEV %s: Sending charge_ok to %s" name parent
                        postalService.send(phev_args.parent, msg)
            
                        if phev_args.name = "phev_827" then
                            syncContext.RaiseDelegateEvent phevBattery phev_args.battery
                            if phev_args.duration > 0 then
                                syncContext.RaiseDelegateEvent phevStatus 1.0
                            else 
                                syncContext.RaiseDelegateEvent phevStatus 0.0

                        if phev_args.duration <= 0 then
                            return! loop <| Action.leave name { phev_args with intentions=intentions } tick <| true <| tick
                        else
                            return! loop <| PHEV({ phev_args with intentions=intentions }.drive()) <| true <| tick
                    | Reset -> 
                        return! loop <| PHEV({ phev_args with battery=phev_args.capacity; duration=(-1) }) <| false <| tick
                    | Kill ->
                        printfn "Agent %s: Exiting.." name
                    | _ -> 
                        syncContext.RaiseEvent error <| Exception("PHEV: Not implemented yet")

                        return! loop phev waiting tick }
                loop _p false 0)
        module Random = 
            let create_phev_agent _p = Agent<Message>.Start(fun agent ->
                let queue = new Queue<Message>() 
                let test = new MathNet.Numerics.Statistics.Histogram()
    
                let rec loop (PHEV({ name=name; parent=parent; histogram=histogram } as phev_args) as phev) waiting = async {
                    let! (msg : Message) = 
                        if not waiting && queue.Count > 0 then
                            async { return queue.Dequeue() }
                        else
                            agent.Receive()
                    match msg with
                    | ReplyTo(replyToMsg, reply) ->
                        match replyToMsg with
                        | RequestModel ->
                            if not waiting then
                                reply.Reply(Model(phev))
                                return! loop phev false
                            else
                                queue.Enqueue(msg)
                                return! loop phev waiting
                    | Model(phev) ->
                        return! loop phev waiting
                    | Charge_OK(_,_,_) ->
                        return! loop (Action.charge phev_args) false
                    | Update(tick) ->
                        let ttl = Action.find_ttl histogram tick 
                        let intentions = Action.create_intention phev_args tick ttl

                        let msg = 
                            match intentions with
                            | h::t -> Charge_OK(phev_args.name, h, ttl)
                            | [] -> Charge_OK(phev_args.name, 0.0<kWh>,ttl)

                        postalService.send(phev_args.parent, msg)
            
                        if phev_args.name = "phev_827" then
                            syncContext.RaiseDelegateEvent phevBattery phev_args.battery
                            if phev_args.duration > 0 then
                                syncContext.RaiseDelegateEvent phevStatus 1.0
                            else 
                                syncContext.RaiseDelegateEvent phevStatus 0.0

                        if phev_args.duration <= 0 then
                            return! loop <| Action.leave name { phev_args with intentions=intentions } tick <| true
                        else
                            return! loop <| PHEV({ phev_args with intentions=intentions }.drive()) <| true
                    | Reset -> 
                        return! loop <| PHEV({ phev_args with battery=phev_args.capacity; duration=(-1) }) <| false
                    | Kill ->
                        printfn "Agent %s: Exiting.." name
                    | _ -> 
                        syncContext.RaiseEvent error <| Exception("PHEV: Not implemented yet")

                        return! loop phev waiting }
                loop _p false)