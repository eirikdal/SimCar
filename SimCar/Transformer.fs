module Transformer

#nowarn "25"

open System
open System.Collections.Generic
open System.Threading
open SynchronizationContext
open Message
open Agent
open Models
open PostalService

module Action = 
    let filter energy rem = 
        if rem > energy then
            energy, rem - energy
        else
            0.0<kWh>, rem
    
    let filter_charges trf_args charges init = 
        charges 
        |> List.sortBy (fun (Demand(_,_,ttl)) -> ttl)
        |> List.fold (fun (rem,filtered) (Demand(name,energy,ttl)) ->     
            if not (name.StartsWith("med") || name.StartsWith("high")) then
                let filter, remaining =
                    if not (name.StartsWith("node")) then
                        filter energy rem
                    else
                        energy, rem
                postalService.send(name, Demand(name, filter, ttl))
                remaining, filtered+(energy-filter)
            else 
                rem,filtered) init
                
module Agent = 
    module Centralized = 
        let create_trf_agent trf = Agent.Start(fun agent ->
            let queue = new Queue<Message>() 
            let rec loop (Transformer({ name=name; parent=parent; children=children } as trf_args) as trf) (charges : Message list) filter waiting = async {
                if charges.Length >= children.Length then 
                    let sum_of_charges = charges |> List.sumBy (fun (Demand(_,energy,ttl)) -> energy)

                    let (rem, filtered) = 
                        if filter then 
                            let sum_of_nodes = charges |> List.sumBy (fun(Demand(name,energy,_)) -> if name.StartsWith("node") then energy else 0.0<kWh>)
                            Action.filter_charges trf_args charges (trf_args.capacity-sum_of_nodes, 0.0<kWh>)
                        else
                            charges |> List.iter (fun (Demand(name,energy,ttl)) -> 
                                if not (name.StartsWith("med") || name.StartsWith("high")) then
                                    postalService.send(name, Demand(name,energy,ttl)))
                            (trf_args.capacity, 0.0<kWh>)

                    if parent <> "brp" then
                        // propagate Demand upwards to (potentially) senior trf agents
                        postalService.send(parent, Demand(name,sum_of_charges-filtered,-1))

                    if name = "med_4" then
                        syncContext.RaiseDelegateEvent trfFiltered (filtered)
                        syncContext.RaiseDelegateEvent trfCurrent (sum_of_charges - filtered)
                        syncContext.RaiseDelegateEvent trfCapacity trf_args.capacity
                
                    return! loop (Transformer({ trf_args with filtered=filtered; current=sum_of_charges-filtered })) [] filter false
                else
                    let! (msg : Message) = 
                        if (not waiting && queue.Count > 0) then
                            async { return queue.Dequeue() }
                        else
                            agent.Receive()

                    match msg with
                    | Update(tick) ->
                        return! loop (Transformer({ trf_args with filtered=0.0<kWh>; current=0.0<kWh> })) charges filter true
                    | ReplyTo(replyToMsg, reply) ->
                        match replyToMsg with
                        | RequestModel ->
                            if waiting then
                                queue.Enqueue(msg)
                                return! loop trf charges filter waiting
                            else
                                reply.Reply(Model(trf))
                                return! loop trf [] filter false
                    | Model(trf) -> 
                        return! loop trf charges filter waiting
                    | Demand(from,energy,_) -> 
                        return! loop trf (msg :: charges) filter waiting
                    | Reset ->
                        return! loop trf charges filter waiting
                    | Filter(filter) ->
                        return! loop trf charges filter waiting
                    | Kill ->
                        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Agent %s: Exiting.." name
                    | _ as test ->
                        syncContext.RaiseDelegateEvent jobError (Exception((test.ToString())))
                        return! loop trf charges filter waiting
            }
            loop trf [] true false)
    module Decentralized = 
        let create_trf_agent trf = Agent.Start(fun agent ->
            let queue = new Queue<Message>() 
            let rec loop (Transformer({ name=name; parent=parent; children=children } as trf_args) as trf) (charges : Message list) filter waiting = async {
                if charges.Length >= children.Length then 
                    let sum_of_charges = charges |> List.sumBy (fun (Demand(_,energy,ttl)) -> energy)

                    let (rem, filtered) = 
                        if filter then 
                            let sum_of_nodes = charges |> List.sumBy (fun(Demand(name,energy,_)) -> if name.StartsWith("node") then energy else 0.0<kWh>)
                            Action.filter_charges trf_args charges (trf_args.capacity-sum_of_nodes, 0.0<kWh>)
                        else
                            charges |> List.iter (fun (Demand(name,energy,ttl)) -> 
                                if not (name.StartsWith("med") || name.StartsWith("high")) then
                                    postalService.send(name, Demand(name,energy,ttl)))
                            (trf_args.capacity, 0.0<kWh>)

                    // propagate Demand upwards to (potentially) senior trf agents
                    postalService.send(parent, Demand(name,sum_of_charges-filtered,-1))
                    
//                    if filtered > 0.0<kWh> || sum_of_charges-filtered > trf_args.capacity then
//                        printfn "%s" name

                    if name = "med_4" then
                        syncContext.RaiseDelegateEvent trfFiltered (filtered)
                        syncContext.RaiseDelegateEvent trfCurrent (sum_of_charges-filtered)
                        syncContext.RaiseDelegateEvent trfCapacity trf_args.capacity
                    
                    return! loop (Transformer({ trf_args with filtered=filtered; current=sum_of_charges-filtered})) [] filter false
                else
                    let! (msg : Message) = 
                        if (not waiting && queue.Count > 0) then
                            async { return queue.Dequeue() }
                        else
                            agent.Receive()

                    match msg with
                    | Update(tick) ->
                        return! loop (Transformer({ trf_args with filtered=0.0<kWh>; current=0.0<kWh> })) charges filter  true
                    | ReplyTo(replyToMsg, reply) ->
                        match replyToMsg with
                        | RequestModel ->
                            if waiting then
                                queue.Enqueue(msg)
                                return! loop trf charges filter  waiting
                            else
                                reply.Reply(Model(trf))
                                return! loop trf [] filter  false
                    | Model(trf) -> 
                        return! loop trf charges filter  waiting
                    | Demand(from,energy,_) -> 
                        return! loop trf (msg :: charges) filter  waiting
                    | Reset ->
                        return! loop trf charges filter waiting
                    | Filter(filter) ->
                        return! loop trf charges filter waiting
                    | Kill ->
                        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Agent %s: Exiting.." name
                    | _ as test ->
                        raise (Exception((test.ToString())))
                        return! loop trf charges filter  waiting
            }
            loop trf [] true false)