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
    
    let filter_charges trf_args charges = 
        charges 
        |> List.sortBy (fun (Charge_OK(_,_,ttl)) -> ttl)
        |> List.fold (fun (rem,filtered) (Charge_OK(name,energy,ttl)) ->     
            if not (name.StartsWith("med") || name.StartsWith("high")) then
                let filter, remaining = filter energy rem
                postalService.send(name, Charge_OK(name, filter, ttl))
                remaining, filtered+(energy-filter)
            else 
                rem,filtered) (trf_args.capacity, 0.0<kWh>)
    
module Agent = 
    module Centralized = 
        let create_trf_agent trf = Agent.Start(fun agent ->
            let queue = new Queue<Message>() 
            let rec loop (Transformer({ name=name; parent=parent; children=children } as trf_args) as trf) (intentions : Message list) (charges : Message list) filter waiting = async {
                if intentions.Length >= children.Length && children.Length > charges.Length then
                    postalService.send(parent, Charge_Intentions(intentions))

                    return! loop trf [] charges filter true
                else if charges.Length >= children.Length then 
                    // propagate Charge_OK upwards to (potentially) senior trf agents
                    postalService.send(parent, Charge_OK(name,0.0<kWh>,-1))

                    let sum_of_charges = charges |> List.sumBy (fun (Charge_OK(_,energy,ttl)) -> energy)

                    let (rem, filtered) = 
                        if filter then 
                            Action.filter_charges trf_args charges
                        else
                            charges |> List.iter (fun (Charge_OK(name,energy,ttl)) -> 
                                if not (name.StartsWith("med") || name.StartsWith("high")) then
                                    postalService.send(name, Charge_OK(name,energy,ttl)))
                            (trf_args.capacity, 0.0<kWh>)

                    if name = "med_3" then
                        syncContext.RaiseDelegateEvent trfFiltered (filtered)
                        syncContext.RaiseDelegateEvent trfCurrent sum_of_charges
                        syncContext.RaiseDelegateEvent trfCapacity trf_args.capacity
                
                    return! loop (Transformer({ trf_args with filtered=filtered; current=sum_of_charges })) [] [] filter false
                else
                    let! (msg : Message) = 
                        if (not waiting && queue.Count > 0) then
                            async { return queue.Dequeue() }
                        else
                            agent.Receive()

                    match msg with
                    | Update(tick) ->
                        return! loop (Transformer({ trf_args with filtered=0.0<kWh>; current=0.0<kWh> })) intentions charges filter true
                    | ReplyTo(replyToMsg, reply) ->
                        match replyToMsg with
                        | RequestModel ->
                            if waiting then
                                queue.Enqueue(msg)
                                return! loop trf intentions charges filter waiting
                            else
                                reply.Reply(Model(trf))
                                return! loop trf [] [] filter false
                    | Charge(from,_,_,_) ->
                        return! loop trf (msg :: intentions) charges filter waiting
                    | Model(trf) -> 
                        return! loop trf intentions charges filter waiting
                    | Charge_OK(from,energy,_) -> 
                        return! loop trf (msg :: intentions) (msg :: charges) filter waiting
                    | Charge_Intentions(_) ->
                        return! loop trf (msg :: intentions) charges filter waiting
                    | Reset ->
                        return! loop trf intentions charges filter waiting
                    | Filter(filter) ->
                        return! loop trf intentions charges filter waiting
                    | Kill ->
                        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Agent %s: Exiting.." name
                    | _ as test ->
                        syncContext.RaiseDelegateEvent jobError (Exception((test.ToString())))
                        return! loop trf intentions charges filter waiting
            }
            loop trf [] [] true false)
    module Decentralized = 
        let create_trf_agent trf = Agent.Start(fun agent ->
            let queue = new Queue<Message>() 
            let rec loop (Transformer({ name=name; parent=parent; children=children } as trf_args) as trf) (charges : Message list) filter waiting = async {
                if charges.Length >= children.Length then 
                    // propagate Charge_OK upwards to (potentially) senior trf agents
                    postalService.send(parent, Charge_OK(name,0.0<kWh>,-1))

                    let sum_of_charges = charges |> List.sumBy (fun (Charge_OK(_,energy,ttl)) -> energy)

                    let (rem, filtered) = 
                        if filter then 
                            Action.filter_charges trf_args charges
                        else
                            charges |> List.iter (fun (Charge_OK(name,energy,ttl)) -> 
                                if not (name.StartsWith("med") || name.StartsWith("high")) then
                                    postalService.send(name, Charge_OK(name,energy,ttl)))
                            (trf_args.capacity, 0.0<kWh>)

                    if name = "med_3" then
                        syncContext.RaiseDelegateEvent trfFiltered (filtered)
                        syncContext.RaiseDelegateEvent trfCurrent sum_of_charges
                        syncContext.RaiseDelegateEvent trfCapacity trf_args.capacity
                    
                    return! loop (Transformer({ trf_args with filtered=filtered; current=sum_of_charges})) [] filter false
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
                    | Charge_OK(from,energy,_) -> 
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