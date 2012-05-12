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
            let rec loop (Transformer({ name=name; parent=parent; children=children } as trf_args) as trf) (intentions : Message list) (charges : Message list) waiting = async {
                if intentions.Length >= children.Length && children.Length > charges.Length then
                    postalService.send(parent, Charge_Intentions(intentions))

                    return! loop trf [] charges true
                else if charges.Length >= children.Length then 
                    // propagate Charge_OK upwards to (potentially) senior trf agents
                    postalService.send(parent, Charge_OK(name,0.0<kWh>,-1))

                    let sum_of_charges = charges |> List.sumBy (fun (Charge_OK(_,energy,ttl)) -> energy)

                    let test =
                        children 
                        |> List.map (fun (child,_) -> charges |> List.exists (fun (Charge_OK(name,_,_)) -> child=name)) 
                        |> List.forall (fun x -> x)

                    let (rem, filtered) = Action.filter_charges trf_args charges

                    if name = "med_1" then
                        syncContext.RaiseDelegateEvent trfFiltered (filtered)
                        syncContext.RaiseDelegateEvent trfCurrent sum_of_charges
                        syncContext.RaiseDelegateEvent trfCapacity trf_args.capacity
                
                    return! loop trf [] [] false
                else
                    let! (msg : Message) = 
                        if (not waiting && queue.Count > 0) then
                            async { return queue.Dequeue() }
                        else
                            agent.Receive()

                    match msg with
                    | Update(tick) ->
                        return! loop trf intentions charges true
                    | ReplyTo(replyToMsg, reply) ->
                        match replyToMsg with
                        | RequestModel ->
                            if waiting then
                                queue.Enqueue(msg)
                                return! loop trf intentions charges waiting
                            else
                                reply.Reply(Model(trf))
                                return! loop trf [] [] false
                    | Charge(from,_,_,_) ->
                        return! loop trf (msg :: intentions) charges waiting
                    | Model(trf) -> 
                        return! loop trf intentions charges waiting
                    | Charge_OK(from,energy,_) -> 
                        return! loop trf (msg :: intentions) (msg :: charges) waiting
                    | Charge_Intentions(_) ->
                        return! loop trf (msg :: intentions) charges waiting
                    | Reset ->
                        return! loop trf intentions charges waiting
                    | Kill ->
                        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Agent %s: Exiting.." name
                    | _ as test ->
                        raise (Exception((test.ToString())))
                        return! loop trf intentions charges waiting
            }
            loop trf [] [] false)
    module Decentralized = 
        let create_trf_agent trf = Agent.Start(fun agent ->
            let queue = new Queue<Message>() 
            let rec loop (Transformer({ name=name; parent=parent; children=children } as trf_args) as trf) (charges : Message list) waiting = async {
                if charges.Length >= children.Length then 
                    // propagate Charge_OK upwards to (potentially) senior trf agents
                    postalService.send(parent, Charge_OK(name,0.0<kWh>,-1))

                    let sum_of_charges = charges |> List.sumBy (fun (Charge_OK(_,energy,ttl)) -> energy)

                    let (rem, filtered) = Action.filter_charges trf_args charges
                        
                    if name = "med_1" then
                        syncContext.RaiseDelegateEvent trfFiltered (filtered)
                        syncContext.RaiseDelegateEvent trfCurrent sum_of_charges
                        syncContext.RaiseDelegateEvent trfCapacity trf_args.capacity
                
                    return! loop trf [] false
                else
                    let! (msg : Message) = 
                        if (not waiting && queue.Count > 0) then
                            async { return queue.Dequeue() }
                        else
                            agent.Receive()

                    match msg with
                    | Update(tick) ->
                        return! loop trf charges true
                    | ReplyTo(replyToMsg, reply) ->
                        match replyToMsg with
                        | RequestModel ->
                            if waiting then
                                queue.Enqueue(msg)
                                return! loop trf charges waiting
                            else
                                reply.Reply(Model(trf))
                                return! loop trf [] false
                    | Model(trf) -> 
                        return! loop trf charges waiting
                    | Charge_OK(from,energy,_) -> 
                        return! loop trf (msg :: charges) waiting
                    | Reset ->
                        return! loop trf charges waiting
                    | Kill ->
                        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Agent %s: Exiting.." name
                    | _ as test ->
                        raise (Exception((test.ToString())))
                        return! loop trf charges waiting
            }
            loop trf [] false)