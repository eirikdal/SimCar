module PowerNode

#nowarn "25"

open System
open System.Collections.Generic
open Agent
open SynchronizationContext
open Message
open Models
open PostalService

module Agent = 
    module Centralized =       
        let create_pnode_agent pnode = Agent.Start(fun agent ->
            let queue = new Queue<Message>() 
            let rec loop (PowerNode({ name=name; parent=parent } as pnode_args) as pnode) waiting = async {
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
                            return! loop pnode waiting
                        else
                            reply.Reply(Model(pnode))
                            return! loop pnode false
                | Update(tick) -> 
                    let current = pnode_args.realtime tick
                    postalService.send(parent, Charge_OK(name, current, -1))
        //            syncContext.RaiseDelegateEvent jobProgress <|  "PowerNode %s: Sending charge_ok to %s" name parent
                    return! loop <| pnode <| true
                | Model(pnode) -> 
                    return! loop pnode waiting
                | Reset ->
                    return! loop <| PowerNode({ pnode_args with current=0.0<kWh>}) <| false
                | Charge_OK(_,current,_) -> 
                    return! loop <| PowerNode({ pnode_args with current=current }) <| false
                | Kill ->
                    syncContext.RaiseDelegateEvent jobProgress <| sprintf "Agent %s: Exiting.." name
                | _ -> 
                    return! loop pnode waiting
            }

            loop pnode false)
    module Decentralized = 
        let create_pnode_agent pnode = Agent.Start(fun agent ->
            let queue = new Queue<Message>() 
            let rec loop (PowerNode({ name=name; parent=parent } as pnode_args) as pnode) waiting = async {
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
                            return! loop pnode waiting
                        else
                            reply.Reply(Model(pnode))
                            return! loop pnode false
                | Update(tick) -> 
                    let current = pnode_args.realtime tick
                    postalService.send(parent, Charge_OK(name, current, -1))
//                    syncContext.RaiseDelegateEvent jobProgress <|  "PowerNode %s: Sending charge_ok to %s" name parent
                    return! loop <| pnode <| true
                | Model(pnode) -> 
                    return! loop pnode waiting
                | Reset ->
                    return! loop <| PowerNode({ pnode_args with current=0.0<kWh>}) <| false
                | Charge_OK(_,current,_) -> 
                    return! loop <| PowerNode({ pnode_args with current=current }) <| false
                | Kill ->
                    syncContext.RaiseDelegateEvent jobProgress <| sprintf "Agent %s: Exiting.." name
                | _ -> 
                    return! loop pnode waiting
            }

            loop pnode false)