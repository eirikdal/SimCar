module PowerNode

#nowarn "25"

open System.Collections
open Agent
open SynchronizationContext
open Message
open System
open Models
open PostalService

let pnode_agent pnode = Agent.Start(fun agent ->
    let queue = new Queue() 
    let rec loop (PowerNode({ name=name; parent=parent } as pnode_args) as pnode) waiting = async {
        let! (msg : Message) = 
            if (not waiting && queue.Count > 0) then
                async { return queue.Dequeue() :?> Message }
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
//            printfn "PowerNode %s: Sending charge_ok to %s" name parent
            return! loop <| pnode <| true
        | Model(pnode) -> 
            return! loop pnode waiting
        | Reset ->
            return! loop <| PowerNode({ pnode_args with current=0.0<kWh>}) <| false
        | Charge_OK(_,current,_) -> return! loop <| PowerNode({ pnode_args with current=current }) <| false
        | Kill ->
            printfn "Agent %s: Exiting.." name
        | _ -> 
            syncContext.RaiseEvent error <| Exception("Not implemented yet")
    }

    loop pnode false)