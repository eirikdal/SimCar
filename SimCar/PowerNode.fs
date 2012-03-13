module PowerNode

#nowarn "25"

open Agent
open SynchronizationContext
open Message
open System
open Models
open PostalService

let pnode_agent pnode = Agent.Start(fun agent ->
    let rec loop (PowerNode({ parent=parent } as pnode_args) as pnode) = async {
        let! msg = agent.Receive()

        match msg with
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(pnode))
        | Update(tick) -> 
            let current = pnode_args.realtime tick
            postalService.send(parent, Charge_OK)
            return! loop <| PowerNode({ pnode_args with current=current })
        | Model(pnode) -> 
            return! loop pnode
        | _ -> 
            syncContext.RaiseEvent error <| Exception("Not implemented yet")

        return! loop pnode
    }

    loop pnode)