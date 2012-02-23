module PowerNode

open Agent
open SynchronizationContext
open Message
open System
open Models

let pnode_agent pnode = Agent.Start(fun agent ->
    let rec loop (PowerNode(name,_,_)) = async {
        let! msg = agent.Receive()

        match msg with
        | Hello -> 
            syncContext.RaiseEvent jobCompleted (agent, sprintf "Agent %s says 'Hello, World!'" name)
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(pnode))
        | _ -> 
            syncContext.RaiseEvent error <| Exception("Not implemented yet")

        return! loop pnode
    }

    loop pnode)