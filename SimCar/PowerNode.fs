module PowerNode

open Agent
open SynchronizationContext
open Message
open System
open Models

let pnode_agent pnode = Agent.Start(fun agent ->
    let rec loop (PowerNode(pnode_args)) = async {
        let! msg = agent.Receive()

        match msg with
        | Hello -> 
            syncContext.RaiseEvent jobCompleted<_> (agent, sprintf "Agent %s says 'Hello, World!'" pnode_args.name)
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(pnode))
        | Update(tick) ->
            ()
        | _ -> 
            syncContext.RaiseEvent error <| Exception("Not implemented yet")

        return! loop pnode
    }

    loop pnode)