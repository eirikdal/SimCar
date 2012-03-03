﻿module BRP

#nowarn "25"

open Agent
open Message
open Models
open System
open System.Threading
open SynchronizationContext

let brp_agent brp = Agent.Start(fun agent ->
    let rec loop (BRP(brp_args)) = async {
        let! msg = agent.Receive()

        match msg with
        | Hello -> 
            syncContext.RaiseEvent jobCompleted<_> (agent, sprintf "Agent %s says 'Hello, World!'" brp_args.name)
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(brp))
        | Update(tick) -> 
            ()
        | Model(brp) -> return! loop brp
        | _ -> 
            syncContext.RaiseEvent error <| Exception("Not implemented yet")

        return! loop brp
    }

    loop brp)