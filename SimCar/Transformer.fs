﻿module Transformer

#nowarn "25"

open System
open System.Threading
open SynchronizationContext
open Message
open Agent
open Models
//open Node
(* 
    Transformer: This is the transformer agent
*)
//let syncContext = SynchronizationContext.CaptureCurrent()

let trf_agent trf = Agent.Start(fun agent ->
    let rec loop (Transformer(trf_args,nodes)) = async {
        let! msg = agent.Receive()
        
        match msg with
        | Hello ->
            syncContext.RaiseEvent jobCompleted<_> (agent, sprintf "Agent %s says 'Hello, World!'" trf_args.name)
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(trf))
        | Update(tick) ->
            ()
        | _ -> 
            syncContext.RaiseEvent error <| Exception("Not yet implemented")

        jobCompleted.Publish |> ignore

        return! loop trf
    }
    
    loop trf)
