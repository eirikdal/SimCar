module PHEV

#nowarn "25"

open System
open System.Threading
open SynchronizationContext
open Message
open Agent
open Models
//open Node

//let syncContext = SynchronizationContext.CaptureCurrent()

(* 
    PHEV: This is the PHEV agent
*)
let phev_agent phev = Agent.Start(fun agent ->
    let rec loop (PHEV(name,capacity,current,battery)) = async {
        let! msg = agent.Receive()
        
        match msg with
        | Hello -> 
            syncContext.RaiseEvent jobCompleted<_> (agent, sprintf "Agent %s says 'Hello, World!'" name)
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(phev))
        | _ -> 
            syncContext.RaiseEvent error <| Exception("Not implemented yet")
        
        return! loop phev
    }
    
    

    loop phev)
