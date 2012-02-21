module Transformer

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
let syncContext = SynchronizationContext.CaptureCurrent()

let trf_agent name trf trf_list = Agent.Start(fun agent ->
    let rec loop name trf trf_list = async {
        let! msg = agent.Receive()
        
        match msg with
        | Assign(from_agent, assign_type) ->
            match assign_type with 
            | Transformer(trf) -> 
                syncContext.RaiseEvent jobCompleted (agent, sprintf "Assigned transformer %s to agent %s" "test" "test")
                return! loop name trf <| Seq.append trf_list [trf]
            | _ -> 
                syncContext.RaiseEvent error <| Exception("Tried to assign non-transformer model to transformer agent. Note to self: Implement post-and-reply")
        | Hello ->
            syncContext.RaiseEvent jobCompleted (agent, sprintf "Agent %s says 'Hello, World!'" name)
        | _ -> 
            syncContext.RaiseEvent error <| Exception("Not yet implemented")

        jobCompleted.Publish |> ignore

        return! loop name trf trf_list
    }
    
    loop name trf trf_list)
