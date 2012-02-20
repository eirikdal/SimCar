module Transformer

open System
open Message
open Agent
open Models
//open Node

(* 
    Transformer: This is the transformer agent
*)
let trf_agent name trf trf_list = Agent.Start(fun agent ->
    let rec loop name trf trf_list = async {
        let! msg = agent.Receive()
        
        match msg with
        | Assign(from_agent, assign_type) ->
            match assign_type with 
            | Transformer(trf) -> 
                printfn "Assigned transformer %s to agent %s" "test" "test"
                return! loop name trf <| Seq.append trf_list [trf]
            | _ ->
                failwith "Tried to assign non-transformer model to transformer agent. Note to self: Implement post-and-reply"
        | Hello -> 
            printfn "Agent %s says 'Hello, World!'" name
        | _ -> failwith "Not yet implemented"
    }
    
    loop name trf trf_list)
