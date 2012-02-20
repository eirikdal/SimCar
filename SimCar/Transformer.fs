module Transformer

open Message
open Agent
open Models
//open Node

(* 
    Transformer: This is the transformer agent
*)
let trf_agent trf = Agent.Start(fun agent ->
    let rec loop trf = async {
        let! msg = agent.Receive()
        
//        match msg with 
//        | Reset ->
//            return! loop phev
        ()
    }
    
    

    loop trf)
