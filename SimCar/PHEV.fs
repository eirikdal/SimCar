module PHEV

open Message
open Agent
open Models
//open Node

(* 
    PHEV: This is the PHEV agent
*)
let phev_agent phev = Agent.Start(fun agent ->
    let rec loop (PHEV(_,name,_,_,_)) = async {
        let! msg = agent.Receive()
        
//        match msg with 
//        | Reset ->
//            return! loop phev
        ()
    }
    
    

    loop phev)
