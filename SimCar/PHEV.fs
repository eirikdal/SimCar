module PHEV

open Message
open Agent
open Models
//open Node

(* 
    PHEV: This is the PHEV agent
*)
let phev_agent phev = Agent.Start(fun agent ->
    let rec loop (PHEV(name,_,_,_,_)) = async {
        let! msg = agent.Receive()
        
        match msg with
        | Hello -> 
            printfn "Agent %s says 'Hello, World!'" name
        | _ -> failwith "Not implemented yet"
        
        return! loop phev
    }
    
    

    loop phev)
