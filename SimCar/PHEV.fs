module PHEV

open Message
open Agent
open Models
//open Node

let phev_agent phev = Agent.Start(fun agent ->
    let rec loop (PHEV(_,name,_,_,_)) = async {
        let! msg = agent.Receive()
        
        match msg with 
        | Hello ->
            printfn "Hello from %s" name
        | Reset ->
            return! loop phev
    }
    
    loop phev)
