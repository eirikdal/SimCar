module PHEV

open System
open System.Threading
open SynchronizationContext
open Message
open Agent
open Models
//open Node

let syncContext = SynchronizationContext.CaptureCurrent()

(* 
    PHEV: This is the PHEV agent
*)
let phev_agent phev = Agent.Start(fun agent ->
    let rec loop (PHEV(name,_,_,_)) = async {
        let! msg = agent.Receive()
        
        match msg with
        | Hello -> 
            syncContext.RaiseEvent jobCompleted (agent, sprintf "Agent %s says 'Hello, World!'" name)
//            printfn "Agent %s says 'Hello, World!'" name
        | _ -> syncContext.RaiseEvent error <| Exception("Not implemented yet")
        
        return! loop phev
    }
    
    

    loop phev)
