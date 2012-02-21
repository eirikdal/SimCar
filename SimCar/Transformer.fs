module Transformer

open System
open System.Threading
open Message
open Agent
open Models
//open Node

type SynchronizationContext with 
    /// A standard helper extension method to raise an event on the GUI thread
    member syncContext.RaiseEvent (event: Event<_>) args = 
        syncContext.Post((fun _ -> event.Trigger args),state=null)

 

    /// A standard helper extension method to capture the current synchronization context.
    /// If none is present, use a context that executes work in the thread pool.
    static member CaptureCurrent () = 

        match SynchronizationContext.Current with 

        | null -> new SynchronizationContext()

        | ctxt -> ctxt

let jobCompleted = new Event<Agent<Message> * string>()
(* 
    Transformer: This is the transformer agent
*)
let syncContext = SynchronizationContext.Current

type SamplingAgent() = 
    member x.Start() = agent.Start()
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
                failwith "Tried to assign non-transformer model to transformer agent. Note to self: Implement post-and-reply"
        | Hello ->
            syncContext.RaiseEvent jobCompleted (agent, sprintf "Agent %s says 'Hello, World!'" name)
//            printfn "Agent %s says 'Hello, World!'" name
        | _ -> failwith "Not yet implemented"

        jobCompleted.Publish |> ignore

        return! loop name trf trf_list
    }
    
    loop name trf trf_list)
