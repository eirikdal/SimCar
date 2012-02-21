// Learn more about F# at http://fsharp.net

open Agent
open System
open System.Threading
open System.IO
open SynchronizationContext
open Models
open Message
open PHEV
open ComManager
open FileManager
open Transformer

let rec run tick =
//    update models

    run <| tick+1

[<EntryPoint>]
let main args = 
    let postalService = new PostalService()

    let make_agent node = 
        match node with
        | Transformer(_,_,_,_) ->
            trf_agent node
        | PHEV(_,_,_,_) ->
            phev_agent node

    let rec traverseTree node : seq<Agent<Message>> = 
        let agent = make_agent node

        let agents = 
            match node with 
            | Transformer(_,nodes,_,_) ->
                nodes |> Seq.fold (fun ac n -> Seq.append (traverseTree n) ac) Seq.empty
            | _ -> Seq.empty

        Seq.append [agent] agents
    
    powergrid_tree()
    |> Seq.map (fun node -> traverseTree node)
    |> Seq.iter (fun agents -> Seq.iter (fun agent -> postalService.add_agent(agent)) agents)

    jobCompleted.Publish.Add(fun (agent, str) -> postalService.Post(Completed(sprintf "%s" str)))

    postalService.send_to_all(Hello)

    run 0