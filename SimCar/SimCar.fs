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
open Tree

let rec run tick =
//    update models

    run <| tick+1

[<EntryPoint>]
let main args = 
    let postalService = new PostalService()

    let test str = 
        postalService.Post(Completed(sprintf "%s" str))
    // add what to do (as lambdas) with jobCompleted and error events
    jobCompleted.Publish.Add(fun (agent, str) -> test str)
    error.Publish.Add(fun e -> postalService.Post(Error(sprintf "%s" e.Message)))

    // make agent tree from model tree (powergrid : Grid list, make_agents : Node<Agent> seq)
    let make_agents = Seq.map (fun n -> to_agents n) powergrid

    // add agents to postalservice
    Seq.iter (fun n -> iterTree n postalService.add_agent) make_agents

    // send RequestModel message to agents
    let responses = Seq.map (fun n -> mapAgents n RequestModel) make_agents

    let print_grid message =
        match message with 
        | Model(gridnode) -> 
            match gridnode with 
            | Transformer(name,_,_,_) ->
                postalService.Post(Completed(sprintf "Received node %s" name))
            | PHEV(name,_,_,_) ->
                postalService.Post(Completed(sprintf "Received node %s" name))
            | PowerNode(name,_,_) ->
                postalService.Post(Completed(sprintf "Received node %s" name))

    postalService.send_to_all(Hello)
    
    Seq.iter (fun n -> iterTree n print_grid) responses

    run 0