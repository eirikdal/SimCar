﻿// Learn more about F# at http://fsharp.net

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

let rec run tick agents =
//    update models
    agents 
    |> Seq.map (fun node -> Tree.send node RequestModel)
    |> Seq.collect (fun node -> Tree.collect node) 
    |> List.ofSeq
    |> ignore

    run (tick+1) agents

[<EntryPoint>]
let main args = 
    let postalService = new PostalService()

    let test str = 
        postalService.Post(Completed(sprintf "%s" str))
    // add what to do (as lambdas) with jobCompleted and error events
    jobCompleted<unit>.Publish.Add(fun (agent, str) -> test str)
    error.Publish.Add(fun e -> postalService.Post(Error(sprintf "%s" e.Message)))

    // make agent tree from model tree (powergrid : Grid list, make_agents : Node<Agent> seq)
    let agents = Seq.map (fun n -> Tree.to_agents n) powergrid

    // add agents to postalservice
    Seq.iter (fun n -> Tree.iter n postalService.add_agent) agents

    // send RequestModel message to agents
    let responses = Seq.map (fun n -> Tree.send n RequestModel) agents

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
    
    Seq.iter (fun n -> Tree.iter n print_grid) responses

    run 0 agents