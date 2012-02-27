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

let postalService = new PostalService()

let print_grid message =
    match message with 
    | Model(gridnode) -> 
        postalService.Post(Completed(sprintf "Received node %s" gridnode.name))
        Model(gridnode)

let op (Model(grid)) = 
    match grid with 
    | Transformer(trf_args,_) ->
        trf_args.current
    | PHEV(phev_args) ->
        phev_args.current
    | BRP(brp_args,_) ->
        Current.ofFloat 0.0
    | PowerNode(pnode_args) ->
        pnode_args.realtime 0

let rec run tick agents =
    let sum_of_currents = 
        agents
        |> Tree.send (Update(tick))
        |> Tree.send_and_reply RequestModel
        |> Tree.map (fun (ag, msg) -> msg)
        |> Tree.foldf op 0.0<kW*h>

    //printfn "Tick %d - Sum of currents: %f\n" tick (Current.toFloat sum_of_currents)
    
    run (tick+1) agents

[<EntryPoint>]
let main args = 
    // add what to do (as lambdas) with jobCompleted and error events
    jobCompleted<Message<string>>.Publish.Add(fun (agent, str) -> postalService.Post(Completed(sprintf "%s" str)))
    error.Publish.Add(fun e -> postalService.Post(Error(sprintf "%s" e.Message)))
    progress.Publish.Add(fun str -> printf "%s" str)

    // make agent tree from model tree (powergrid : Grid list, make_agents : Node<Agent> seq)
    let agents = Tree.to_agents powergrid

    // add agents to postalservice
    Tree.iter postalService.add_agent agents

    // send RequestModel message to agents
    let responses = Tree.send RequestModel agents

    postalService.send_to_all(Hello)
    
//    Tree.iter print_grid responses
    do printf "Running 10000 iterations\n"
    run 0 agents
    do printfn "Finished 10000 iterations\n"
    Console.ReadKey() |> ignore

    0