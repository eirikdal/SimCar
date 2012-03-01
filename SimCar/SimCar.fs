// Learn more about F# at http://fsharp.net

open Agent
open DayAhead
open System
open System.Threading
open System.IO
open SynchronizationContext
open Models
open Message
open PHEV
open PostalService
open FileManager
open Transformer
open MSDN.FSharp
open System.Windows

#nowarn "25"

let postalService = new PostalService()

let print_grid message =
    match message with 
    | Model(gridnode) -> 
        postalService.Post(Completed(sprintf "Received node %s" gridnode.name))
        Model(gridnode)

let op tick (Model(grid)) = 
    match grid with 
    | Transformer(trf_args,_) ->
//        trf_args.current
        Current.ofFloat 0.0
    | PHEV(phev_args) ->
        phev_args.current
    | BRP(brp_args,_) ->
        Current.ofFloat 0.0
    | PowerNode(pnode_args) ->
        pnode_args.realtime (tick)

let run day agents =
    let sum_of_realtime tick = 
        agents
        |> Tree.send (Update(tick))
        |> Tree.send_and_reply RequestModel
        |> Tree.map (fun (ag, msg) -> msg)
        |> Tree.foldf (op tick) 0.0<kW*h>

    let realtime = Array.init(96) (fun i -> sum_of_realtime i)
//    
    let dayahead = 
        realtime
        |> scan

//    let sum = 
//        realtime 
//        |> Array.fold (fun ac rt -> ac + Current.toFloat rt) 0.0

    realtime


[<STAThread>]
[<EntryPoint>]
let main args = 
    // add what to do (as lambdas) with jobCompleted and error events
    jobCompleted<Message<string>>.Publish.Add(fun (agent, str) -> postalService.Post(Completed(sprintf "%s" str)))
    error.Publish.Add(fun e -> postalService.Post(Error(sprintf "%s" e.Message)))
    progress.Publish.Add(fun str -> printf "%s" str)

    // make agent tree from model tree (powergrid : Grid list, make_agents : Node<Agent> seq)
    let agents = to_agents powergrid

    // add agents to postalservice
    Tree.iter postalService.add_agent agents

    // send RequestModel message to agents
    let responses = Tree.send RequestModel agents

    postalService.send_to_all(Hello)
    
    let num_iter = 1
//    Tree.iter print_grid responses
    do printf "Running 7 iterations\n"
    let results = 
        Seq.initInfinite (fun day -> run day agents)
        |> Seq.take num_iter
    do printfn "Finished 7 iterations\n"

    // fold over sequence of arrays, compute average
    let avg_realtime = 
        results
        |> Seq.fold (fun ac rt -> 
            rt 
            |> Array.map2 (fun ac1 ac2 -> ac1 + ac2) ac) (Array.init 96 (fun x -> 0.0<kW*h>))
        |> Array.map (fun ac -> ac / (float num_iter))

    let area = Charting.FSharpChart.SplineArea avg_realtime
    let formsHost = new Forms.Integration.WindowsFormsHost(Child = new Charting.ChartControl(area))
    let graphWindow = new Window(Content = formsHost, Title = "Average realtime consumption")
    let wpfApp = new System.Windows.Application()
    wpfApp.Run(graphWindow) |> ignore
    Console.ReadKey() |> ignore

    0