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

// create chart from data
let create_chart data title = 
    let formsHost = new Forms.Integration.WindowsFormsHost(Child = new Charting.ChartControl(data))
    let graphWindow = new Window(Content = formsHost, Title = title)
    let wpfApp = new System.Windows.Application()
    wpfApp.Run(graphWindow) |> ignore

// for testing purposes
let print_grid message =
    match message with 
    | Model(gridnode) -> 
        postalService.Post(Completed(sprintf "Received node %s" gridnode.name))
        Model(gridnode)

// for testing purposes
let op tick (Model(grid)) = 
    match grid with 
    | Transformer(trf_args,_) ->
        Current.ofFloat 0.0
    | PHEV(phev_args) ->
        phev_args.current
    | BRP(brp_args,_) ->
        Current.ofFloat 0.0
    | PowerNode(pnode_args) ->
        pnode_args.realtime (tick)

// main control flow of the simulator
let run day agents =
    let sum_of_realtime tick = 
        agents
        |> Tree.send (Update(tick))
        |> Tree.send_and_reply RequestModel
        |> Tree.map (fun (ag, msg) -> msg)
        |> Tree.foldf (op tick) 0.0<kW*h>

    let realtime = Array.init(96) (fun i -> sum_of_realtime i)
  
    let dayahead = 
        realtime
        |> scan

    let sum_realtime = 
        realtime 
        |> Array.fold (fun ac rt -> ac + Current.toFloat rt) 0.0

    let sum_dayahead = 
        dayahead
        |> Array.fold (fun ac d -> ac + Current.toFloat d) 0.0

    printf "Sum realtime: %f\n" sum_realtime
    printf "Sum dayahead: %f\n" sum_dayahead

    (dayahead, realtime)

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
    let ticks_in_day = 96

    do printf "Running %d iterations with %d ticks per day\n" num_iter ticks_in_day
    let results = 
        Seq.initInfinite (fun day -> run day agents)
        |> Seq.take num_iter
    do printfn "Finished %d iterations with %d ticks per day\n" num_iter ticks_in_day
    
    // unzip results into lists
    let (dayahead, realtime) = results |> List.ofSeq |> List.unzip

    // fold over sequence of arrays, compute average
    let avg = 
        Seq.fold (fun ac rt -> 
            rt |> Array.map2 (fun ac1 ac2 -> ac1 + ac2) ac) (Array.zeroCreate<energy> ticks_in_day)
        >> Array.map (fun ac -> ac / (float num_iter))

    let avg_dayahead = avg dayahead
    let avg_realtime = avg realtime

    let avg_area_of_realtime = Charting.FSharpChart.Area avg_realtime
    let avg_area_of_dayahead = Charting.FSharpChart.Area avg_dayahead

    let syncContext = System.Threading.SynchronizationContext.Current

//    create_chart avg_area_of_realtime "Average realtime consumption"
    create_chart avg_area_of_dayahead "Dayahead profile"

    Console.ReadKey() |> ignore

    0