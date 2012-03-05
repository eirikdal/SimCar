module main

open SimCar
open SynchronizationContext
open Message
open PostalService
open FileManager
open System
open MSDN.FSharp.Charting
open Models

[<STAThread>]
[<EntryPoint>]
let main args =
//    let chart1 = Array.zeroCreate<float<kWh>>(96) |> FSharpChart.Line
//    do updateEvent.Publish.Add(fun array -> chart1.Series) 
    // add what to do (as lambdas) with jobCompleted and error events
    jobCompleted<Message<string>>.Publish.Add(fun (agent, str) -> postalService.Post(Completed(sprintf "%s" str)))
    error.Publish.Add(fun e -> postalService.Post(Error(sprintf "%s" e.Message)))
    progress.Publish.Add(fun str -> printf "%s" str)
    phevEvent.Publish.Add(fun phev -> printfn "%s" phev)
    // make agent tree from model tree (powergrid : Grid list, make_agents : Node<Agent> seq)
    let agents = to_agents powergrid

    // add agents to postalservice
    Tree.iter postalService.add_agent agents

    // send RequestModel message to agents
    let responses = Tree.send RequestModel agents
    
    let num_iter = 10
    let ticks_in_day = 96

    // create an infinite sequence of simulation steps
    let results = 
        Seq.initInfinite (fun day -> run day agents)
        |> Seq.take num_iter
    
    // unzip results into lists
    let (phevs, pnodes) = results |> List.ofSeq |> List.unzip

    // fold over sequence of arrays, compute average, return functional composition of Seq.fold and Array.map
    let zeroArray n = (Array.zeroCreate<float<kWh>> n)
    let avg n = 
        Seq.fold (fun ac rt -> rt |> Array.map2 (fun ac1 ac2 -> ac1 + ac2) ac) (zeroArray n)
        >> Array.map (fun ac -> ac / (float num_iter))

    let avg_phevs = avg 96 phevs
    let avg_pnodes = avg 96 pnodes

    let realtime = FSharpChart.Combine [FSharpChart.Line avg_phevs 
                                        FSharpChart.Line avg_pnodes]

//    let chart2 = realtime |> FSharpChart.Create

//    FSharpChart.Line avg_phevs |> FSharpChart.Create
//    let syncContext = System.Threading.SynchronizationContext.Current

    create_chart realtime "Average realtime consumption"
//    create_chart avg_area_of_dayahead "Dayahead profile"

    do Console.ReadKey() |> ignore

    0