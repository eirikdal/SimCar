module Sim

open System.Drawing
open SimCar
open SynchronizationContext
open Message
open PostalService
open FileManager
open System
open MSDN.FSharp.Charting
open Models
open Tree

type Sim(num_iter, ticks_in_day) = 
    let _agents = to_agents powergrid
    member self.Agents = _agents |> Tree.map (fun (name, from) -> from)
        
    member self.RegisterProb (handler) = 
        probEvent.Publish.AddHandler handler

    member self.RegisterProbReset (handler) = 
        probReset.Publish.AddHandler handler

    member self.PostalService = postalService
    
    member self.RegisterProgress (handler) = 
        progress.Publish.AddHandler handler

    member self.RegisterDayaheadStep (handler) = 
        dayahead_step.Publish.AddHandler handler

    member self.RegisterDayaheadProgress (handler) =
        dayahead_progress.Publish.AddHandler handler

    member self.RegisterDayaheadInit (handler) =
        dayahead_init.Publish.AddHandler handler
    
    member self.RegisterEvents () = 
    // add what to do (as lambdas) with jobCompleted and error events
    //    jobCompleted<Message<string>>.Publish.Add(fun (agent, str) -> postalService.Post(Completed(sprintf "%s" str)))
        error.Publish.Add(fun e -> postalService.Post(Error(sprintf "%s" e.Message)))
    //    progress.Publish.Add(fun str -> printf "%s" str)
    //    phevEvent.Publish.Add(fun phev -> printfn "%s" phev) 
        
    member self.Init() = 
        postalService.agents <- _agents

    member self.Run() = 
        // create an infinite sequence of simulation steps
        let results = 
            Seq.initInfinite (fun day -> run day self.Agents)
            |> Seq.take num_iter
        // unzip results into lists
        let (phevs, pnodes) = results |> List.ofSeq |> List.unzip


        // fold over sequence of arrays, compute average, return functional composition of Seq.fold and Array.map
        let zeroArray n = (Array.zeroCreate<float<kWh>> n)
        let avg n = 
            Seq.fold (fun ac rt -> rt |> Array.map2 (fun ac1 ac2 -> ac1 + ac2) ac) (zeroArray n)
            >> Array.map (fun ac -> ac / (float num_iter))

        Array.ofList [avg 96 phevs;avg 96 pnodes]

    member self.Test_dayahead(num_iter) = 
        let (dayahead, _) as test = run 0 self.Agents
        
        let mutable test = dayahead
        for i in 0 .. num_iter do 
            test <- test |> DayAhead.shave
            dayahead_progress.Trigger([|box test; box System.EventArgs.Empty|])
        
        dayahead_step.Trigger([|box ([||] : float array); System.EventArgs.Empty|])

//[<EntryPoint>]
//let main args =
////    let realtime = 
////        FSharpChart.Combine 
////            [FSharpChart.Line(avg_phevs,Name="Average dayahead")
////             FSharpChart.Line(avg_pnodes,Name="Average realtime")]
////        |> FSharpChart.WithLegend()
//
////    let syncContext = System.Threading.SynchronizationContext.Current
//    
////    let default_font = 
////         new Font("Calibri", 9.0f, FontStyle.Regular)
////    realtime.Title <- StyleHelper.Title(sprintf "%d simulations" num_iter, Font=default_font)
////    realtime.Legend <- StyleHelper.Legend(InsideArea = false, Alignment = StringAlignment.Center, FontName = "Verbatim")
////
////    realtime.SaveChartAs(sprintf "theta-%f-alpha-%f-num_iter-%d.png" DayAhead.theta DayAhead.alpha num_iter, ChartImageFormat.Png)
//    
////    create_chart avg_phevs "Dayahead profile"
//    Console.ReadKey() |> ignore
//
//    0