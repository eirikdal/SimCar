module Sim

open System.Drawing
open SimCar
open SynchronizationContext
open Message
open PostalService
open FileManager
open System
open Models
open Tree

type SimCar(nIter, nTicksPerDayq) = 
    let _agents = to_agents <| powergrid()
    member self.Agents = _agents |> Tree.map (fun (name, from) -> from)
        
    member self.RegisterPhevBattery (handler) = 
        phevBattery.Publish.AddHandler handler

    member self.RegisterPhevStatus (handler) = 
        phevStatus.Publish.AddHandler handler

    member self.RegisterProb (handler) = 
        probEvent.Publish.AddHandler handler

    member self.RegisterProbReset (handler) = 
        probReset.Publish.AddHandler handler

    member self.PostalService = postalService
    
    member self.RegisterProgressTotal (handler) = 
        progressTotal.Publish.AddHandler handler

    member self.RegisterProgressPhev (handler) = 
        progressPhev.Publish.AddHandler handler

    member self.RegisterProgressPnode (handler) = 
        progressPnode.Publish.AddHandler handler

    member self.RegisterDayaheadStep (handler) = 
        dayaheadStep.Publish.AddHandler handler

    member self.RegisterDayaheadProgress (handler) =
        dayaheadProgress.Publish.AddHandler handler

    member self.RegisterDayaheadInit (handler) =
        dayaheadInit.Publish.AddHandler handler

//    member self.RegisterComputeDayahead () = 
//        updateEvent.Publish.Add(fun dayahead -> IO.write_to_file <| FileManager.file_dayahead <| Parsing.parse_dayahead (List.ofArray dayahead))
//    
    // attach functions to events
    member self.RegisterEvents () = 
        error.Publish.Add(fun e -> postalService.Post(Error(sprintf "%s" e.Message)))
        //jobDebug.Publish.Add(fun str -> printfn "%s" str)

    member self.Init() = 
        postalService.agents <- _agents

    member self.ComputeDayahead(?days) = 
        let n = match days with Some d -> d | None -> nIter

        IO.clear_dayahead_data()
//        self.RegisterComputeDayahead()

        postalService.send("brp", Dayahead((fun _ -> 0.0<kWh>)))
        postalService.send("brp", Prediction((fun _ -> 0.0<kWh>)))
        postalService.send("brp", Schedule(BRP.Action.schedule_none))

        printfn "Computing dayahead"
//        syncContext.RaiseEvent jobDebug <| "Computing dayahead"
        [for i in 0 .. (n-1) do run i self.Agents true] |> ignore
//
//        Seq.initInfinite (fun day -> run day self.Agents true)
//        |> Seq.take n 
//        |> Seq.cache
//        |> List.ofSeq
//        |> ignore
//        syncContext.RaiseEvent jobDebug <| "Dayahead computed"
//        self.Agents |> Tree.send (Reset) |> ignore
        
        PHEV.rand <- new System.Random()
        printfn "Dayahead computed"
        
    // create an infinite sequence of simulation steps
    member self.Run(?days) = 
        let n = match days with Some d -> d | None -> nIter

        postalService.send("brp", Dayahead(FileManager.dayahead()))
        postalService.send("brp", Prediction(FileManager.prediction()))
        postalService.send("brp", Schedule(BRP.Action.schedule_reactive))

        printfn "Running simulations"
        [for i in 0 .. (n-1) do run i self.Agents false] |> ignore 
//        Seq.initInfinite (fun day -> run day self.Agents false)
//        |> Seq.take n
//        |> Seq.cache
//        |> List.ofSeq
//        |> ignore
        printfn "Finished simulations"

    member self.TestDayahead(n) = 
        test_dayahead n self.Agents
