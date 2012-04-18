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
   
    member self.RegisterDayaheadAnt (handler) =
        dayaheadAnt.Publish.AddHandler handler

    member self.RegisterDayaheadSupervisor (handler) = 
        dayaheadSupervisor.Publish.AddHandler handler

    member self.RegisterDayaheadExpected (handler) = 
        dayaheadExpected.Publish.AddHandler handler
        
    member self.RegisterPhevBattery (handler) = 
        phevBattery.Publish.AddHandler handler

    member self.RegisterPhevStatus (handler) = 
        phevStatus.Publish.AddHandler handler

    member self.RegisterPhevFailed (handler) = 
        phevFailed.Publish.AddHandler handler

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

    member self.RegisterTrfCapacity (handler) = 
        trfCapacity.Publish.AddHandler handler

    member self.RegisterTrfCurrent (handler) = 
        trfCurrent.Publish.AddHandler handler

    member self.RegisterTrfFiltered (handler) = 
        trfFiltered.Publish.AddHandler handler

//    member self.RegisterComputeDayahead () = 
//        updateEvent.Publish.Add(fun dayahead -> IO.write_to_file <| FileManager.file_dayahead <| Parsing.parse_dayahead (List.ofArray dayahead))
//    
    // attach functions to events
    member self.RegisterEvents () = 
        error.Publish.Add(fun e -> postalService.Post(Error(sprintf "%s" e.Message)))
        //jobDebug.Publish.Add(fun str -> printfn "%s" str)

    member self.Init() = 
        IO.clear_screenshots()
        postalService.agents <- _agents

    member self.ComputeDayahead(?days) = 
        let n = match days with Some d -> d | None -> nIter

        IO.clear_dayahead_data()
        
//        self.RegisterComputeDayahead()
        
        let op i node = 
            match node with
            | Transformer(_) -> 0.0<kWh>
            | PHEV(_) -> 0.0<kWh>
            | PowerNode({ realtime=realtime }) -> realtime(i)
            | BRP(_) -> 0.0<kWh>

        let calc_power tick = 
            powergrid()
            |> Tree.foldl (op tick) (0.0<kWh>)

        let realtime = [for i in 0 .. n*96 do yield Energy.toFloat <| calc_power i] |> Array.ofList

        printfn "Computing dayahead"
// Pre-compute alternative:
        postalService.send("brp", Dayahead((fun _ -> 0.0<kWh>)))
        postalService.send("brp", Prediction((fun _ -> 0.0<kWh>)))
        postalService.send("brp", Schedule(BRP.Action.schedule_none))

        [for i in 0 .. (n-1) do run i self.Agents true] |> ignore
        self.Agents |> Tree.send (Reset) |> ignore
        let dayahead = Array.init(n*96) (fun i -> Energy.toFloat <| FileManager.prediction()(i))

// Swarm alternative:
//        let dayahead = DayaheadSwarm.dayahead(realtime, n) 

// Non-swarm alternative:
//        let dayahead = DayaheadExp.Algorithm.distribute realtime n |> Array.ofList
//        printfn "sum of dayahead %f" <| Array.sum dayahead
        postalService.send("brp", Dayahead(dayahead |> Array.get >> Energy.ofFloat))
        postalService.send("brp", Prediction(realtime |> Array.get >> Energy.ofFloat))

//        IO.write_doubles <| FileManager.file_prediction <| Parsing.parse_dayahead (List.ofArray pnodes)
        IO.write_doubles <| FileManager.file_dayahead <| (List.ofArray dayahead)

        PHEV.rand <- new System.Random()
        printfn "Dayahead computed"
        
    // create an infinite sequence of simulation steps
    member self.Run(?days) = 
        let n = match days with Some d -> d | None -> nIter


        postalService.send("brp", Dayahead(FileManager.dayahead()))
//        postalService.send("brp", Prediction(FileManager.prediction()))
        postalService.send("brp", Schedule(BRP.Action.schedule_reactive))

        printfn "Running simulations"
        [for i in 0 .. (n-1) do run i self.Agents false] |> ignore 
        printfn "Finished simulations"

    member self.TestDayahead(n) = 
        test_dayahead n self.Agents
