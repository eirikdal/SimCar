﻿module Sim

open System
//open System.Drawing
open SimCar
open SynchronizationContext
open Message
open PostalService
open FileManager
open Models
open Tree

type Method = 
    | Swarm
    | Shaving
    | Distribute

type Contribution =
    | Expected
    | Simulated

type SimCar(nIter, nTicksPerDayq) = 
    let _agents = to_agents <| powergrid()
    member self.Agents = _agents |> Tree.map (fun (name, from) -> from)
    
    member self.PostalService = postalService
   
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

    member self.ComputeDayahead(?days, ?dayahead, ?baseline) = 
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

        let realtime = [for i in 0 .. n*96 do yield calc_power i] |> Array.ofList

        printfn "Computing dayahead"
        
        let phev_contribution = 
             match baseline with
             | None | Some Expected -> 
                Tree.phev_expected
             | Some Simulated -> 
                [for i in 0 .. (n-1) do run i self.Agents true] |> ignore
                self.Agents |> Tree.send (Reset) |> ignore
                Array.init(n*96) (fun i -> FileManager.dayahead()(i))

        let dayahead = 
            match dayahead with
            | None | Some Shaving ->
// Pre-compute alternative:
                postalService.send("brp", Dayahead((fun _ -> 0.0<kWh>)))
                postalService.send("brp", Prediction((fun _ -> 0.0<kWh>)))
                postalService.send("brp", Schedule(BRP.Action.schedule_none))
                
                [|for i in 0 .. (n-1) do
                    let _from,_to = (i*96),(i*96)+96
                    let day = Array.sub realtime _from 96
                    let phev = 
                        match baseline with 
                        | None | Some Expected ->
                            Tree.phev_expected
                        | Some Simulated ->
                            Array.sub phev_contribution _from 96
            
                    let realtime_updated =                 
                        Array.sum2 phev day
                        |> DayAhead.shave 0.3 0.95
                    yield! realtime_updated|]
            | Some Swarm ->
// Swarm alternative:
                DayaheadSwarm.dayahead(realtime, n) 
            | Some Distributed ->
// Non-swarm alternative:
                DayaheadExp.Algorithm.distribute phev_contribution realtime 0.95 n |> Array.ofList
//        printfn "sum of dayahead %f" <| Array.sum dayahead
        postalService.send("brp", Dayahead(dayahead |> Array.get))
        postalService.send("brp", Prediction(realtime |> Array.get))

//        IO.write_doubles <| FileManager.file_prediction <| Parsing.parse_dayahead (List.ofArray pnodes)
//        IO.write_doubles <| FileManager.file_dayahead <| (dayahead |> List.ofArray |> List.map Energy.toFloat)

        PHEV.rand <- new System.Random()
        printfn "Dayahead computed"
        
    // create an infinite sequence of simulation steps
    member self.Run(?days) = 
        let n = match days with Some d -> d | None -> nIter


//        postalService.send("brp", Dayahead(FileManager.dayahead()))
//        postalService.send("brp", Prediction(FileManager.prediction()))
        postalService.send("brp", Schedule(BRP.Action.schedule_reactive))

        printfn "Running simulations"
        [for i in 0 .. (n-1) do run i self.Agents false] |> ignore 
        printfn "Finished simulations"
        kill self.Agents

    member self.TestDayahead(n) = 
        test_dayahead n self.Agents
