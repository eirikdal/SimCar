module Sim

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
    | Distance
    | Mixed
    | Random

type Contribution =
    | Expected
    | Simulated

type Scheduler = 
    | Proactive 
    | Reactive
    | Random
    | Mixed

type SimCar(nIter, nTicksPerDay, ?scheduler, ?ttlwindow) =
    let mutable distanceTheta = 1.0
    let mutable shavingTheta = 0.99
    let mutable shavingAlpha = 0.2
    let mutable mixedwindow = 40
    let ttlwindow = match ttlwindow with Some ttl -> ttl | None -> 76
    let agents = 
        match scheduler with
        | Some Reactive -> 
            Grid.Centralized.make_tree <| powergrid() <| BRP.Action.schedule_reactive <| ttlwindow
        | Some Proactive -> 
            Grid.Centralized.make_tree <| powergrid() <| BRP.Action.schedule_proactive <| ttlwindow
        | Some Mixed -> 
            Grid.Decentralized.Mixed.make_tree <| powergrid() <| ttlwindow <| mixedwindow
        | Some Random ->
            Grid.Decentralized.Random.make_tree <| powergrid() <| ttlwindow
        | None ->
            Grid.Centralized.make_tree <| powergrid() <| BRP.Action.schedule_none <| ttlwindow

    member self.DistanceTheta with set(theta) = distanceTheta <- theta
    member self.ShavingTheta with set(theta) = shavingTheta <- theta
    member self.ShavingAlpha with set(alpha) = shavingAlpha <- alpha
    member self.MixedWindow with set(window) = mixedwindow <- window

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
                [for i in 0 .. (n-1) do run i agents true] |> ignore
                agents |> Tree.send (Reset) |> ignore
                Array.init(n*96) (fun i -> FileManager.dayahead()(i))

        let dayahead = 
            match dayahead with
            | None | Some Method.Shaving ->
// Pre-compute alternative:
                postalService.send("brp", Dayahead((fun _ -> 0.0<kWh>)))
                postalService.send("brp", Prediction((fun _ -> 0.0<kWh>)))
//                postalService.send("brp", Schedule(BRP.Action.schedule_none))
                let window_size = 128
                [|for i in 0 .. (n-1) do
                    let _from,_to = (i*96),(i*96)+window_size
                    let day = Array.sub realtime _from window_size
                    let phev = 
                        match baseline with 
                        | None | Some Expected ->
                            Tree.phev_expected
                        | Some Simulated ->
                            Array.sub phev_contribution _from window_size
            
                    let realtime_updated =                 
                        Array.sum2 phev day
                        |> DayAhead.shave shavingAlpha shavingTheta
                    yield! realtime_updated|]
            | Some Method.Swarm ->
// Swarm alternative:
                DayaheadSwarm.dayahead(realtime, n) 
            | Some Method.Distance ->
// Non-swarm alternative:
                DayaheadExp.Algorithm.distribute phev_contribution realtime distanceTheta n |> Array.ofList
            | Some Method.Random ->
                DayaheadExp.Algorithm.distribute_random phev_contribution realtime n |> Array.ofList
            | Some Method.Mixed ->
                DayaheadExp.Algorithm.distribute_mixed phev_contribution realtime n |> Array.ofList
                
//        printfn "sum of dayahead %f" <| Array.sum dayahead
        postalService.send("brp", Dayahead(dayahead |> Array.get))
        postalService.send("brp", Prediction(realtime |> Array.get))

//        IO.write_doubles <| FileManager.file_prediction <| Parsing.parse_dayahead (List.ofArray pnodes)
//        IO.write_doubles <| FileManager.file_dayahead <| (dayahead |> List.ofArray |> List.map Energy.toFloat)

//        PHEV.rand <- new System.Random()
        printfn "Dayahead computed"
        
    // create an infinite sequence of simulation steps
    member self.Run(?days) = 
        let n = match days with Some d -> d | None -> nIter

//        postalService.send("brp", Dayahead(FileManager.dayahead()))
//        postalService.send("brp", Prediction(FileManager.prediction()))
//        postalService.send("brp", Schedule(BRP.Action.schedule_reactive))
        
        printfn "Running simulations"
        [for i in 0 .. (n-1) do run i agents false] |> ignore 
        printfn "Finished simulations"
        kill agents

    member self.TestDayahead(n) = 
        test_dayahead n agents
