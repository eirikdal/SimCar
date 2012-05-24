﻿module Sim

open System
//open System.Drawing
open SimCar
open SynchronizationContext
open MathNet.Numerics.Statistics
open Message
open PostalService
open FileManager
open Models
open Tree

type Method = 
    | Shaving
    | Distance
    | Mixed
    | Random
    | Superposition
    with
    override self.ToString() =
        match self with 
        | Shaving -> "Peak-shaving"
        | Distance -> "Distance"
        | Method.Mixed -> "Mixed" 
        | Method.Random -> "Random"
        | Superposition -> "Superposition"

type Contribution =
    | Expected
    | Simulated
    with
    override self.ToString() =
        match self with
        | Expected -> "Expected"
        | Simulated -> "Simulated"

type Scheduler = 
    | Proactive 
    | Reactive
    | Random
    | Mixed
    with 
    override self.ToString() = 
        match self with
        | Proactive -> "Proactive"
        | Reactive -> "Reactive"
        | Random -> "Random"
        | Mixed -> "Mixed"
    
type SimCar(nTicksPerDay) =
    let mutable distanceTheta = 1.0
    let mutable shavingTheta = 0.99
    let mutable shavingAlpha = 0.2
    let mutable ttlWindow = 30
    let mutable nDays = 10
    let mutable _method = Some Distance
    let mutable _contr = None
    let mutable schedule = BRP.Action.schedule_none
    let mutable _scheduler = None
    let mutable agents = None//Grid.Centralized.make_tree (powergrid) ttlWindow
    let mutable powergrid = None

    member self.Agents with get() = agents.Value
    member self.DistanceTheta with set(theta) = distanceTheta <- theta
    member self.ShavingTheta with set(theta) = shavingTheta <- theta
    member self.ShavingAlpha with set(alpha) = shavingAlpha <- alpha
    member self.Days with set(days) = nDays <- days and get() = nDays
    member self.Method with set(meth) = _method <- meth
    member self.Contribution with set(contr) = _contr <- contr
    member self.PhevWindow with set(window) = ttlWindow <- window
    member self.Scheduler with set(scheduler) = 
                                                postalService.Reset()
                                                powergrid <- Some <| create_powergrid()

                                                agents <- Some <|
                                                match scheduler with
                                                | Some Reactive -> 
                                                    schedule <- BRP.Action.schedule_reactive
                                                    Grid.Centralized.make_tree <| powergrid.Value <| ttlWindow
                                                | Some Proactive -> 
                                                    schedule <- BRP.Action.schedule_proactive
                                                    Grid.Centralized.make_tree <| powergrid.Value <| ttlWindow
                                                | Some Mixed -> 
                                                    Grid.Decentralized.Mixed.make_tree <| powergrid.Value <| ttlWindow 
                                                | Some Random ->
                                                    Grid.Decentralized.Random.make_tree <| powergrid.Value <| ttlWindow
                                                | None -> 
                                                    schedule <- BRP.Action.schedule_none
                                                    Grid.Decentralized.Random.make_tree <| powergrid.Value <| ttlWindow
                                                _scheduler <- scheduler
                                                    
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

    member self.RegisterPhevLeft (handler) = 
        phevLeft.Publish.AddHandler handler

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

    member self.RegisterTrfUpdate (handler) = 
        trfUpdate.Publish.AddHandler handler

    member self.RegisterError (handler) = 
        jobError.Publish.AddHandler handler

    member self.RegisterDebug (handler) =
        jobDebug.Publish.AddHandler handler

    member self.RegisterProgress (handler) = 
        jobProgress.Publish.AddHandler handler

//    member self.RegisterComputeDayahead () = 
//        updateEvent.Publish.Add(fun dayahead -> IO.write_to_file <| FileManager.file_dayahead <| Parsing.parse_dayahead (List.ofArray dayahead))
//    
    // attach functions to events
//    member self.RegisterEvents () = 
//        error.Publish.Add(fun e -> postalService.Post(Error(sprintf "%s" e.Message)))
//        jobDebug.Publish.Add(fun str -> syncContext.RaiseDelegateEvent jobProgress <|  "%s" str)

//    member self.Init() = 
//        IO.clear_screenshots()

    member self.ComputeDayahead() = 
        IO.clear_dayahead_data()
        postalService.send("brp", Schedule(BRP.Action.schedule_none))

        let op i node = 
            match node with
            | Transformer(_) -> 0.0<kWh>
            | PHEV(_) -> 0.0<kWh>
            | PowerNode({ realtime=realtime }) -> realtime(i)
            | BRP(_) -> 0.0<kWh>

        let calc_power tick = 
            powergrid.Value
            |> Tree.foldl (op tick) (0.0<kWh>)

        let realtime = [for i in 0 .. (nDays+2)*96 do yield calc_power i] |> Array.ofList

        syncContext.RaiseDelegateEvent jobDebug <| sprintf "[%s] Computing dayahead" (String.Format("{0:hh:mm}", DateTime.Now))
        
        let phev_contribution = 
            match _contr with
            | Some Expected -> 
                Tree.phev_expected powergrid.Value
            | Some Simulated -> 
                [for i in 0 .. (nDays+2) do compute_dayahead i agents.Value] |> ignore
                agents.Value |> Tree.send (Reset) |> ignore
                Array.init((nDays+2)*96) (fun i -> FileManager.dayahead()(i))
            | None ->
                Array.init((nDays+2)*96) (fun _ -> 0.0<kWh>)

        let dayahead = 
            match _method with
            | Some Method.Shaving ->
                postalService.send("brp", Dayahead((fun _ -> 0.0<kWh>)))
                postalService.send("brp", Prediction((fun _ -> 0.0<kWh>)))
                postalService.send("brp", Schedule(BRP.Action.schedule_none))
                let window_size = 120
                
                let dayahead = Array.copy realtime

                for i in 0 .. (nDays) do
                    let _from,_to = (i*96),(i*96)+window_size
                    let day = Array.sub realtime _from window_size
                    let phev = 
                        match _contr with 
                        | Some Expected ->
                            let temp = Tree.phev_expected powergrid.Value
                            [|for i in 0 .. window_size-1 do yield temp.[i%96]|]
                        | Some Simulated ->
                            Array.sub phev_contribution _from window_size
                        | None ->
                            day
                    
                    let temp = Array.sum2 phev day |> DayAhead.Shifted.shave shavingAlpha shavingTheta phev

                    for j in _from .. _to-1 do 
                        dayahead.[j] <- temp.[j-_from]

                dayahead
            | Some Method.Superposition ->
                let window_size = 96
                [|for i in 0 .. (nDays) do
                    let _from,_to = (i*96),(i*96)+window_size
                    let day = Array.sub realtime _from window_size
                    let phev = 
                        match _contr with 
                        | Some Expected ->
                            phev_contribution
                        | Some Simulated ->
                            Array.sub phev_contribution _from window_size
                        | None ->
                            day
                    yield! Array.sum2 phev day|]
            | Some Method.Distance ->
                DayaheadExp.Algorithm.distribute phev_contribution distanceTheta (nDays+1) realtime |> Array.ofList
            | Some Method.Random ->
                DayaheadExp.Algorithm.distribute_random phev_contribution realtime (nDays+1) |> Array.ofList
            | Some Method.Mixed ->
                DayaheadExp.Algorithm.distribute_mixed phev_contribution realtime (nDays+1) |> Array.ofList
            | None ->
                Array.init ((nDays+1)*96) (fun _ -> 0.0<kWh>)
                
//        syncContext.RaiseDelegateEvent jobProgress <|  "sum of dayahead %f" <| Array.sum dayahead
        postalService.send("brp", Dayahead(dayahead |> Array.get))
        postalService.send("brp", Prediction(realtime |> Array.get))

//        IO.write_doubles <| FileManager.file_prediction <| Parsing.parse_dayahead (List.ofArray pnodes)
//        IO.write_doubles <| FileManager.file_dayahead <| (dayahead |> List.ofArray |> List.map Energy.toFloat)

        syncContext.RaiseDelegateEvent jobDebug <| sprintf "[%s] Dayahead computed" (String.Format("{0:hh:mm}", DateTime.Now))
        
    // create an infinite sequence of simulation steps
    member self.Run(started : string) = 
//        postalService.send("brp", Dayahead(FileManager.dayahead()))
//        postalService.send("brp", Prediction(FileManager.prediction()))
        postalService.send("brp", Schedule(schedule))
        
        match _scheduler with 
        | None -> self.Agents |> Tree.send(Filter(false)) |> ignore
        | _ -> self.Agents |> Tree.send(Filter(true)) |> ignore
            

        syncContext.RaiseDelegateEvent jobDebug <|  sprintf "[%s] Running simulations" (String.Format("{0:hh:mm}", DateTime.Now))
        let results = [for i in 0 .. (nDays-1) do yield run i agents.Value] 
        syncContext.RaiseDelegateEvent jobProgress <| "------------------------------------"
        syncContext.RaiseDelegateEvent jobDebug <| sprintf "[%s] Finished simulations" (String.Format("{0:hh:mm}", DateTime.Now))
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "\t\tAverage\t\tMax\t\tstd\t\tskewness\n"

        let (phevs_sum, phevs_ux, pnodes_sum, total_max, total_avg, total_sum,
             par, dayahead_sum, dif, ratio, trf_delta, trf_filtered) = 
            results |> List.fold (fun (a,l,b,c,d,e,f,g,h,i,j,k) (x:Results) -> (x.phevs_sum::a, x.phevs_ux::l, x.pnodes_sum::b, x.total_max::c,
                                                                                x.total_avg::d, x.total_sum::e, x.par::f,
                                                                                x.dayahead_sum::g, x.dif::h, x.ratio::i, x.trf_delta::j, x.trf_filtered::k)) ([],[],[],[],[],[],[],[],[],[],[],[])
                                    
                    
        let phevs_stat, phevs_ux_stat, pnodes_stat, max_stat, avg_stat, sum_stat, 
            par_stat, dayahead_stat, dif_stat, ratio_stat, trf_stat, trf_fltr_stat = 
                new DescriptiveStatistics(phevs_sum),
                new DescriptiveStatistics(phevs_ux),
                new DescriptiveStatistics(pnodes_sum),
                new DescriptiveStatistics(total_max),
                new DescriptiveStatistics(total_avg),
                new DescriptiveStatistics(total_sum),
                new DescriptiveStatistics(par),
                new DescriptiveStatistics(dayahead_sum),
                new DescriptiveStatistics(dif),
                new DescriptiveStatistics(ratio),
                new DescriptiveStatistics(trf_delta),
                new DescriptiveStatistics(trf_filtered)

        let print_description meth contr scheduler = 
            let match_method scheduler meth = 
                match meth with
                | Some Shaving ->
                    match contr with 
                    | Some Expected -> sprintf "After %i days of simulation using %s scheduling with peak-shaving on expected values for PHEV demand. (\(theta\)=%f, \(alpha\)=%f and PHEV learning window of %i)" nDays scheduler shavingTheta shavingAlpha ttlWindow
                    | Some Simulated -> sprintf "After %i days of simulation using %s scheduling with peak-shaving on simulated values for PHEV demand (\(theta\)=%f, \(alpha\)=%f and PHEV learning window of %i)" nDays scheduler shavingTheta shavingAlpha ttlWindow
                | Some Distance ->
                    match contr with 
                    | Some Expected -> sprintf "After %i days of simulation using %s scheduling with distance-rule on expected values for PHEV demand (\(theta\)=%f and PHEV learning window of %i)" nDays scheduler distanceTheta ttlWindow
                    | Some Simulated -> sprintf "After %i days of simulation using %s scheduling with distance-rule on simulated values for PHEV demand (\(theta\)=%f and PHEV learning window of %i)" nDays scheduler distanceTheta ttlWindow
            match scheduler with 
            | Some Proactive ->
                match_method "proactive" meth
            | Some Reactive ->
                match_method "reactive" meth
            | Some Random ->
                sprintf "After %i days of simulation using random mechanism with learning window %i" nDays ttlWindow
            | Some Mixed ->
                sprintf "After %i days of simulation using mixed mechanism with learning window %i" nDays ttlWindow
            | None -> 
                sprintf "Baseline after %i days of simulation" nDays

        let capt (opt : 'a option) = if opt.IsSome then opt.ToString() else "None"

        let latex = 
            sprintf "\\begin{table}
\\begin{center}
\\begin{tabular}{l | l | l | l | l}
%s & %s & %s & %s & %s\\\\
\hline
\hline
%s & %.2f* & %.2f* & %.1f & %.2f \\\\
%s & %.2f* & %.2f* & %.1f & %.2f  \\\\
%s & %.2f* & %.2f* & %.1f & %.2f  \\\\
%s & %.1f & %.1f & %.1f & %.2f  \\\\
%s & %.1f & %.1f & %.1f & %.2f  \\\\
%s & %.2f & %.2f & %.2f & %.2f  \\\\
%s & %.2f* & %.2f* & %.2f* & %.2f  \\\\
%s & %.2f* & %.2f* & %.2f* & %.2f  \\\\
\cline{2-4}
%s & %.2f* & %.2f* & %.2f* & %.2f  \\\\
\cline{2-4}
%s & %.2f* & %.2f* & %.2f & %.2f  \\\\
\end{tabular}
\\end{center}
\\caption{%s}
\\label{tab:experiment-%s}
\\end{table}"
                    "Desc." "\(\mu\)" "Max" "\(\sigma\)" "\(\gamma\)"
//                    "PHEVs" phevs_stat.Mean phevs_stat.Maximum phevs_stat.StandardDeviation
//                    "PowerNodes" pnodes_stat.Mean phevs_stat.Maximum phevs_stat.StandardDeviation
                    "Peak" (max_stat.Mean / 1000.0) (max_stat.Maximum / 1000.0) max_stat.StandardDeviation max_stat.Skewness
                    "Daily.Avg" (avg_stat.Mean / 1000.0) (avg_stat.Maximum / 1000.0) avg_stat.StandardDeviation avg_stat.Skewness
                    "PHEV(Ux)" (phevs_ux_stat.Mean / 1000.0) (phevs_ux_stat.Maximum / 1000.0) phevs_ux_stat.StandardDeviation phevs_ux_stat.Skewness
                    "Trf(Exc)" trf_stat.Mean trf_stat.Maximum trf_stat.StandardDeviation trf_stat.Skewness
                    "Trf(Flt)" trf_fltr_stat.Mean trf_fltr_stat.Maximum trf_fltr_stat.StandardDeviation trf_fltr_stat.Skewness
                    "PAR" par_stat.Mean par_stat.Maximum par_stat.StandardDeviation par_stat.Skewness
                    "Dayh." (dayahead_stat.Mean / 1000.0) (dayahead_stat.Maximum / 1000.0) (dayahead_stat.StandardDeviation / 1000.0) dayahead_stat.Skewness
                    "Dayh.(Dx)" (dif_stat.Mean / 1000.0) (dif_stat.Maximum / 1000.0) (dif_stat.StandardDeviation / 1000.0) dif_stat.Skewness
                    "Total" (sum_stat.Mean / 1000.0) (sum_stat.Maximum / 1000.0) (sum_stat.StandardDeviation / 1000.0) sum_stat.Skewness
                    "DDx/Tot." ratio_stat.Mean ratio_stat.Maximum ratio_stat.StandardDeviation ratio_stat.Skewness
                    (print_description _method _contr _scheduler)
                    (String.Format("{0:hhmm}", DateTime.Now))

        let fileLog = String.Format("c:\\SimCar\\SimCar\\data\\log\\latex\\{0}.tex", started);
        let file = new System.IO.StreamWriter(fileLog);
        file.WriteLine(latex);
        file.Close()

        syncContext.RaiseDelegateEvent jobProgress <| sprintf "PHEVs\t\t%.2f\t\t%.2f\t\t%.2f\t\t%.2f" phevs_stat.Mean phevs_stat.Maximum phevs_stat.StandardDeviation phevs_stat.Skewness
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "PHEVs (Ux)\t\t%.2f\t\t%.2f\t\t%.2f\t\t%.2f" phevs_ux_stat.Mean phevs_ux_stat.Maximum phevs_ux_stat.StandardDeviation phevs_ux_stat.Skewness
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "PowerNodes\t%.2f\t%.2f\t%.2f\t\t%.2f" pnodes_stat.Mean pnodes_stat.Maximum pnodes_stat.StandardDeviation pnodes_stat.Skewness
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Peak\t\t%.2f\t\t%.2f\t\t%.2f\t\t%.2f" max_stat.Mean max_stat.Maximum max_stat.StandardDeviation max_stat.Skewness
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Daily Avg\t\t%.2f\t\t%.2f\t\t%.2f\t\t%.2f" avg_stat.Mean avg_stat.Maximum avg_stat.StandardDeviation avg_stat.StandardDeviation
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "PAR\t\t%.3f\t\t%.3f\t\t%.3f\t\t%.2f" par_stat.Mean par_stat.Maximum par_stat.StandardDeviation par_stat.Skewness
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Dayahead\t%.2f\t%.2f\t%.2f\t\t%.2f" dayahead_stat.Mean dayahead_stat.Maximum dayahead_stat.StandardDeviation dayahead_stat.Skewness
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Dayahead (Dx)\t\t%.2f\t\t%.2f\t\t%.2f\t\t%.2f" dif_stat.Mean dif_stat.Maximum dif_stat.StandardDeviation dif_stat.Skewness
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Ratio\t\t%.3f\t\t%.3f\t\t%.3f\t\t%.2f" ratio_stat.Mean ratio_stat.Maximum ratio_stat.StandardDeviation ratio_stat.Skewness
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Trf (Dx)\t\t%.2f\t\t%.2f\t\t%.2f\t\t%.2f" trf_stat.Mean trf_stat.Maximum trf_stat.StandardDeviation trf_stat.Skewness
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Trf (Filtered)\t\t%.2f\t\t%.2f\t\t%.2f\t\t%.2f" trf_fltr_stat.Mean trf_fltr_stat.Maximum trf_fltr_stat.StandardDeviation trf_fltr_stat.Skewness
        syncContext.RaiseDelegateEvent jobProgress <| sprintf "Total\t\t%.2f\t%.2f\t%.2f\t\t%.2f" sum_stat.Mean sum_stat.Maximum sum_stat.StandardDeviation sum_stat.Skewness

    member self.TestDayahead(n) = 
        test_dayahead n agents.Value

    member self.Kill() =
        kill agents.Value