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

type SimCar(nIter, nTicksPerDay) = 
    let _agents = to_agents powergrid
    member self.Agents = _agents |> Tree.map (fun (name, from) -> from)
        
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
    
    // attach functions to events
    member self.RegisterEvents () = 
        error.Publish.Add(fun e -> postalService.Post(Error(sprintf "%s" e.Message)))
        
    member self.Init() = 
        postalService.agents <- _agents

    // create an infinite sequence of simulation steps
    member self.Run() = 
        Seq.initInfinite (fun day -> run day self.Agents)
        |> Seq.take nIter 
        |> Seq.cache
        |> List.ofSeq
