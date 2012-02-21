// Learn more about F# at http://fsharp.net

open Agent
open System
open System.Threading
open System.IO
open SynchronizationContext
open Models
open Message
open PHEV
open ComManager
open FileManager
open Transformer

let rec run tick =
//    update models

    run <| tick+1

[<EntryPoint>]
let main args = 
    let postalService = new PostalService()

//    list_of_phevs()
//    |> Seq.map (fun phev -> phev_agent phev)
//    |> Seq.iter (fun phev -> postalService.add_agent(phev))

    let test = list_of_trfs()

    list_of_trfs()
    |> Seq.map (fun trf -> trf_agent "test123" trf Seq.empty)
    |> Seq.iter (fun trf -> postalService.add_agent(trf))

    jobCompleted.Publish.Add(fun (agent, str) -> postalService.Post(Completed(sprintf "%s" str)))

    postalService.send_to_all(Hello)

    run 0