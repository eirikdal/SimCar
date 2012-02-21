// Learn more about F# at http://fsharp.net

open Agent
open System
open System.IO
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

    list_of_phevs()
    |> Seq.map (fun phev -> phev_agent phev)
    |> Seq.iter (fun phev -> postalService.add_agent(phev))

    list_of_trfs()
    |> Seq.map (fun trf -> trf_agent "test123" trf Seq.empty)
    |> Seq.iter (fun trf -> postalService.add_agent(trf))

    postalService.send_all(Hello)

    run 0