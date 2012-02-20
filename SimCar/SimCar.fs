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

[<EntryPoint>]
let main args = 
    let postalService = new PostalService()

    let phev_agents = list_of_phevs()
    let trf_agents = list_of_trfs()

    phev_agents
    |> Seq.map (fun phev -> phev_agent phev)
    |> Seq.iter (fun phev -> postalService.add_agent(phev, PHEV_Agent))

    trf_agents
    |> Seq.map (fun trf -> trf_agent trf)
    |> Seq.iter (fun trf -> postalService.add_agent(trf, Trf_Agent))

    ignore(Console.ReadKey())

    0