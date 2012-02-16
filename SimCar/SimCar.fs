// Learn more about F# at http://fsharp.net

open Agent
open System
open System.IO
open Models
open Message
open PHEV
open ComManager
open FileManager

[<EntryPoint>]
let main args = 
    let postalService = new PostalService()

    let phev_agents = list_of_phevs()

    ignore(Seq.iter (fun phev -> postalService.add_agent(phev, PHEV_Agent)))

    ignore(Console.ReadKey())

    0