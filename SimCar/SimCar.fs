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
//    let phevs = list_of_phevs()
    let comManager = new ComManager()

    let phev_agents = list_of_phevs()
    Seq.iter (fun phev -> comManager.add_agent(phev_agent phev))
    ignore(Console.ReadKey())
    0