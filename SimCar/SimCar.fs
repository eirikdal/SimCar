// Learn more about F# at http://fsharp.net
module SimCar

open Agent
open DayAhead
open System
open System.Threading
open System.IO
open SynchronizationContext
open Models
open Message
open PHEV
open PostalService
open FileManager
open Transformer
open MSDN.FSharp
open MSDN.FSharp.Charting
open System.Windows
open System.Windows.Forms
open System.Windows.Forms.DataVisualization

#nowarn "25"

let postalService = new PostalService()

// create chart from data
// TODO: Change labelling of X-axis to reflect hours
let create_chart data title = 
    let formsHost = new Forms.Integration.WindowsFormsHost(Child = new Charting.ChartControl(data))
    let graphWindow = new Window(Content = formsHost, Title = title)
    let wpfApp = new System.Windows.Application()
    wpfApp.Run(graphWindow) |> ignore

// for testing purposes
let print_grid message =
    match message with 
    | Model(gridnode) -> 
        postalService.Post(Completed(sprintf "Received node %s" gridnode.name))
        Model(gridnode)

// the update-function, takes the current node and threaded accumulator as parameters
let update (ag:Agent<Message<_>>, Model(grid)) (ac : float<kWh>) : float<kWh> = 
    match grid with 
    | Transformer(trf_args) ->
        let trf = Transformer({ trf_args with current=ac })
        ag.Post(Model(trf))
        ac
    | PHEV(phev_args) ->
        if phev_args.current > 0.0<kWh> then
            ac + phev_args.current
        else
            ac + phev_args.current
    | BRP(brp_args) ->
        let brp = BRP({ brp_args with current=ac })
        ag.Post(Model(brp))
        ac
    | PowerNode(pnode_args) ->
//        ag.Post(Model(grid))
        pnode_args.current

let fold_phevs (_, Model(grid)) (ac : float<kWh>) = 
    match grid with 
    | PHEV(phev_args) -> ac + phev_args.current
    | _ -> ac

let fold_pnodes (_, Model(grid)) (ac : float<kWh>) = 
    match grid with 
    | PowerNode(phev_args) -> ac + phev_args.current
    | _ -> ac

let moving_average (array : float<kWh> array) = 
    array |> Seq.ofArray |> Seq.windowed (5) |> Seq.map Array.average

// main control flow of the simulator
let run day agents =
    let run_sim tick = 
        agents
        |> Tree.send (Update(tick)) // inform agents that new tick has begun
        |> Tree.send_and_reply RequestModel // request model from agents

    let realtime = Array.init(96) (fun i -> run_sim i)

    let updated_realtime = 
        realtime
        |> Array.map (Tree.foldr update) // right-fold over tree, applying the update function (inorder traversal)

    syncContext.RaiseEvent updateEvent updated_realtime

    let phevs = 
        realtime
        |> Array.map (Tree.foldr fold_phevs)

    let pnodes = 
        realtime
        |> Array.map (Tree.foldr fold_pnodes)

    // calculate dayahead profile
    let dayahead = 
        updated_realtime
//        |> scan
        |> moving_average
        |> Array.ofSeq

    // calculate total energy consumption
    let sum_realtime = 
        updated_realtime 
        |> Array.fold (fun ac rt -> ac + rt) 0.0<kWh>

    // calculate total dayahead consumption
    let sum_phevs = 
        phevs
        |> Array.fold (fun ac d -> ac + d) 0.0<kWh>

    let sum_pnodes = 
        pnodes
        |> Array.fold (fun ac d -> ac + d) 0.0<kWh>

//    printf "Sum PowerNodes: %f\n" <| Energy.toFloat sum_pnodes
//    printf "Sum PHEVs: %f\n" <| Energy.toFloat sum_phevs
//    printf "Sum realtime: %f\n" <| Energy.toFloat sum_realtime

    (phevs, pnodes)
