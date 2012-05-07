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
open BRP 
open PowerNode
open PostalService
open FileManager
open Transformer
open System.Windows

#nowarn "25"

let mutable schedule = BRP.Action.schedule_greedy
// make the right kind of agent for a given node
let model_to_agent name node = 
    match node with
    | Transformer(_) ->
        (name, Agent.Centralized.create_trf_agent node)
    | PHEV(_) ->
        (name, Agent.Centralized.create_phev_agent node name)
    | PowerNode(_) ->
        (name, Agent.Centralized.create_pnode_agent node)
    | BRP(_) ->
        (name, Agent.Centralized.create_brp_agent node schedule)

// traverse a tree of models, creating a mirrored tree of agents as we go along
let rec to_agents node = 
    match node with
    | Node(nodes, Some(Transformer({name=name}) as trf)) ->
        Node(List.map (fun n -> to_agents n) nodes, Some <| model_to_agent name trf)
    | Node(nodes, Some(PowerNode({name=name}) as pnode)) ->
        Leaf(Some <| model_to_agent name pnode)
    | Node(nodes, Some(PHEV({name=name}) as phev)) ->
        Leaf(Some <| model_to_agent name phev)
    | Node(nodes, Some(BRP({name=name}) as brp)) ->
        Node(List.map (fun n -> to_agents n) nodes, Some <| model_to_agent name brp)

// for testing purposes
let print_grid message =
    match message with 
    | Model(gridnode) -> 
        postalService.Post(Completed(sprintf "Received node %s" gridnode.name))
        Model(gridnode)

// the update-function, takes the current node and threaded accumulator as parameters
let update (ag:Agent<_>, Model(grid)) (ac : float<kWh>) : float<kWh> = 
    match grid with 
    | Transformer(trf_args) ->
        let trf = Transformer({ trf_args with current=ac })
        ag.Post(Model(trf))
        ac
    | PHEV(phev_args) ->
        if phev_args.current > 0.0<kWh> then
            ac + phev_args.current
        else
            ac
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

// Compute kernel for the blur algorithm
let computeCoefficients size =
  let halfSize = size / 2
  let gauss = List.init size (fun i -> 
    Math.Exp(-float((i - halfSize) * (i - halfSize)) / 8.0))
    
  // Normalize values and convert them to 'float4'
  let sum = List.sum gauss
  [ for v in gauss -> (v / sum) ]

let filter = computeCoefficients 4
let gaussian_blur (array : float<kWh> array) = 
    array |> Seq.ofArray |> Seq.windowed (4) |> Seq.map (fun x -> Seq.map2 (fun f x -> f * x) filter x) |> Seq.map Seq.sum

let moving_average (array : float<kWh> array) = 
    array |> Seq.ofArray |> Seq.windowed (4) |> Seq.map Array.average

let test_dayahead iter agents = 
    let tick n = 
        agents
        |> Tree.send (Update(n)) // inform agents that new tick has begun
        |> Tree.send_reply RequestModel // request model from agents

    let realtime = Array.init(96) (fun i -> tick i)

    let updated_realtime = 
        realtime
        |> Array.map (Tree.foldr update) // right-fold over tree, applying the update function (inorder traversal)
    
    let rec shave n rt = 
        syncContext.RaiseDelegateEvent dayaheadProgress rt
        if n > 0 then 
            shave (n-1) (rt |> DayAhead.shave 0.3 0.95)

    shave iter updated_realtime
//
//    let test = [|0.0<kWh>|] |> Array.append updated_realtime |> Array.append [|0.0<kWh>;0.0<kWh>|]
//    let moving_dayahead = 
//        test
//        |> gaussian_blur
//        |> Array.ofSeq

//    syncContext.RaiseDelegateEvent progressPnode updated_realtime      
//    syncContext.RaiseDelegateEvent progressTotal moving_dayahead
    // Raise events
let kill agents = 
    agents
    |> Tree.send (Kill)
    |> ignore

// main control flow of the simulator
let run day agents compute_dayahead =
    let tick n = 
        agents
        |> Tree.send (Update(n)) // inform agents that new tick has begun
        |> Tree.send_reply RequestModel // request model from agents

    printfn "Simulating day %d" day

    let realtime = Array.init(96) (fun i -> tick ((day*96) + i))

    // right-fold over tree, applying the update function (inorder traversal)
    let updated_realtime = 
        realtime
        |> Array.map (Tree.foldr update) 

    let pnodes = 
        realtime
        |> Array.map (Tree.foldr fold_pnodes)

    let phevs = 
        realtime
        |> Array.map (Tree.foldr fold_phevs)

//    let moving_dayahead = 
//        updated_realtime
//        |> moving_average
//        |> Array.ofSeq
    
    if compute_dayahead then
        IO.write_doubles <| FileManager.file_prediction <| Parsing.parse_dayahead (List.ofArray pnodes)
        IO.write_doubles <| FileManager.file_dayahead <| Parsing.parse_dayahead (List.ofArray phevs)
//        syncContext.RaiseEvent updateEvent <| dayahead
    else
        printfn "sum of phevs %f" <| (phevs |> Array.map Energy.toFloat |> Array.sum)

        let (Model(BRP( { dayahead=dayahead }))) = postalService.send_reply("brp", RequestDayahead)

        let dayahead = Array.init(96) (fun i -> dayahead ((day*96) + i))
        // Raise events
        syncContext.RaiseDelegateEvent dayaheadProgress dayahead
        syncContext.RaiseDelegateEvent progressPhev phevs
        syncContext.RaiseDelegateEvent progressPnode pnodes
        syncContext.RaiseDelegateEvent progressTotal updated_realtime