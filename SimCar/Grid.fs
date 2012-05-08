module Grid

open System
open Agent
open Message
open PostalService
open Models
open Transformer
open PHEV
open BRP 
open PowerNode
open MathNet.Numerics

module Centralized = 
    let make_agent name node schedule = 
        match node with
        | Transformer(_) ->
            let agent = Agent.Centralized.create_trf_agent node
            postalService.add_agent(name, agent)
            (name, agent)
        | PHEV(_) ->
            let agent = Agent.Centralized.create_phev_agent node
            postalService.add_agent(name, agent)
            (name, agent)
        | PowerNode(_) ->
            let agent = Agent.Centralized.create_pnode_agent node
            postalService.add_agent(name, agent)
            (name, agent)
        | BRP(_) ->
            let agent = Agent.Centralized.create_pnode_agent node
            postalService.add_agent(name, agent)
            (name, Agent.Centralized.create_brp_agent node schedule)

    // traverse a tree of models, creating a mirrored tree of agents as we go along
    let rec make_tree node scheduler = 
        match node with
        | Node(nodes, Some(Transformer({name=name}) as trf)) ->
            Node(List.map (fun n -> make_tree n scheduler) nodes, Some <| make_agent name trf scheduler)
        | Node(nodes, Some(PowerNode({name=name}) as pnode)) ->
            Leaf(Some <| make_agent name pnode scheduler)
        | Node(nodes, Some(PHEV({name=name}) as phev)) ->
            Leaf(Some <| make_agent name phev scheduler)
        | Node(nodes, Some(BRP({name=name}) as brp)) ->
            Node(List.map (fun n -> make_tree n scheduler) nodes, Some <| make_agent name brp scheduler)

module Decentralized = 
    module Random = 
        let make_agent name node =    
            match node with
            | Transformer(_) ->
                let agent = Agent.Decentralized.create_trf_agent node
                postalService.add_agent(name, agent)
                (name, agent)
            | PHEV(_) ->
                let agent = Agent.Decentralized.Random.create_phev_agent node
                postalService.add_agent(name, agent)
                (name, agent)
            | PowerNode(_) ->
                let agent = Agent.Decentralized.create_pnode_agent node
                postalService.add_agent(name, agent)
                (name, agent)
            | BRP(_) ->
                let agent = Agent.Decentralized.Random.create_brp_agent node
                postalService.add_agent(name, agent)
                (name, agent)

        // traverse a tree of models, creating a mirrored tree of agents as we go along
        let rec make_tree node = 
            match node with
            | Node(nodes, Some(Transformer({name=name}) as trf)) ->
                Node(List.map (fun n -> make_tree n) nodes, Some <| make_agent name trf)
            | Node(nodes, Some(PowerNode({name=name}) as pnode)) ->
                Leaf(Some <| make_agent name pnode)
            | Node(nodes, Some(PHEV({name=name}) as phev)) ->
                Leaf(Some <| make_agent name phev)
            | Node(nodes, Some(BRP({name=name}) as brp)) ->
                Node(List.map (fun n -> make_tree n) nodes, Some <| make_agent name brp)
    module Mixed = 
        let make_agent name node =    
            match node with
            | Transformer(_) ->
                let agent = Agent.Decentralized.create_trf_agent node
                postalService.add_agent(name, agent)
                (name, agent)
            | PHEV(_) ->
                let agent = Agent.Decentralized.Mixed.create_phev_agent node
                postalService.add_agent(name, agent)
                (name, agent)
            | PowerNode(_) ->
                let agent = Agent.Decentralized.create_pnode_agent node
                postalService.add_agent(name, agent)
                (name, agent)
            | BRP(_) ->
                let agent = Agent.Decentralized.Mixed.create_brp_agent node
                postalService.add_agent(name, agent)
                (name, agent)

        // traverse a tree of models, creating a mirrored tree of agents as we go along
        let rec make_tree node = 
            match node with
            | Node(nodes, Some(Transformer({name=name}) as trf)) ->
                Node(List.map (fun n -> make_tree n) nodes, Some <| make_agent name trf)
            | Node(nodes, Some(PowerNode({name=name}) as pnode)) ->
                Leaf(Some <| make_agent name pnode)
            | Node(nodes, Some(PHEV({name=name}) as phev)) ->
                Leaf(Some <| make_agent name phev)
            | Node(nodes, Some(BRP({name=name}) as brp)) ->
                Node(List.map (fun n -> make_tree n) nodes, Some <| make_agent name brp)

module Util  = 
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