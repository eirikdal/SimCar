module Grid

#nowarn "25"

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
    let make_agent name node ttlwindow = 
        match node with
        | Transformer(_) ->
            let agent = Agent.Centralized.create_trf_agent node
            postalService.add_agent(name, agent)
            agent
        | PHEV(_) ->
            let agent = Agent.Centralized.create_phev_agent node name ttlwindow
            postalService.add_agent(name, agent)
            agent
        | PowerNode(_) ->
            let agent = Agent.Centralized.create_pnode_agent node
            postalService.add_agent(name, agent)
            agent
        | BRP(_) ->
            let agent = Agent.Centralized.create_brp_agent node 616
            postalService.add_agent(name, agent)
            agent

    // traverse a tree of models, creating a mirrored tree of agents as we go along
    let rec make_tree node ttlwindow = 
        match node with
        | Node(nodes, Some(Transformer({name=name}) as trf)) ->
            Node(List.map (fun n -> make_tree n ttlwindow) nodes, Some <| make_agent name trf ttlwindow)
        | Node(nodes, Some(PowerNode({name=name}) as pnode)) ->
            Leaf(Some <| make_agent name pnode ttlwindow)
        | Node(nodes, Some(PHEV({name=name}) as phev)) ->
            Leaf(Some <| make_agent name phev ttlwindow)
        | Node(nodes, Some(BRP({name=name}) as brp)) ->
            Node(List.map (fun n -> make_tree n ttlwindow) nodes, Some <| make_agent name brp ttlwindow)

module Decentralized = 
    module Random = 
        let make_agent name node ttlwindow =    
            match node with
            | Transformer(_) ->
                let agent = Agent.Decentralized.create_trf_agent node
                postalService.add_agent(name, agent)
                agent
            | PHEV(_) ->
                let agent = Agent.Decentralized.Random.create_phev_agent node ttlwindow
                postalService.add_agent(name, agent)
                agent
            | PowerNode(_) ->
                let agent = Agent.Decentralized.create_pnode_agent node
                postalService.add_agent(name, agent)
                agent
            | BRP(_) ->
                let agent = Agent.Decentralized.Random.create_brp_agent node
                postalService.add_agent(name, agent)
                agent

        // traverse a tree of models, creating a mirrored tree of agents as we go along
        let rec make_tree node ttlwindow = 
            match node with
            | Node(nodes, Some(Transformer({name=name}) as trf)) ->
                Node(List.map (fun n -> make_tree n ttlwindow) nodes, Some <| make_agent name trf ttlwindow)
            | Node(nodes, Some(PowerNode({name=name}) as pnode)) ->
                Leaf(Some <| make_agent name pnode ttlwindow)
            | Node(nodes, Some(PHEV({name=name}) as phev)) ->
                Leaf(Some <| make_agent name phev ttlwindow)
            | Node(nodes, Some(BRP({name=name}) as brp)) ->
                Node(List.map (fun n -> make_tree n ttlwindow) nodes, Some <| make_agent name brp ttlwindow)
    module Predictions = 
        let make_agent name node ttlwindow =    
            match node with
            | Transformer(_) ->
                let agent = Agent.Decentralized.create_trf_agent node
                postalService.add_agent(name, agent)
                agent
            | PHEV(_) ->
                let agent = Agent.Decentralized.Predictions.create_phev_agent node ttlwindow
                postalService.add_agent(name, agent)
                agent
            | PowerNode(_) ->
                let agent = Agent.Decentralized.create_pnode_agent node
                postalService.add_agent(name, agent)
                agent
            | BRP(_) ->
                let agent = Agent.Decentralized.Predictions.create_brp_agent node
                postalService.add_agent(name, agent)
                agent

        // traverse a tree of models, creating a mirrored tree of agents as we go along
        let rec make_tree node ttlwindow = 
            match node with
            | Node(nodes, Some(Transformer({name=name}) as trf)) ->
                Node(List.map (fun n -> make_tree n ttlwindow) nodes, Some <| make_agent name trf ttlwindow)
            | Node(nodes, Some(PowerNode({name=name}) as pnode)) ->
                Leaf(Some <| make_agent name pnode ttlwindow)
            | Node(nodes, Some(PHEV({name=name}) as phev)) ->
                Leaf(Some <| make_agent name phev ttlwindow)
            | Node(nodes, Some(BRP({name=name}) as brp)) ->
                Node(List.map (fun n -> make_tree n ttlwindow) nodes, Some <| make_agent name brp ttlwindow)

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

    let fold_phevs_ux (_, Model(grid)) (ac : float<kWh>) = 
        match grid with 
        | PHEV(phev_args) -> ac + phev_args.failed
        | _ -> ac

    let fold_pnodes (_, Model(grid)) (ac : float<kWh>) = 
        match grid with 
        | PowerNode(phev_args) -> ac + phev_args.current
        | _ -> ac

    let fold_trf_delta (_, Model(grid)) (ac : float<kWh>) = 
        match grid with 
        | Transformer(trf_args) -> if trf_args.current > trf_args.capacity then ac + (trf_args.current - trf_args.capacity) else ac
        | _ -> ac

    let fold_trf_filter (_, Model(grid)) (ac : float<kWh>) =
        match grid with 
        | Transformer(trf_args) -> ac + trf_args.filtered
        | _ -> ac