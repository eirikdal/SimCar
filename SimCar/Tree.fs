module Tree

open Agent
open Message
open System
open System.Globalization
open Models

// traverse a tree of nodes, applying function iterf to each node
let rec iter iterf node = 
    match node with
    | Node(nodes, Some(leaf)) ->
        iterf leaf
        List.iter (fun n -> iter iterf n) nodes
    | Leaf(Some(leaf)) ->
        iterf leaf
    | Node(nodes, None) ->
        Seq.iter (fun n -> iter iterf n) nodes
    | Leaf(None) ->
        ()

// mapBack function mapf to each node in the tree
let rec mapBack mapf node = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let list = List.map (fun n -> mapBack mapf n) nodes
        let res = mapf leaf
        Node(list, Some(res))
    | Leaf(Some(leaf)) ->
        Leaf(Some(mapf leaf))
    | Node(nodes, None) ->
        Node(List.map (fun n -> mapBack mapf n) nodes, None)
    | Leaf(None) ->
        Leaf(None)

// map function mapf to each node in the tree
let rec map mapf node = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let res = mapf leaf
        Node(List.map (fun n -> map mapf n) nodes, Some(res))
    | Leaf(Some(leaf)) ->
        Leaf(Some(mapf leaf))
    | Node(nodes, None) ->
        Node(List.map (fun n -> map mapf n) nodes, None)
    | Leaf(None) ->
        Leaf(None)

// traverse a tree of agents, return an isomorphic tree of (agent,message) pairs
let send_reply msg node = 
    let rec send_reply msg (node : Node<Agent<Message>>) = 
        match node with
        | Node(nodes, Some(leaf)) ->
            let list = List.map (fun n -> send_reply msg n) nodes 
            let res = leaf.PostAndAsyncReply((fun replyChannel -> ReplyTo(msg, replyChannel)), 100000)
            Node(list, Some (leaf, res))
        | Node(nodes, None) -> 
            Node(List.map (fun n -> send_reply msg n) nodes, None)
        | Leaf(Some(leaf)) ->
            let res = leaf.PostAndAsyncReply((fun replyChannel -> ReplyTo(msg, replyChannel)), 100000)
            Leaf(Some <| (leaf, res))
        | Leaf(None) ->
            Leaf(None)

    send_reply msg node
    |> mapBack (fun (ag, msg) -> (ag, Async.RunSynchronously(msg)))

// traverse a tree of models, return an isomorphic tree of agents
let rec send msg (node : Node<Agent<Message>>) = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let res = leaf.Post(msg)
        Node(List.map (fun n -> send msg n) nodes, Some <| leaf)
    | Node(nodes, None) -> 
        Node(List.map (fun n -> send msg n) nodes, None)
    | Leaf(Some(leaf)) ->
        let res = leaf.Post(msg)
        Leaf(Some <| leaf)
    | Leaf(None) ->
        Leaf(None)

// fold left on a tree using preorder traversal
let rec foldl op res node = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let temp = op leaf
        List.fold (fun (ac : energy) n -> ac + foldl op temp n) temp nodes
    | Leaf(Some(leaf)) ->
        op leaf
    | Node(nodes, None) ->
        List.fold (fun (ac : energy) n -> ac + foldl op res n) res nodes
    | Leaf(None) ->
        res

// fold right on a tree using inorder traversal
let rec foldr op node : float<kWh> = 
    match node with 
    | Node(nodes, Some(leaf)) ->
        let f = List.fold (fun ac f -> ac + f) 0.0<kWh> ([ for n in nodes do yield foldr op n ])
        op leaf f
    | Leaf(Some(leaf)) ->
        op leaf 0.0<kWh>
    | Node(nodes, None) -> 
        List.fold (fun ac f -> ac + f) 0.0<kWh> ([ for n in nodes do yield foldr op n ])
    | Leaf(None) -> 
        0.0<kWh>
        
//let post_reply msg (leaf : Agent<Message>) = leaf.PostAndReply((fun replyChannel -> ReplyTo(msg, replyChannel)), 100000)

// traverse tree, return a sequence of the leaves
let rec collect node = 
    seq {
        match node with 
        | Node(nodes, Some(leaf)) -> 
            yield! [leaf]
            for n in nodes do yield! collect n
        | Node(nodes, none) ->
            for n in nodes do yield! collect n
        | Leaf(Some(leaf)) ->
            yield! [leaf]
        | Leaf(None) ->
            ()
    }

let collect_exp node = 
    match node with
    | Transformer(_) -> []
    | PHEV(phev_args) as node -> phev_args.profile.to_exp_float(phev_args.rate, phev_args.capacity)
    | PowerNode(_) -> []
    | BRP(_) -> []

let phev_expected = 
    FileManager.powergrid()
    |> map collect_exp 
    |> collect
    |> List.ofSeq
    |> List.filter (fun x -> x.Length > 0)
    |> List.sumn
    |> Array.ofList