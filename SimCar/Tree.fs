module Tree

open Agent
open Message
open System
open System.Globalization
open SynchronizationContext
open Models

// traverse a tree of nodes, applying function iterf to each node
let rec iter iterf node = 
    match node with
    | Node(nodes, Some(leaf)) ->
        iterf leaf
        Seq.iter (fun n -> iter iterf n) nodes
    | Leaf(Some(leaf)) ->
        iterf leaf
    | Node(nodes, None) ->
        Seq.iter (fun n -> iter iterf n) nodes
    | Leaf(None) ->
        ()

// traverse a tree of models, creating a mirrored tree of agents as we go along
let rec send_and_reply msg (node : Node<Agent<Message>>) = 
    try 
        match node with
        | Node(nodes, Some(leaf)) ->
            let res = leaf.PostAndReply((fun replyChannel -> ReplyTo(msg, replyChannel)), 1000)
            Node(Seq.map (fun n -> send_and_reply msg n) nodes |> Seq.cache, Some (leaf, res))
        | Node(nodes, None) -> 
            Node(Seq.map (fun n -> send_and_reply msg n) nodes, None)
        | Leaf(Some(leaf)) ->
            let res = leaf.PostAndReply((fun replyChannel -> ReplyTo(msg, replyChannel)), 1000)
            Leaf(Some <| (leaf, res))
        | Leaf(None) ->
            Leaf(None)
    with 
    | :? TimeoutException -> 
        syncContext.RaiseEvent error <| Exception(sprintf "Agent timed out")
        Leaf(None)

// traverse a tree of models, creating a mirrored tree of agents as we go along
let rec send msg (node : Node<Agent<Message>>) = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let res = leaf.Post(msg)
        Node(Seq.map (fun n -> send msg n) nodes |> Seq.cache, Some <| leaf)
    | Node(nodes, None) -> 
        Node(Seq.map (fun n -> send msg n) nodes |> Seq.cache, None)
    | Leaf(Some(leaf)) ->
        let res = leaf.Post(msg)
        Leaf(Some <| leaf)
    | Leaf(None) ->
        Leaf(None)

let rec map mapf node = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let res = mapf leaf
        Node(Seq.map (fun n -> map mapf n) nodes |> Seq.cache, Some(res))
    | Leaf(Some(leaf)) ->
        Leaf(Some(mapf leaf))
    | Node(nodes, None) ->
        Node(Seq.map (fun n -> map mapf n) nodes |> Seq.cache, None)
    | Leaf(None) ->
        Leaf(None)

// fold left on a tree using preorder traversal
let rec foldl op res node = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let temp = op leaf
        Seq.fold (fun (ac : energy) n -> ac + foldl op temp n) temp nodes
    | Leaf(Some(leaf)) ->
        op leaf
    | Node(nodes, None) ->
        Seq.fold (fun (ac : energy) n -> ac + foldl op res n) res nodes
    | Leaf(None) ->
        res

// fold right on a tree using inorder traversal
let rec foldr op node : float<kWh> = 
    match node with 
    | Node(nodes, Some(leaf)) ->
        let f = Seq.fold (fun ac f -> ac + f) 0.0<kWh> (seq { for n in nodes do yield foldr op n })
        op leaf f
    | Leaf(Some(leaf)) ->
        op leaf 0.0<kWh>
    | Node(nodes, None) -> 
        Seq.fold (fun ac f -> ac + f) 0.0<kWh> (seq { for n in nodes do yield foldr op n })
    | Leaf(None) -> 
        0.0<kWh>
        
let rec collect (node : Node<Message>) = 
    seq<Message> {
        match node with 
        | Node(nodes, Some(msg)) -> 
            yield! [msg]
            for n in nodes do yield! collect n
        | Node(nodes, none) ->
            for n in nodes do yield! collect n
        | Leaf(Some(msg)) ->
            yield! [msg]
        | Leaf(None) ->
            ()
    }