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
        List.iter (fun n -> iter iterf n) nodes
    | Leaf(Some(leaf)) ->
        iterf leaf
    | Node(nodes, None) ->
        Seq.iter (fun n -> iter iterf n) nodes
    | Leaf(None) ->
        ()

// Traverse a tree of models, creating a mirrored tree of agents as we go along
// this function should be avoided as much as possible, and should only be used when
// a reply needs to be guaranteed. This is because of performance reasons.
let rec send_reply msg (node : Node<Agent<Message>>) = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let list = List.map (fun n -> send_reply msg n) nodes
        let res = leaf.PostAndReply((fun replyChannel -> ReplyTo(msg, replyChannel)), 5000)
        Node(list, Some (leaf, res))
    | Node(nodes, None) -> 
        Node(List.map (fun n -> send_reply msg n) nodes, None)
    | Leaf(Some(leaf)) ->
        let res = leaf.PostAndReply((fun replyChannel -> ReplyTo(msg, replyChannel)), 5000)
        Leaf(Some <| (leaf, res))
    | Leaf(None) ->
        Leaf(None)

// traverse a tree of models, creating a mirrored tree of agents as we go along
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

//let rec collect msg node = 
//    fetch msg node 