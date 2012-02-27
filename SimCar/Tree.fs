module Tree

open Agent
open Message
open System
open System.Globalization
open SynchronizationContext
open PHEV
open Transformer
open PowerNode
open Models
open BRP

// make the right kind of agent for a given node
let make_agent node = 
    match node with
    | Transformer(_,_) ->
        trf_agent node
    | PHEV(_) ->
        phev_agent node
    | PowerNode(_) ->
        pnode_agent node
    | BRP(_,_) ->
        brp_agent node

// traverse a tree of models, creating a mirrored tree of agents as we go along
let rec to_agents node = 
    match node with
    | Transformer(_,nodes) ->
        Node(Seq.map (fun n -> to_agents n) nodes, Some <| make_agent node)
    | PowerNode(_) ->
        Leaf(Some <| make_agent node)
    | PHEV(_) ->
        Leaf(Some <| make_agent node)
    | BRP(_,nodes) ->
        Node(Seq.map (fun n -> to_agents n) nodes, Some <| make_agent node)

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
let rec send_and_reply msg (node : Node<Agent<_ Message>>) = 
    try 
        match node with
        | Node(nodes, Some(leaf)) ->
            let res = leaf.PostAndReply((fun replyChannel -> ReplyTo(msg, replyChannel)), 1000)
            Node(Seq.map (fun n -> send_and_reply msg n) nodes, Some (leaf, res))
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
let rec send msg (node : Node<Agent<_ Message>>) = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let res = leaf.Post(msg)
        Node(Seq.map (fun n -> send msg n) nodes, Some <| leaf)
    | Node(nodes, None) -> 
        Node(Seq.map (fun n -> send msg n) nodes, None)
    | Leaf(Some(leaf)) ->
        let res = leaf.Post(msg)
        Leaf(Some <| leaf)
    | Leaf(None) ->
        Leaf(None)

let rec map mapf node = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let res = mapf leaf
        Node(Seq.map (fun n -> map mapf n) nodes, Some(res))
    | Leaf(Some(leaf)) ->
        Leaf(Some(mapf leaf))
    | Node(nodes, None) ->
        Node(Seq.map (fun n -> map mapf n) nodes, None)
    | Leaf(None) ->
        Leaf(None)

let rec foldf op res node = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let temp = op leaf
        Seq.fold (fun (ac : energy) n -> ac + foldf op temp n) temp nodes
    | Leaf(Some(leaf)) ->
        op leaf
    | Node(nodes, None) ->
        Seq.fold (fun (ac : energy) n -> ac + foldf op res n) res nodes
    | Leaf(None) ->
        res

let rec collect (node : Node<'a Message>) = 
    seq<'a Message> {
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