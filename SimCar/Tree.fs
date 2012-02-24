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

// make the right kind of agent for a given node
let make_agent node = 
    match node with
    | Transformer(_,_,_,_) ->
        trf_agent node
    | PHEV(_,_,_,_) ->
        phev_agent node
    | PowerNode(_,_,_) ->
        pnode_agent node

// traverse a tree of models, creating a mirrored tree of agents as we go along
let rec to_agents node = 
    match node with
    | Transformer(_,nodes,_,_) ->
        Node(Seq.map (fun n -> to_agents n) nodes, Some <| make_agent node)
    | PowerNode(_,_,_) ->
        Leaf(Some <| make_agent node)
    | PHEV(_,_,_,_) ->
        Leaf(Some <| make_agent node)

// traverse a tree of nodes, applying function iterf to each node
let rec iterTree node iterf = 
    match node with
    | Node(nodes, Some(leaf)) ->
        iterf leaf
        Seq.iter (fun n -> iterTree n iterf) nodes
    | Leaf(Some(leaf)) ->
        iterf leaf
    | Node(nodes, None) ->
        Seq.iter (fun n -> iterTree n iterf) nodes
    | Leaf(None) ->
        ()

// traverse a tree of models, creating a mirrored tree of agents as we go along
let rec mapAgents (node : Node<Agent<_ Message>>) msg = 
    try 
        match node with
        | Node(nodes, Some(leaf)) ->
            let res = leaf.PostAndReply((fun replyChannel -> ReplyTo(msg, replyChannel)), 1000)
            Node(Seq.map (fun n -> mapAgents n msg) nodes, Some res)
        | Node(nodes, None) -> 
             Node(Seq.map (fun n -> mapAgents n msg) nodes, None)
        | Leaf(Some(leaf)) ->
            let res = leaf.PostAndReply((fun replyChannel -> ReplyTo(msg, replyChannel)), 1000)
            Leaf(Some <| res)
        | Leaf(None) ->
            Leaf(None)
    with 
    | :? TimeoutException -> 
        syncContext.RaiseEvent error <| Exception(sprintf "Agent timed out")
        Leaf(None)

let rec mapTree node mapf = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let res = mapf leaf
        Node(Seq.map (fun n -> mapTree n mapf) nodes, res)
    | Leaf(Some(leaf)) ->
        Leaf(mapf leaf)
    | Node(nodes, None) ->
        Node(Seq.map (fun n -> mapTree n mapf) nodes, None)
    | Leaf(None) ->
        Leaf(None)


let rec collectTree (node : Node<'a Message>) = 
    seq<'a Message> {
        match node with 
        | Node(nodes, Some(msg)) -> 
            yield! [msg]
            for n in nodes do yield! collectTree n
        | Node(nodes, none) ->
            for n in nodes do yield! collectTree n
        | Leaf(Some(msg)) ->
            yield! [msg]
        | Leaf(None) ->
            ()
    }