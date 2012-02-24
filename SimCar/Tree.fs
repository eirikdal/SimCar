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
let rec iter node iterf = 
    match node with
    | Node(nodes, Some(leaf)) ->
        iterf leaf
        Seq.iter (fun n -> iter n iterf) nodes
    | Leaf(Some(leaf)) ->
        iterf leaf
    | Node(nodes, None) ->
        Seq.iter (fun n -> iter n iterf) nodes
    | Leaf(None) ->
        ()

// traverse a tree of models, creating a mirrored tree of agents as we go along
let rec send (node : Node<Agent<_ Message>>) msg = 
    try 
        match node with
        | Node(nodes, Some(leaf)) ->
            let res = leaf.PostAndReply((fun replyChannel -> ReplyTo(msg, replyChannel)), 1000)
            Node(Seq.map (fun n -> send n msg) nodes, Some res)
        | Node(nodes, None) -> 
             Node(Seq.map (fun n -> send n msg) nodes, None)
        | Leaf(Some(leaf)) ->
            let res = leaf.PostAndReply((fun replyChannel -> ReplyTo(msg, replyChannel)), 1000)
            Leaf(Some <| res)
        | Leaf(None) ->
            Leaf(None)
    with 
    | :? TimeoutException -> 
        syncContext.RaiseEvent error <| Exception(sprintf "Agent timed out")
        Leaf(None)

let rec map node mapf = 
    match node with
    | Node(nodes, Some(leaf)) ->
        let res = mapf leaf
        Node(Seq.map (fun n -> map n mapf) nodes, res)
    | Leaf(Some(leaf)) ->
        Leaf(mapf leaf)
    | Node(nodes, None) ->
        Node(Seq.map (fun n -> map n mapf) nodes, None)
    | Leaf(None) ->
        Leaf(None)

let fold node foldf op : float = 
    let rec _foldTree node foldf res =
        match node with
        | Node(nodes, Some(leaf)) ->
            let v = op (foldf leaf) res
            _foldTree node foldf v
        | Leaf(Some(leaf)) ->
            op (foldf leaf) res
        | Node(nodes, None) ->
            _foldTree node foldf res
        | Leaf(None) ->
            res
    _foldTree node foldf 0.0

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