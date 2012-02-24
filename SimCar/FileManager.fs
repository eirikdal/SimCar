﻿module FileManager

open System
open System.IO
open System.Globalization
open Models

let folder_of file = sprintf "%s\\%s" (Directory.GetParent(Directory.GetCurrentDirectory()).Parent.FullName) file

let read_file file = 
    seq {
        use sr = new StreamReader(folder_of file)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let rec create_powergrid trf_seq nodes (rest : string list byref) =
    match (trf_seq : string list) with 
    | h::t ->
        match h.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with
        | [|"trf";name;capacity;current|] ->
            let node = create_node name Seq.empty capacity current
            create_powergrid t (List.append nodes [node]) (&rest)
        | [|"trf";name;capacity;current;"{"|] -> 
            let node = create_node name (create_powergrid t [] &rest) capacity current
            create_powergrid rest (List.append nodes [node]) (&rest)
        | [|"phev";name;capacity;current;battery|] -> 
            let node = create_phev name capacity current battery
            create_powergrid t (List.append nodes [node]) (&rest)
        | [|"pnode";name;dayahead;realtime|] ->
            let node = create_powernode name dayahead realtime
            create_powergrid t (List.append nodes [node]) (&rest)
        | [|"}"|] -> 
            rest <- t
            nodes
        | _ -> raise <| IOException("Error while reading Transformers from file")
    | _ -> nodes

let powergrid = 
    let mutable rest = []
    let trfs = List.ofSeq (read_file "brp.txt")
    
    BRP("brp", create_powergrid trfs [] (&rest))