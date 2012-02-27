﻿module FileManager

open System
open System.IO
open System.Globalization
open Models
open System.Text.RegularExpressions

let folder_of file = sprintf "%s\\%s" (Directory.GetParent(Directory.GetCurrentDirectory()).Parent.FullName) file

let read_file file = 
    seq {
        use sr = new StreamReader(folder_of file)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|Float|_|) (str: string) =
   let mutable floatvalue = 0.0
   if System.Double.TryParse(str, &floatvalue) then Some(floatvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let parse_dist str = 
    match str with 
    | ParseRegex "mean=(\d){1,2}:(\d){1,2}" [Float h; Float m] ->
        h + m
    | ParseRegex "std=(\d)+" [Float std] ->
        std
    | ParseRegex "duration=(\d)+" [Float duration] ->
        duration
    | _ -> raise <| Exception "Parsing failed"

let rec parse_profile stream (dist : Distribution list) (rest : string list byref) = 
    match (stream : string list) with 
    | h::t ->
        match h.Split([|' ';','|], StringSplitOptions.RemoveEmptyEntries) with
        | [|dist_type;mean;std;duration|] ->
            let temp = create_distribution dist_type (parse_dist mean) (parse_dist std) (int <| parse_dist duration)
            parse_profile t (List.append dist [temp]) (&rest)
        | [|"}"|] -> 
            rest <- t
            dist
        | _ -> raise <| Exception "Unexpected end of stream."
    | _ -> raise <| Exception("Unexpected end of stream. Maybe missing closing '}'?")
            
let rec parse_profiles stream profiles (rest : string list byref) =
    match (stream : string list) with 
    | h::t ->
        match h.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with
        | [|profile;"{"|] ->
            let profile = DistProfile(profile, parse_profile t [] (&rest))
            parse_profiles rest (List.append [profile] profiles) (&rest)
        | _ -> 
            rest <- t
            profiles
    | _ -> profiles

let profiles : Profile list = 
    let mutable rest = []
    let stream = List.ofSeq (read_file "profiles.txt")

    parse_profiles stream [] (&rest)

// 
// Parsing the powergrid, transformers, power nodes and PHEVs.
//
let rec parse_powergrid stream nodes (rest : string list byref) =
    match (stream : string list) with 
    | h::t ->
        match h.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with
        | [|"trf";name;capacity;current|] ->
            let node = create_node name Seq.empty capacity current
            parse_powergrid t (List.append nodes [node]) (&rest)
        | [|"trf";name;capacity;current;"{"|] -> 
            let node = create_node name (parse_powergrid t [] &rest) capacity current
            parse_powergrid rest (List.append nodes [node]) (&rest)
        | [|"phev";name;profile;capacity;current;battery|] -> 
            let node = create_phev name capacity current battery profile profiles
            parse_powergrid t (List.append nodes [node]) (&rest)
        | [|"pnode";name;dayahead;realtime|] ->
            let node = create_powernode name dayahead realtime
            parse_powergrid t (List.append nodes [node]) (&rest)
        | [|"}"|] -> 
            rest <- t
            nodes
        | _ -> raise <| IOException("Error while reading Transformers from file")
    | _ -> nodes

let powergrid = 
    let mutable rest = []
    let stream = List.ofSeq (read_file "brp.txt")
    
    create_brp "brp" (parse_powergrid stream [] (&rest)) Models.take