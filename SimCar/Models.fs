﻿module Models

#nowarn "25"

open Agent
open System
open System.Globalization

// scalar units
[<Measure>] type k  
[<Measure>] type M
// units of time
[<Measure>] type h
[<Measure>] type min
[<Measure>] type s
// units of power
[<Measure>] type W
[<Measure>] type kW = k*W
[<Measure>] type MW = M*W
[<Measure>] type Wh = W*h
[<Measure>] type kWh = kW*h
[<Measure>] type MWh = MW*h

// incorrect use:
//let v = 1.0<kWh> + 1.0<Wh>
//let v = 1.0<kWh> + 1.0
// correct use: 
//let v = 1.0<kWh> + 1.0<kWh>
//let v = 1.0<kWh> * 1.0<k>

type power = float<kW>
type energy = float<kWh>
type dayahead = (int -> energy)
type realtime = (int -> energy)
type capacity = energy
type current = energy
type battery = energy
type name = string

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Capacity = 
    let inline toFloat (value : energy) = float value
    let inline ofFloat (value : float) =
        if value < 0.0 then
            raise <| System.ArgumentOutOfRangeException ("value")
        else
            LanguagePrimitives.FloatWithMeasure<kWh> (float value)

module Battery = 
    let inline toFloat (value : energy) = float value
    let inline ofFloat (value : float) =
        if value < 0.0 then
            raise <| System.ArgumentOutOfRangeException ("value")
        else
            LanguagePrimitives.FloatWithMeasure<kWh> (float value)

module Energy = 
    let inline toFloat (value : energy) = float value
    let inline ofFloat (value : float) =
        if value > 10000000.0 || value < -10000000.0 then
            raise <| System.ArgumentOutOfRangeException ("value")
        else
            LanguagePrimitives.FloatWithMeasure<kWh> (float value)

type DistributionType = 
    | Normal
    | LogNormal

type Distribution = 
    { dist_type : DistributionType;
    mean : float;
    sigma : float;
    duration : int }

type Profile = 
    | DistProfile of string * Distribution list
    | FloatProfile of string * float seq

type Node<'T> = 
    | Node of (Node<'T> seq) * 'T option
    | Leaf of 'T option

type BrpArguments = 
    { name : string;
    dayahead : dayahead;
    current : energy }

type PhevArguments =
    { name : string;
    profile : Profile;
    capacity : capacity;
    current : energy;
    battery : battery;
    rate : energy;
    left : int;
    duration : int; }

type TrfArguments = 
    { name : string; 
    capacity : capacity;
    current : energy }

type PnodeArguments = 
    { name : string;
    realtime : realtime;
    current : energy }

type Grid = 
    | BRP of BrpArguments
    | Transformer of TrfArguments
    | PowerNode of PnodeArguments
    | PHEV of PhevArguments
    with 
    member self.name = 
        match self with
        | PHEV(phev_arg) -> phev_arg.name
        | Transformer(trf_arg) -> trf_arg.name
        | PowerNode(pnode_arg) -> pnode_arg.name
        | BRP(brp_arg) -> brp_arg.name

// dummy functions for dayahead and realtime mode, for testing purposes
let sine n = Energy.ofFloat <| sin (2.0 * Math.PI * (float n))
let gen = (Seq.initInfinite (fun x -> 1.0))
let take n = sine <| Seq.nth n gen

// function that creates a transformer model, takes name, other connected nodes, capacity and current as parameters
let create_node name nodes capacity current = 
    let trf_arg = 
        { name=name;
        TrfArguments.capacity=Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture);
        TrfArguments.current=Energy.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture) }

    Node(nodes, Some <| Transformer(trf_arg))

// function that creates a PHEV model, takes name, capacity, current and battery as parameters
let create_phev name capacity current battery rate profile (profiles : Profile seq) =
    let phev_arg = 
        { name=name;
        profile=Seq.find (fun (DistProfile(prof_name, dist)) -> prof_name = profile) profiles;
        capacity=Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture);
        current=Energy.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture);
        battery=Battery.ofFloat <| Double.Parse(battery, CultureInfo.InvariantCulture);
        rate=Energy.ofFloat <| Double.Parse(rate, CultureInfo.InvariantCulture);
        duration=(-1);
        left=(-1) }
    Node(Seq.empty, Some <| PHEV(phev_arg))

let create_powernode name realtime = 
    let pnode_arg =
        { name=name;
        realtime=realtime;
        current=0.0<kWh>; }
    Node(Seq.empty, Some <| PowerNode(pnode_arg))

let create_brp name nodes dayahead = 
    let brp_arg : BrpArguments = 
        { name=name;
        dayahead=dayahead;
        current=0.0<kWh> }

    Node(nodes, Some <| BRP(brp_arg)) 

let create_distribution str_type mean sigma duration =
    let dist_type = 
        match str_type with
        | "gauss" -> Normal
        | "lognormal" -> LogNormal
        | _ -> raise <| Exception("Undefined distribution")

    { dist_type=dist_type;
    mean=mean;
    sigma=sigma;
    duration=duration }