module Models

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
[<Measure>] type kW
[<Measure>] type MW
[<Measure>] type Wh
[<Measure>] type kWh
[<Measure>] type MWh

// incorrect use:
// let v = 1.0<kW*h> + 1.0<W*h>
// let v = 1.0<kW*h> + 1.0
// correct use: 
// let v = 1.0<kW*h> + 1.0<kW*h>
// let v = 1.0<kW*h> * 1.0<k>

type power = float<kW>
type energy = float<kW*h>
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
            LanguagePrimitives.FloatWithMeasure<kW*h> (float value)

module Battery = 
    let inline toFloat (value : energy) = float value
    let inline ofFloat (value : float) =
        if value < 0.0 then
            raise <| System.ArgumentOutOfRangeException ("value")
        else
            LanguagePrimitives.FloatWithMeasure<kW*h> (float value)

module Current = 
    let inline toFloat (value : energy) = float value
    let inline ofFloat (value : float) =
        if value > 10000000.0 || value < -10000000.0 then
            raise <| System.ArgumentOutOfRangeException ("value")
        else
            LanguagePrimitives.FloatWithMeasure<kW*h> (float value)

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
    dayahead : dayahead }

type PhevArguments =
    { name : string;
    profile : Profile;
    capacity : capacity;
    current : current;
    battery : battery }

type TrfArguments = 
    { name : string; 
    capacity : capacity;
    current : current }

type PnodeArguments = 
    { name : string;
    dayahead : dayahead;
    realtime : realtime }

type Grid = 
    | BRP of BrpArguments * (Grid seq) 
    | Transformer of TrfArguments * (Grid seq) 
    | PowerNode of PnodeArguments
    | PHEV of PhevArguments
    with 
    member self.name = 
        match self with
        | PHEV(phev_arg) -> phev_arg.name
        | Transformer(trf_arg, _) -> trf_arg.name
        | PowerNode(pnode_arg) -> pnode_arg.name
        | BRP(brp_arg,_) -> brp_arg.name

// dummy functions for dayahead and realtime mode, for testing purposes
let sine n = Current.ofFloat <| sin (2.0 * Math.PI * (float n))
let gen = (Seq.initInfinite (fun x -> 1.0))
let take n = sine <| Seq.nth n gen

// function that creates a transformer model, takes name, other connected nodes, capacity and current as parameters
let create_node name nodes capacity current = 
    let trf_arg = 
        { name=name;
        TrfArguments.capacity=Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture);
        TrfArguments.current=Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture) }

    Transformer(trf_arg, nodes)

// function that creates a PHEV model, takes name, capacity, current and battery as parameters
let create_phev name capacity current battery profile (profiles : Profile seq) =
    let phev_arg = 
        { name=name;
        profile=Seq.find (fun (DistProfile(prof_name, dist)) -> prof_name = profile) profiles;
        capacity=Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture);
        current=Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture);
        battery=Battery.ofFloat <| Double.Parse(battery, CultureInfo.InvariantCulture); }
    PHEV(phev_arg)

let create_powernode name dayahead realtime = 
    let pnode_arg =
        { name=name;
        dayahead=take;
        realtime=take; }
    PowerNode(pnode_arg)

let create_brp name nodes dayahead = 
    let brp_arg : BrpArguments = 
        { name=name;
        dayahead=dayahead }

    BRP(brp_arg,nodes)

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