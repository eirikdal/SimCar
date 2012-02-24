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
// let v = 1.0<kW*h> * 1.0

type intent = float<kW*h>
type capacity = float<kW*h>
type current = float<kW*h>
type battery = float<kW*h>
type dayahead = (int -> float<kW*h>)
type realtime = (int -> float<kW*h>)
type name = string

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Capacity = 
    let inline toFloat (value : capacity) = float value
    let inline ofFloat (value : float) =
        if value < 0.0 then
            raise <| System.ArgumentOutOfRangeException ("value")
        else
            LanguagePrimitives.FloatWithMeasure<kW*h> (float value)

module Battery = 
    let inline toFloat (value : capacity) = float value
    let inline ofFloat (value : float) =
        if value < 0.0 then
            raise <| System.ArgumentOutOfRangeException ("value")
        else
            LanguagePrimitives.FloatWithMeasure<kW*h> (float value)

module Current = 
    let inline toFloat (value : capacity) = float value
    let inline ofFloat (value : float) =
        if value > 10000000.0 || value < -10000000.0 then
            raise <| System.ArgumentOutOfRangeException ("value")
        else
            LanguagePrimitives.FloatWithMeasure<kW*h> (float value)

type Node<'T> = 
    | Node of (Node<'T> seq) * 'T option
    | Leaf of 'T option

type Grid = 
    | BRP of name * (Grid seq)
    | Transformer of name * (Grid seq) * capacity * current
    | PowerNode of name * dayahead * realtime
    | PHEV of name * capacity * current * battery
    with 
    member self.name = 
        match self with
        | PHEV(name,_,_,_) -> name

// function that creates a transformer model, takes name, other connected nodes, capacity and current as parameters
let create_node name nodes capacity current = 
    Transformer(name, nodes, Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture), Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture))

// function that creates a PHEV model, takes name, capacity, current and battery as parameters
let create_phev name capacity current battery =
    PHEV(name, Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture),
        Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture),
        Battery.ofFloat <| Double.Parse(battery, CultureInfo.InvariantCulture))

let gen = (Seq.initInfinite (fun x -> Capacity.ofFloat 1.0))
let take n = gen |> Seq.nth n

let create_powernode name capacity current = 
    PowerNode(name, take, take)
