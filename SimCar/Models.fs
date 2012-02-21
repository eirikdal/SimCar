﻿module Models

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
type dayahead = float<kW*h>
type realtime = float<kW*h>
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
 
type Node = 
    | Node of name * (Node seq) * capacity * current
    | PowerNode of name * dayahead * realtime
    | PHEV of name * capacity * current * battery
    with 
    member self.name = 
        match self with
        | PHEV(name,_,_,_) -> name
        