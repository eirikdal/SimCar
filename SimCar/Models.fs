module Models

// Units of effect
[<Measure>] type h
[<Measure>] type W
[<Measure>] type kW
[<Measure>] type MW
[<Measure>] type Wh = W * h
[<Measure>] type kWh = kW * h
[<Measure>] type MWh = MW * h

type capacity = float<kWh>
type current = float<kWh>
type battery = float<kWh>
type name = string

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Capacity = 
    let inline toFloat (value : capacity) = float value
    let inline ofFloat value =
        if value < 0.0 then
            raise <| System.ArgumentOutOfRangeException ("value")
        else
            LanguagePrimitives.FloatWithMeasure<kWh> value

module Battery = 
    let inline toFloat (value : capacity) = float value
    let inline ofFloat value =
        if value < 0.0 then
            raise <| System.ArgumentOutOfRangeException ("value")
        else
            LanguagePrimitives.FloatWithMeasure<kWh> value

module Current = 
    let inline toFloat (value : capacity) = float value
    let inline ofFloat value =
        if value > 10000000.0 || value < -10000000.0 then
            raise <| System.ArgumentOutOfRangeException ("value")
        else
            LanguagePrimitives.FloatWithMeasure<kWh> value

type Transformer = 
    | Transformer_Node of (Transformer seq) * name * capacity * current
    | Transformer_Leaf of Transformer * name * (GridNode seq) option * (PHEV seq) option * capacity * current
and GridNode = 
    | GridNode of Transformer * name
and PHEV = 
    | PHEV of Transformer option * name * capacity * current * battery