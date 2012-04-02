module Models

#nowarn "25"

open Agent
open System
open System.Globalization
open SynchronizationContext
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions


// scalar units
[<Measure>] type k
[<Measure>] type M
// units of time
[<Measure>] type tick
[<Measure>] type day
[<Measure>] type week
[<Measure>] type h
[<Measure>] type min

let tick_to_min tick = tick / 15<min/tick>
let min_per_tick : int<min/tick> = 15<min/tick>

// units of power
[<Measure>] type A
[<Measure>] type V
[<Measure>] type W = A*V
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
    let inline ofFloat (value : float) = LanguagePrimitives.FloatWithMeasure<kWh> (float value)
    let inline toKilo (value : float<W>) = LanguagePrimitives.FloatWithMeasure<kWh> ((float value) / 1000.0)

module Current = 
    let inline toFloat (value : energy) = float value
    let inline ofFloat (value : float) = LanguagePrimitives.FloatWithMeasure<A> (float value)

module Voltage = 
    let inline toFloat (value : energy) = float value
    let inline ofFloat (value : float) = LanguagePrimitives.FloatWithMeasure<V> (float value)

//let (+) (a:float<kW>) (b:float<kW>) = Energy.ofFloat ((float a) + 1000.0*(float b))

type DistributionType = 
    | Normal
    | LogNormal

type Distribution = 
    { dist_type : DistributionType;
    mean : float;
    sigma : float;
    duration : int;
    dist : float seq }

type Profile = 
    | DistProfile of string * Distribution list
    | FloatProfile of string * Distribution list
    with 
    member self.sum(time,profile) = 
        match profile.dist_type with
        | Normal ->
            let n = new Normal(profile.mean, profile.sigma)
            n.CumulativeDistribution(time) - n.CumulativeDistribution((time-1.0))
        | LogNormal ->
            let n = new MathNet.Numerics.Distributions.Gamma(profile.mean, 7.0)
            n.CumulativeDistribution(time+1.0) - n.CumulativeDistribution((time))
    // cache the distributions
    member self.calc(name, (profiles : Distribution list)) : Profile =         
        let dist_list = 
            profiles
            |> List.map (fun p -> { p with dist=(fun i -> self.sum((float i), p)) |> Seq.initInfinite |> Seq.take 96 |> Seq.cache })
    
        // calculate the accumulated probability density function of all distributions
        let prob ({dist=dist}) i = dist |> Seq.nth (i%96)
        let temp = Seq.initInfinite (fun i -> dist_list |> Seq.fold (fun ac (d : Distribution) -> ac + (prob d i)) 0.0) |> Seq.take 96 |> Array.ofSeq

        probEvent.Trigger [|box temp; box System.EventArgs.Empty|]

        FloatProfile(name, dist_list)
    member self.float_profile() = 
        match self with
        | FloatProfile(dist_name,dist_list) ->
            self
        | DistProfile(name,dist_list) ->
            self.calc(name,dist_list)
    member self.to_float() = 
        let sum2 list1 list2 = 
            list1 |> List.map2 (fun sum t -> if (sum+t) < 1.0 then sum+t else 1.0) list2
        let sumn list = list |> List.fold (fun ac dist -> sum2 ac (List.ofSeq dist.dist)) (List.init (96) (fun _ -> 0.0))
        match self with 
        | FloatProfile(_, dist_list) -> 
            sumn dist_list
        | DistProfile(name,dist_list) ->
            self.calc(name,dist_list).to_float()
    member self.to_exp_float(rate) =
        match self with 
        | FloatProfile(_,dist_list) ->
            let calc_for_dist (dist : Distribution) = 
                let dist' = dist.dist |> Array.ofSeq
                let duration = dist.duration
                // calculate expected load
                let windows_of_expected_trips = Seq.init (96+(duration-1)) (fun i -> (i,0.0)) |> Seq.windowed (duration) |> Array.ofSeq

                let v = Array.init (96) (fun _ -> 0.0)
                
                let windows_of_expected = 
                    windows_of_expected_trips 
                    |> Array.mapi (fun i window -> window |> Array.map (fun (i',_) -> if i < 96 then (i', rate * dist'.[i]) else (i', 0.0)))
                
                Array.init (96) (fun i ->
                    windows_of_expected
                    |> Array.fold (fun ac window -> 
                        ac + (window |> Array.fold (fun ac' (i', rate') -> if i = i' then ac'+rate' else ac') 0.0)) 0.0)
                |> List.ofArray

            let dist_with_exp = dist_list |> List.map (fun dist -> calc_for_dist dist)
            dist_with_exp
        | DistProfile(name,dist_list) ->
            self.calc(name,dist_list).to_exp_float(rate)
type Node<'T> = 
    | Node of (Node<'T> list) * 'T option
    | Leaf of 'T option

type Status = 
    | OK
    | Waiting

type BrpArguments = 
    { name : string;
    dayahead : dayahead;
    realtime : realtime;
    current : energy;
    children : (string * Status) list; }

type PhevInfo = 
    | Status of capacity * battery * energy

type PhevStatus = 
    | Status of int * int


type PhevArguments =
    { name : string;
    profile : Profile;
    capacity : capacity;
    current : energy;
    battery : battery;
    rate : energy;
    left : int;
    duration : int;
    parent : string;
    intentions : energy list;
    histogram : int array }
    with 
        member self.leave(tick : int, duration : int) : PhevArguments =  
            self.histogram.[(tick%96)] <- self.histogram.[(tick%96)] + 1

            if self.battery < self.capacity && (self.capacity - self.battery) >= self.rate then 
                syncContext.RaiseDelegateEvent phevFailed (tick%96)

            syncContext.RaiseDelegateEvent phevLeft (tick%96)
        
            { self with left=(tick%96); duration=duration; intentions=[]}
        member self.charge() = 
            match self.intentions with 
            | rate::t -> 
                let current' = if self.duration <= 0 then rate else 0.0<kWh>
                { self with current=current'; battery=(self.battery+current'); intentions=t }
            | [] -> { self with current=0.0<kWh>; battery=self.battery; }
        member self.drive() =
//            { self with current=0.0<kWh>; battery=(self.battery - self.rate); duration=self.duration-1 }
            let battery' = if self.battery >= self.rate then self.battery - self.rate else 0.0<kWh>
            { self with current=0.0<kWh>; battery=battery'; duration=self.duration-1 }

type TrfArguments = 
    { name : string; 
    capacity : capacity;
    current : energy;
    parent : string;
    children : (string * Status) list; }

type PnodeArguments = 
    { name : string;
    realtime : realtime;
    current : energy;
    parent : string; }

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

// function that creates a transformer model, takes name, other connected nodes, capacity and current as parameters
let create_node name nodes capacity current parent children = 
    let trf_arg = 
        { name=name;
        TrfArguments.capacity=Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture);
        TrfArguments.current=Energy.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture);
        parent=parent;
        children=children |> List.map (fun child -> (child, Waiting)); }

    Node(nodes, Some <| Transformer(trf_arg))

// function that creates a PHEV model, takes name, capacity, current and battery as parameters
let create_phev name capacity current battery rate profile parent (profiles : Profile list) =
    let phev_arg = 
        { name=name;
        profile=List.find (fun (DistProfile(prof_name, dist)) -> prof_name = profile) profiles;
        capacity=Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture);
        current=Energy.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture);
        battery=Battery.ofFloat <| Double.Parse(battery, CultureInfo.InvariantCulture);
        rate=Energy.ofFloat <| Double.Parse(rate, CultureInfo.InvariantCulture);
        histogram=Array.init (96) (fun i -> 0);
        duration=(-1);
        left=(-1);
        parent=parent;
        intentions=[] }
    Node(List.empty, Some <| PHEV(phev_arg))

let create_powernode name realtime parent = 
    let pnode_arg =
        { name=name;
        realtime=realtime;
        current=0.0<kWh>;
        parent=parent; }
    Node(List.empty, Some <| PowerNode(pnode_arg))

let create_brp name nodes dayahead children = 
    let brp_arg : BrpArguments = 
        { name=name;
        dayahead=dayahead;
        current=0.0<kWh>;
        realtime=Array.init (96) (fun _ -> 0.0<kWh>) |> Array.get;
        children=children |> List.map (fun child -> (child, Waiting)); }

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
    duration=duration;
    dist=Seq.empty }
