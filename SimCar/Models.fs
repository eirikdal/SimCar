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
type dayahead = (energy array)
type realtime = (energy array)
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
    | Weibull
    | Gumbel

type Distribution = 
    { dist_type : DistributionType;
    mean : float;
    sigma : float;
    duration : int list;
    dist : float seq }

type Results = {
    phevs_sum : float;
    phevs_ux : float;
    pnodes_sum : float;
    total_max : float;
    total_avg : float;
    total_sum : float; 
    par : float;
    dayahead_sum : float; 
    dif : float; 
    ratio : float; 
    trf_delta : float;
    trf_filtered : float;}

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
//            let mu = log (profile.mean**2.0/sqrt(profile.sigma+profile.mean**2.0))
//            let sigma = sqrt (log(profile.sigma/(profile.mean**2.0)+1.0))
//            let n = new MathNet.Numerics.Distributions.LogNormal(mu, sigma)
//            n.CumulativeDistribution(time+1.0) - n.CumulativeDistribution((time))
            if time >= profile.mean then
                let n = new MathNet.Numerics.Distributions.LogNormal(0.0,profile.sigma)
                n.CumulativeDistribution((time-profile.mean)+1.0) - n.CumulativeDistribution(time-profile.mean)
            else 
                0.0
        | Weibull ->
            let n = new MathNet.Numerics.Distributions.Weibull(profile.sigma,5.0)
            n.CumulativeDistribution((time-profile.mean)+1.0) - n.CumulativeDistribution(time-profile.mean)
        | Gumbel ->
            if time >= profile.mean then
                let n = new MathNet.Numerics.Distributions.Gamma(profile.sigma,20.0)
                n.CumulativeDistribution((time-profile.mean)+1.0) - n.CumulativeDistribution(time-profile.mean)
            else 
                0.0
    // cache the distributions
    member self.calc(name, (profiles : Distribution list)) : Profile =         
        let dist_list = 
            profiles
            |> List.map (fun p -> { p with dist=(fun i -> self.sum((float i), p)) |> Seq.initInfinite |> Seq.take 96 |> Seq.cache })
    
        // calculate the accumulated probability density function of all distributions
        let prob ({dist=dist}) i = dist |> Seq.nth (i%96)
        let temp = Seq.initInfinite (fun i -> dist_list |> Seq.fold (fun ac (d : Distribution) -> ac + (prob d i)) 0.0) |> Seq.take 96 |> Array.ofSeq

        syncContext.RaiseDelegateEvent probEvent (name, temp)

        FloatProfile(name, dist_list)
    member self.float_profile() = 
        match self with
        | FloatProfile(dist_name,dist_list) ->
            self
        | DistProfile(name,dist_list) ->
            self.calc(name,dist_list)
    member self.to_float() = 
        let sum2 list1 list2 : float list = List.map2 (fun x y -> x+y) list1 list2
//            list1 |> List.map2 (fun sum t -> if (sum+t) < 1.0 then sum+t else 1.0) list2
        let sumn list = list |> List.fold (fun ac dist -> sum2 ac (List.ofSeq dist.dist)) (List.init (96) (fun _ -> 0.0))
        match self with 
        | FloatProfile(_, dist_list) -> 
            sumn dist_list
        | DistProfile(name,dist_list) ->
            self.calc(name,dist_list).to_float()
    member self.to_exp_float(charge_rate : float<kWh>, discharge_rate, capacity) : float<kWh> list =
        match self with 
        | FloatProfile(_,dist_list) ->
            let calc_for_dist (dist : Distribution) = 
                let dist' = dist.dist |> Array.ofSeq
                let duration = dist.duration
                
                let expected = Array.init(96) (fun _ -> 0.0<kWh>)

                let duration' = List.sortBy (fun x -> -x) duration
                let mutable sum_discharge = duration' |> List.sumBy (fun x -> if x = 1 then discharge_rate else 0.0<kWh>) 

                for i in 0 .. dist'.Length-1 do 
                    for j in 0 .. duration.Length-1 do 
                        if sum_discharge > 0.0<kWh> then
                            let temp = charge_rate * dist'.[i]
                            expected.[(i+j+duration'.Length)%96] <- expected.[(i+j+duration'.Length)%96] + temp
                            sum_discharge <- sum_discharge - temp

                expected |> List.ofArray

            dist_list 
            |> List.map (fun dist -> calc_for_dist dist)
            |> List.argmaxn

        | DistProfile(name,dist_list) ->
            self.calc(name,dist_list).to_exp_float(charge_rate, discharge_rate, capacity)
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
    charge_rate : energy;
    discharge_rate : energy;
    left : int;
    duration : int list;
    parent : string;
    intentions : energy list;
    failed : energy;
    histogram : int array }
    with 
        member self.leave(tick : int, duration : int list) : PhevArguments =  
            self.histogram.[(tick%96)] <- self.histogram.[(tick%96)] + 1

            syncContext.RaiseDelegateEvent phevLeft (tick%96, self.capacity-self.battery)
            syncContext.RaiseEvent phevBatteryLeft (tick%96, Energy.toFloat self.battery / Energy.toFloat self.capacity)
        
            { self with left=(tick%96); duration=duration; failed=self.failed+(self.capacity-self.battery); intentions=[]}
        member self.charge() = 
            match self.intentions with 
            | rate::t -> 
                let current' = if List.length self.duration <= 0 then rate else 0.0<kWh>
                if current'+self.battery <= self.capacity then 
                    { self with current=current'; battery=(self.battery+current'); intentions=t }
                else
                    { self with current=self.capacity-self.battery; battery=self.capacity; }
            | [] -> { self with current=0.0<kWh>; battery=self.battery; }
        member self.drive() =
            match self.duration with 
            | 1::t ->
                let battery' = if self.battery >= self.discharge_rate then self.battery - self.discharge_rate else 0.0<kWh>
                { self with current=0.0<kWh>; battery=battery'; duration=self.duration.Tail }
            | 0::t -> 
                { self with current=0.0<kWh>; duration=self.duration.Tail }
type TrfArguments = 
    { name : string; 
    capacity : capacity;
    current : energy;
    parent : string;
    filtered : energy;
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
        children=children |> List.map (fun child -> (child, Waiting));
        filtered=0.0<kWh> }

    Node(nodes, Some <| Transformer(trf_arg))

// function that creates a PHEV model, takes name, capacity, current and battery as parameters
let create_phev name capacity current battery charge_rate discharge_rate profile parent (profiles : Profile list) =
    let phev_arg = 
        { name=name;
        profile=List.find (fun (DistProfile(prof_name, dist)) -> prof_name = profile) profiles;
        capacity=Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture);
        current=Energy.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture);
        battery=Battery.ofFloat <| Double.Parse(battery, CultureInfo.InvariantCulture);
        charge_rate=Energy.ofFloat <| Double.Parse(charge_rate, CultureInfo.InvariantCulture);
        discharge_rate=Energy.ofFloat <| Double.Parse(discharge_rate, CultureInfo.InvariantCulture);
        histogram=Array.init (96) (fun i -> 0);
        duration=[];
        left=(-1);
        failed=0.0<kWh>;
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
        realtime=Array.init (96) (fun _ -> 0.0<kWh>);
        children=children |> List.map (fun child -> (child, Waiting)); }

    Node(nodes, Some <| BRP(brp_arg)) 

let create_distribution str_type mean sigma duration =
    let dist_type = 
        match str_type with
        | "gauss" | "normal" -> Normal
        | "lognormal" -> LogNormal
        | "weibull" -> Weibull
        | "gumbel" -> Gumbel
        | _ -> raise <| Exception("Undefined distribution")

    { dist_type=dist_type;
    mean=mean;
    sigma=sigma;
    duration=duration;
    dist=Seq.empty }
