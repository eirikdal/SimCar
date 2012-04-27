#r "bin/Debug/SimCar.dll"

open Models
open System
open System.IO
open System.Globalization
open FileManager

#nowarn "25"

let profile_file = "C:\\SimCar\\SimCar\\data\\powerprofiles.txt"
let brp_file = "C:\\SimCar\\SimCar\\data\\brp2.txt"

let high_watt = 155.0*8.0;
let med_watt = 7.2*8.0;

let transformer_ampere  = [8.; 8.; 8.] |> List.map (fun x -> Current.ofFloat x)
let transformer_voltage = [Voltage.ofFloat 0.240; Voltage.ofFloat 7.2; Voltage.ofFloat 155.0]
//let trf_cap = List.map2 (fun (x : float<A>) (y : float<V>) -> Energy.toKilo (x * y)) transformer_ampere transformer_voltage
let trf_cap = [8.<A>*0.24<V>; 8.<A>*7.2<V>]

type Transformer = 
    | LOW of string * float
    | MED of int * float * Transformer list
    | HIGH of int * float * Transformer list
    | PHEV of string

let phev_ratio = 0.15

let rand = new System.Random()

let make_grid = 
    let make_meds (MED(med_name,cur,lows) as med) meds (LOW(trf_name,trf_peak) as low) = 
        if (trf_peak+cur < med_watt) then
            if rand.NextDouble() < phev_ratio then 
                let phev = PHEV(sprintf "phev_%s" trf_name)
                (MED(med_name,trf_peak+cur,low::phev::lows)),meds
            else
                (MED(med_name,trf_peak+cur,low::lows)),meds                
        else
            if rand.NextDouble() < phev_ratio then 
                let phev = PHEV(sprintf "phev_%s" trf_name)
                (MED(med_name+1,0.0,[])),((MED(med_name,trf_peak+cur,low::phev::lows))::meds)
            else
                (MED(med_name+1,0.0,[])),((MED(med_name,trf_peak+cur,low::lows))::meds)          

    let make_highs (HIGH(high_name,high_cur,meds) as high) highs (MED(trf_name,med_cur,_) as med) = 
        if (med_cur+high_cur < high_watt) then
            (HIGH(high_name,med_cur+high_cur,med::meds)),highs
        else
            (HIGH(high_name+1,0.0,[])),(HIGH(high_name,med_cur+high_cur,med::meds)::highs)

    let profiles = 
        Parsing.powerprofiles
        |> List.map (fun (name, powerlist) -> (name, Array.max powerlist))
        |> List.filter (fun (_,peak) -> if peak <= 10.0 then true else false)
        |> List.map (fun ((name, peak) as node) -> LOW(name, peak))
        |> List.fold (fun (med,meds) trf -> make_meds med meds trf) (MED(0,0.0,[]),[])
        |> (fun (med,meds) -> med::meds)
        |> List.fold (fun (high,highs) med -> make_highs high highs med) (HIGH(0,0.0,[]),[])
        |> (fun (high,highs) -> high::highs)

    let rec test nodes = 
        match nodes with 
        | HIGH(n,_,children) ->
            if children.Length > 0 then
                let str = sprintf "trf high_%d %f %f {" n high_watt 0.0
                let test = str::([for node in children do yield! test node])
                List.append test ["}"]
            else
                []
        | MED(n,_,children) -> 
            let str = sprintf "trf med_%d %f %f {" n med_watt 0.0
            let test = str::([for node in children do yield! test node])
            List.append test ["}"]
        | LOW(name,_) -> 
            [yield sprintf "pnode node_%s %s" name name]
        | PHEV(name) -> 
            if rand.NextDouble() < 0.2 then 
                [yield sprintf "phev %s housewife 16.0 0.0 16.0 2.5" name]
            else
                [yield sprintf "phev %s worker 16.0 0.0 16.0 2.5" name]

//    profiles |> List.fold (fun ac (profile, peak) -> append profile ac peak)

//    profiles
    let lines = [for profile in profiles do yield! test profile]
    File.WriteAllLines(brp_file, lines)