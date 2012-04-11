module DayaheadExp


open System.Collections.Generic
open Models
open SynchronizationContext
open System
open FileManager

#nowarn "25"
#nowarn "40"

let data_folder = "C:\\SimCar\\SimCar\\data\\powernodes\\"
let interpol_folder = "C:\\SimCar\\SimCar\\data\\interpol\\"

let profiles_interpol = Parsing.parse_powerprofiles(interpol_folder)

let powergrid = 
    FileManager.powergrid()

let collect_exp node = 
    match node with
    | Transformer(_) -> []
    | PHEV(phev_args) as node -> phev_args.profile.to_exp_float(Energy.toFloat <| phev_args.rate)
    | PowerNode(_) -> []
    | BRP(_) -> []

let mutable agg_dist' = 
    powergrid
    |> Tree.map collect_exp 
    |> Tree.collect
    |> List.ofSeq
    |> List.filter (fun x -> if x.Length = 0 then false else true)
    |> List.sumn
    |> Array.ofList

let rate = 1.25

module Algorithm = 
    let distribute realtime days = 
        let theta = 0.995
        let util pos' (day:float array) (pos,x) = 
            let distance = theta ** (float <| abs(pos-pos'))
            distance*(1.0 / day.[pos])
        let dist (day : float array) = 
            let rec fill idx left =
                if left > rate then 
                    let (i,v) = 
                        day
                        |> Array.mapi (fun i x -> (i,x))
                        |> Array.maxBy (util idx day) 
                    day.[i] <- day.[i] + rate
                    fill idx (left-rate)
                else
                    let (idx,v) = 
                        day
                        |> Array.mapi (fun i x -> (i,x))
                        |> Array.minBy (util idx day) 
                    day.[idx] <- day.[idx] + (rate - (rate-left))
                    day.[idx]
                  
            agg_dist' |> Array.mapi fill
        [for i in 0 .. (days-1) do
            let _from,_to = (i*96),(i*96)+96
            let mutable day = Array.sub realtime _from 96
            
            let realtime_updated = dist day
            yield! (List.ofArray day)]

