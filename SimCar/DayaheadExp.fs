module DayaheadExp


open System.Collections.Generic
open Models
open SynchronizationContext
open System
open FileManager

#nowarn "25"
#nowarn "40"

let rate = 1.25<kWh>

module Algorithm = 
    let rand = new Random()
    let distribute_random phev_expected realtime days = 
        let dist (day : float<kWh> array) : float<kWh> array = 
            let rec fill idx left =
                if left > rate then 
                    let i = rand.Next(0,96)
                    day.[i] <- day.[i] + rate
                    fill idx (left-rate)
                else
                    let idx = rand.Next(0,96)
                    day.[idx] <- day.[idx] + (rate - (rate-left))
                    day.[idx]
                  
            phev_expected |> Array.mapi fill
        [for i in 0 .. (days-1) do
            let _from,_to = (i*96),(i*96)+96
            let mutable day = Array.sub realtime _from 96
            
            let realtime_updated = dist day
            yield! (List.ofArray day)]

    let distribute (phev_expected:float<kWh> array) realtime theta days = 
//        printfn "sum of PHEV expected %f" (Array.sum phev_expected)
        let util pos' (day:float<kWh> array) (pos,x) = 
            let distance = theta ** (float <| abs(pos-pos'))
            distance*(1.0<kWh> / day.[pos])
        let dist (day : float<kWh> array) : float<kWh> array = 
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
                  
            phev_expected |> Array.mapi fill
        [for i in 0 .. (days-1) do
            let _from,_to = (i*96),(i*96)+96
            let mutable day = Array.sub realtime _from 96
            
            let realtime_updated = dist day
            yield! (List.ofArray day)]

