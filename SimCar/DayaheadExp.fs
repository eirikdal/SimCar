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
    let rand = MathNet.Numerics.Random.Xorshift(true)

    let distribute_Predictions phev_expected realtime days =
        let generate_problist tick window realtime = 
            let baseline = [for i in tick .. (tick+window) do yield realtime(i)]
            let max_baseline = List.max baseline
            let min_baseline = List.min baseline

            List.map (fun x -> (x-min_baseline) / (max_baseline-min_baseline)) baseline

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

    let distribute (phev_expected:float<kWh> array) theta days realtime = 
//        syncContext.RaiseDelegateEvent jobProgress <|  "sum of PHEV expected %f" (Array.sum phev_expected)
        let util pos' (day:float<kWh> array) (pos,x) = 
            let distance = theta ** (float <| abs(pos-pos'))
            distance*(1.0<kWh> / x)
        let dist (day : float<kWh> array) (phev_expected : float<kWh> array) : float<kWh> array = 
            let rec fill idx left =
                if left > rate then 
                    let (i,v) = 
                        day
                        |> Array.mapi (fun i x -> (i,x))
                        |> Array.maxBy (util idx day) 
                    day.[i] <- day.[i] + rate
                    fill idx (left-rate)
                else
                    let (i,v) = 
                        day
                        |> Array.mapi (fun i x -> (i,x))
                        |> Array.maxBy (util idx day) 
                    day.[i] <- day.[i] + (rate - (rate-left))
                    day.[i]
                  
            phev_expected |> Array.mapi fill

        let window_size = 120
                
//        let dayahead = Array.init ((days+1)*96) (fun _ -> 0.0<kWh>)
////
//        for i in 0 .. (days-1) do
//            let _from,_to = (i*96),(i*96)+window_size
//            let day = Array.sub realtime _from window_size
//
//            let phev_expected = 
//                if phev_expected.Length > window_size then
//                    Array.sub phev_expected _from window_size
//                else
//                    Array.init (window_size) (fun x -> phev_expected.[x%96])
//                    
//            dist day phev_expected |> ignore
//
//            for j in _from .. _to-1 do 
//                dayahead.[j] <- day.[j-_from]
//
//        dayahead |> List.ofArray
        [for i in 0 .. (days-1) do
            let _from,_to = (i*96),(i*96)+96
            let mutable day = Array.sub realtime _from 96
            let phev_expected = 
                if phev_expected.Length > 96 then
                    Array.sub phev_expected _from 96
                else
                    phev_expected
            let realtime_updated = dist day phev_expected
            yield! (List.ofArray day)]

