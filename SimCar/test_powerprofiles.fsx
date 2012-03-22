#r "bin/Debug/SimCar.dll"
#load "FSharpChart.fsx"

open FileManager
open MSDN.FSharp.Charting

#nowarn "25"

let data = 
    Parsing.powerprofiles
    |> Seq.ofList |> Seq.take 50 |> List.ofSeq
    |> List.map (fun (name,profile) -> Array.sub profile 0 96)
    |> List.fold (fun ac profile ->
        Array.map2 (fun x y -> x + y) ac profile) (Array.init (96) (fun _ -> 0.0))

let test = FSharpChart.Line data


