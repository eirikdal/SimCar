#r "bin/Debug/SimCar.dll"
#load "FSharpChart.fsx"

open FileManager
open MSDN.FSharp.Charting

let data_folder = "C:\\SimCar\\SimCar\\data\\powernodes\\"
let interpol_folder = "C:\\SimCar\\SimCar\\data\\interpol\\"

#nowarn "25"

let profiles = Parsing.parse_powerprofiles(data_folder)
let profiles_interpol = Parsing.parse_powerprofiles(interpol_folder)
//
let take n = 
    Seq.ofList >> Seq.take n >> List.ofSeq
    >> List.map (fun (name,profile) -> Array.sub profile 0 96)
    >> List.filter (fun profile -> if (Array.max profile) > 10.0 then true else false)
    >> List.fold (fun ac profile ->
        Array.map2 (fun x y -> x + y) ac profile) (Array.init (96) (fun _ -> 0.0))

//let test = FSharpChart.Line data
let sample = Array.sub (snd profiles.Head) 0 96
let sample2 = Array.sub (snd profiles_interpol.Head) 0 96
//let test2 = FSharpChart.Combine [FSharpChart.Line (sample);
//                                    FSharpChart.Line (sample2)]

let test = Tree.phev_expected
let test2 = take 1000 profiles_interpol |> Array.map Models.Energy.ofFloat
let test3 = Array.sum2 test test2
let test4 = DayAhead.shave 0.3 0.95 test3

FSharpChart.Line test3
FSharpChart.Line test4

//test2.SaveChartAs(img, System.Windows.Forms.DataVisualization.Charting.ChartImageFormat.Png)