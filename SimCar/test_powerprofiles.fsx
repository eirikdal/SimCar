#r "bin/Debug/SimCar.dll"
#load "FSharpChart.fsx"

open Models
open FileManager
open MSDN.FSharp.Charting
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

//let data_folder = "C:\\SimCar\\SimCar\\data\\powernodes\\"
let interpol_folder = "C:\\SimCar\\SimCar\\data\\interpol\\"

#nowarn "25"

let profiles = Parsing.parse_powerprofiles(data_folder)
let profiles_interpol = Parsing.parse_powerprofiles(interpol_folder)
//
let take n day = 
    Seq.ofList >> Seq.take n >> List.ofSeq
    >> List.map (fun (name,profile) -> Array.sub profile (96*day) 96)
//    >> List.filter (fun profile -> Array.max profile > 10.0)
    >> List.fold (fun ac profile ->
        Array.map2 (fun x y -> x + y) ac profile) (Array.init (96) (fun _ -> 0.0))

//let test = FSharpChart.Line data
//let sample = Array.sub (snd profiles.Head) 0 96
let sample2 = Array.sub (snd profiles_interpol.Head) 0 96
//let test2 = FSharpChart.Combine [FSharpChart.Line (sample);
//                                    FSharpChart.Line (sample2)]

let dashGrid = 
    Grid( LineColor = Color.Gainsboro, 
          LineDashStyle = ChartDashStyle.Dash )

let create_chart _from _to (data : float<kWh>[]) (title : string) =     
    FSharpChart.Line(data, Name=title)
    |> FSharpChart.WithSeries.Style(BorderWidth = 2)
    |> FSharpChart.WithArea.AxisY
        ( Minimum = _from, Maximum = _to, MajorGrid = dashGrid ) 
    |> FSharpChart.WithArea.AxisX(MajorGrid = dashGrid)
    |> FSharpChart.WithLegend
        (InsideArea=false, Font=new Font("Arial", 8.0f),
        Alignment = StringAlignment.Center, Docking=Docking.Top)
//
//let phev_simulated = FileManager.IO.read_doubles (FileManager.file_phev) |> Array.map (fun x -> Energy.ofFloat x)
let phev_expected = Tree.phev_expected <| create_powergrid()
let powernodes = take 1280 90 profiles_interpol |> Array.map Models.Energy.ofFloat
let total = Array.sum2 phev_expected powernodes
let shaved = DayAhead.Shifted.shave 0.5 0.95 total phev_expected

//let test4 = DayaheadExp.Algorithm.distribute test test2 0.993 1 |> Array.ofList


create_chart 0.0 4000.0 phev_expected "Expected PHEV"
create_chart 0000.0 4000.0 powernodes "powernodes"
create_chart 2000.0 8000.0 shaved "shaved"
create_chart 2000.0 8000.0 total "total"

//create_chart 0.0 4000.0 (Array.sub phev_simulated 288 96) "Simulated PHEV"

//test2.SaveChartAs(img, System.Windows.Forms.DataVisualization.Charting.ChartImageFormat.Png)