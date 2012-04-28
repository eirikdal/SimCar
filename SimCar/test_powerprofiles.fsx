#r "bin/Debug/SimCar.dll"
#load "FSharpChart.fsx"

open Models
open FileManager
open MSDN.FSharp.Charting
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

let data_folder = "C:\\SimCar\\SimCar\\data\\powernodes\\"
let interpol_folder = "C:\\SimCar\\SimCar\\data\\interpol\\"

#nowarn "25"

let profiles = Parsing.parse_powerprofiles(data_folder)
let profiles_interpol = Parsing.parse_powerprofiles(interpol_folder)
//
let take n = 
    Seq.ofList >> Seq.take n >> List.ofSeq
    >> List.map (fun (name,profile) -> Array.sub profile 0 96)
//    >> List.filter (fun profile -> if (Array.max profile) > 10.0 then true else false)
    >> List.fold (fun ac profile ->
        Array.map2 (fun x y -> x + y) ac profile) (Array.init (96) (fun _ -> 0.0))

//let test = FSharpChart.Line data
let sample = Array.sub (snd profiles.Head) 0 96
let sample2 = Array.sub (snd profiles_interpol.Head) 0 96
//let test2 = FSharpChart.Combine [FSharpChart.Line (sample);
//                                    FSharpChart.Line (sample2)]

let test = Tree.phev_expected
let test2 = take 100 profiles_interpol |> Array.map Models.Energy.ofFloat
let test3 = Array.sum2 test test2
let test4 = DayAhead.shave 0.3 0.95 test3

let dashGrid = 
    Grid( LineColor = Color.Gainsboro, 
          LineDashStyle = ChartDashStyle.Dash )

let create_chart (data : float<kWh>[]) (title : string) =     
    FSharpChart.Line(data, Name=title)
    |> FSharpChart.WithSeries.Style(BorderWidth = 2)
    |> FSharpChart.WithArea.AxisY
        ( Minimum = 0.0, Maximum = 240.0, MajorGrid = dashGrid ) 
    |> FSharpChart.WithArea.AxisX(MajorGrid = dashGrid)
    |> FSharpChart.WithLegend
        (InsideArea=false, Font=new Font("Arial", 8.0f),
        Alignment = StringAlignment.Center, Docking=Docking.Top)

create_chart test "Test"
create_chart test2 "Test"
create_chart test3 "Test"
create_chart test4 "Test"

//test2.SaveChartAs(img, System.Windows.Forms.DataVisualization.Charting.ChartImageFormat.Png)