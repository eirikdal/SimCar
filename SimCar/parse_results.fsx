#r "C:\SimCar\packages\MathNet.Numerics.2.1.2\lib\Net40\MathNet.Numerics.dll"
#r "C:\SimCar\packages\MathNet.Numerics.FSharp.2.1.2\lib\Net40\MathNet.Numerics.FSharp.dll"
#r "bin/Debug/SimCar.dll"
#load "FSharpChart.fsx"

open System
open Models
open MathNet.Numerics.Statistics
open MSDN.FSharp.Charting
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting


let fileResults = "c:\\SimCar\\SimCar\\data\\log\\experiments\\"
let experiments = ["baseline"; "nullhyp"; "mixed";"random";"proactive-peak-exp";"reactive-peak-exp";"proactive-dist-exp";"reactive-dist-exp"]
let experiment_ys = ["Baseline"; "Min.Extreme"; "Mixed";"Uniform";"Proactive (peak)";"Reactive (peak)";"Proactive (dist)";"Reactive (dist)"]
//let experiments = ["baseline"; "mixed";"random";"proactive-peak-exp";"reactive-peak-exp";"proactive-dist-exp";"reactive-dist-exp"]
//let experiment_ys = ["Baseline"; "Mixed";"Uniform";"Proactive (peak)";"Reactive (peak)";"Proactive (dist)";"Reactive (dist)"]
let dashGrid = 
    Grid( LineColor = Color.Gainsboro, 
          LineDashStyle = ChartDashStyle.Dash )


let count_samples data = 
    [|for (f : double) in 1.00 .. 0.01 .. 1.5 do 
        yield f, data |> Array.fold (fun ac (x : Double) -> if (Math.Round(x, 2, MidpointRounding.AwayFromZero)) = f then ac+1.0 else ac) 0.0 |> (fun x -> if x = 0.0 then Double.NaN else x)|]

//FSharpChart.
let create_comparison_chart (data : (string*float) list) (title : string) (_from : float option) (_to : float option) =     
    let axisY = 
        if _from.IsSome && _to.IsSome then FSharpChart.WithArea.AxisY (MajorGrid = dashGrid, Minimum = _from.Value, Maximum = _to.Value, Title = title) 
        else FSharpChart.WithArea.AxisY ( MajorGrid = dashGrid, Title = title ) 
    FSharpChart.Column(data)
    |> FSharpChart.WithSeries.Style(BorderWidth = 2, Color = Color.FromArgb(99,99,99))
    |> axisY
    |> FSharpChart.WithArea.AxisX(MajorGrid = dashGrid)
    |> FSharpChart.WithLegend
        (InsideArea=false, Font=new Font("Arial", 8.0f),
        Alignment = StringAlignment.Center, Docking=Docking.Top)

let create_boxplot_chart (data : (string*float array) list) (title : string) (_from : float option) (_to : float option) =     
    let axisY = 
        if _from.IsSome && _to.IsSome then FSharpChart.WithArea.AxisY (MajorGrid = dashGrid, Minimum = _from.Value, Maximum = _to.Value, Title = title) 
        else FSharpChart.WithArea.AxisY ( MajorGrid = dashGrid, Title = title ) 
    FSharpChart.BoxPlot(data)
    |> FSharpChart.WithSeries.Style(BorderWidth = 2, Color = Color.FromArgb(99,99,99))
    |> axisY
    |> FSharpChart.WithArea.AxisX(MajorGrid = dashGrid)
    |> FSharpChart.WithLegend
        (InsideArea=false, Font=new Font("Arial", 8.0f),
        Alignment = StringAlignment.Center, Docking=Docking.Top)

let create_chart (data : float list) (title : string) (rangeY : (float*float) option) (rangeX : (float*float) option) =     
    let axisY = 
        if rangeY.IsSome then FSharpChart.WithArea.AxisY (MajorGrid = dashGrid, Minimum = (fst rangeY.Value), Maximum = (snd rangeY.Value), Title = title) 
        else FSharpChart.WithArea.AxisY ( MajorGrid = dashGrid, Title = title ) 
    let axisX = 
         if rangeX.IsSome then FSharpChart.WithArea.AxisX(MajorGrid = dashGrid, Title="time [15 min / x]", Minimum = (fst rangeX.Value), Maximum = (snd rangeX.Value))
         else FSharpChart.WithArea.AxisX(MajorGrid = dashGrid, Title="time [15 min / x]")

    FSharpChart.Area(data)
    |> FSharpChart.WithSeries.Style(BorderWidth = 2, Color = Color.FromArgb(99,99,99))
    |> axisY
    |> axisX
    |> FSharpChart.WithLegend
        (InsideArea=false, Font=new Font("Arial", 8.0f),
        Alignment = StringAlignment.Center, Docking=Docking.Top)

let pretty_chart color chart = 
    chart |> FSharpChart.WithSeries.Style(BorderWidth = 2, Color = color) 

//let create_chart (data : float list) (data2 : float list) (title : string) =  
//    let tick = (new TickMark())
//    let style = new LabelStyle()
//    style.Enabled <- false
//    tick.TickMarkStyle <- TickMarkStyle.None   
//    FSharpChart.Combine [FSharpChart.StackedColumn(data) |> pretty_chart (Color.FromArgb(156,156,156)); FSharpChart.StackedColumn(data2) |> pretty_chart (Color.FromArgb(99,99,99))]
//    |> FSharpChart.WithSeries.Style(BorderWidth = 2, Color = Color.FromArgb(99,99,99))
//    |> FSharpChart.WithArea.AxisY ( Enabled = AxisEnabled.False, LabelStyle = style, MajorTickMark = tick, MinorTickMark = tick, MajorGrid = dashGrid, Title = title ) 
//    |> FSharpChart.WithArea.AxisX( LabelStyle = style, MajorTickMark = tick, MinorTickMark = tick, MajorGrid = dashGrid)
//    |> FSharpChart.WithLegend
//        (InsideArea=false, Font=new Font("Arial", 8.0f),
//        Alignment = StringAlignment.Center, Docking=Docking.Top)

let combine_chart (data : float list) (data2 : float list) (title : string) (subtit1 : string, subtit2 : string) (rangeY : (float*float) option) (rangeX : (float*float) option) =     
    let axisY = 
        if rangeY.IsSome then FSharpChart.WithArea.AxisY (MajorGrid = dashGrid, Minimum = (fst rangeY.Value), Maximum = (snd rangeY.Value), Title = title) 
        else FSharpChart.WithArea.AxisY ( MajorGrid = dashGrid, Title = title ) 
    let axisX = 
         if rangeX.IsSome then FSharpChart.WithArea.AxisX(MajorGrid = dashGrid, Title="time [15 min / x]", Minimum = (fst rangeX.Value), Maximum = (snd rangeX.Value))
         else FSharpChart.WithArea.AxisX(MajorGrid = dashGrid, Title="time [15 min / x]")
    FSharpChart.Combine [FSharpChart.Area(data, Name=subtit1) |> pretty_chart (Color.FromArgb(156,156,156)); FSharpChart.Area(data2, Name=subtit2) |> pretty_chart (Color.FromArgb(99,99,99))]
    |> axisY
    |> axisX
    |> FSharpChart.WithLegend (InsideArea=true, Font=new Font("Arial", 8.0f), Alignment = StringAlignment.Center, Docking=Docking.Bottom) 
        

let hist bats = 
    [for i in 0 .. (96-1) do 
        let (v,count) = bats |> List.fold (fun (ac,n) (idx,v) -> if idx=(float i) then ac+v,n+1 else ac,n) (0.0,0)
        if count <> 0 then yield (v / float count) else yield 0.0]

let rec reduce ac = 
    function 
    | i::v::t -> reduce ((i,v)::ac) t
    | _ -> ac

let collect_results file = 
    List.map (fun folder -> FileManager.IO.read_doubles (fileResults + folder + "\\" + file) |> List.ofArray)

//let collect_bat_hist = collect_results "phev_battery.dat" experiments |> List.map (reduce [])
//let collect_bat_avg = collect_bat_hist |> List.map hist
let collect_stat = List.map (fun (x : float list) -> new DescriptiveStatistics(x))
let collect_mean = List.map (fun (x : DescriptiveStatistics) -> x.Mean)

let from_list = collect_stat >> collect_mean
let from_file file = (collect_results file) 
let from_file_to_mean file = from_file file >> collect_stat >> collect_mean

let phevs_sum = List.zip experiment_ys <| from_file_to_mean "phevs_sum.dat" experiments
let phevs_ux = List.zip experiment_ys <| from_file_to_mean "phevs_ux.dat" experiments
let par = List.zip experiment_ys <| from_file "par.dat" experiments
let trf_delta = List.zip experiment_ys <| from_file_to_mean "trf_delta.dat" experiments
let trf_filtered = List.zip experiment_ys <| from_file_to_mean "trf_filtered.dat" experiments
let total_avg = List.zip experiment_ys <| from_file_to_mean "total_avg.dat" experiments
let total_max = List.zip experiment_ys <| from_file_to_mean "total_max.dat" experiments

//let phevs_battery = (collect_bat_hist |> List.map (hist)) |> from_list |> List.zip experiment_ys
//let avg_battery = collect_bat_hist |> List.map (fun x -> List.map (fun (_,v) -> v) x) |> List.map (fun x -> new DescriptiveStatistics(x))

create_comparison_chart trf_delta "Trf(Delta)" None None
create_comparison_chart trf_filtered "Trf(Filtered)" None None
create_boxplot_chart (List.map (fun (title, vs) -> title, Array.ofList vs) par) "PAR" <| Some 1.1 <| Some 1.25
create_comparison_chart phevs_ux "PHEVs (Ux)" None None
//create_comparison_chart (List.zip experiment_ys (List.map (fun (x:DescriptiveStatistics) -> x.Mean) avg_battery)) "Avg. Battery" None None
//create_comparison_chart total_avg "Avg. Daily Consumption" <| Some 2700.0 <| Some 3000.0
create_comparison_chart total_max "Avg. Daily Peak" <| Some 3200.0 <| Some 3500.0
//create_boxplot_chart (List.map (fun (title, vs) -> title, Array.ofList vs) total_max) "Daily peak" <| Some 2000.0 <| Some 5000.0

//create_chart collect_bat_avg.Head "Average battery histogram (Baseline)" <| Some(0.0,1.0) <| Some(30.0,96.0)
//combine_chart collect_bat_avg.Head collect_bat_avg.Tail.Head "Average battery histogram (Mixed)" ("Baseline","Mixed") <| Some(0.0,1.0) <| Some(30.0,96.0)
//combine_chart collect_bat_avg.Head collect_bat_avg.Tail.Tail.Head "Average battery histogram (Uniform)" ("Baseline","Uniform") <| Some(0.0,1.0) <| Some(30.0,96.0)
//combine_chart collect_bat_avg.Head collect_bat_avg.Tail.Tail.Tail.Head "Average battery histogram (Proactive w/peak-shaving)" ("Baseline","Proactive (peak)") <| Some(0.0,1.0) <| Some(30.0,96.0)
//combine_chart collect_bat_avg.Head collect_bat_avg.Tail.Tail.Tail.Tail.Head "Average battery histogram (Reactive w/peak-shaving)" ("Baseline","Reactive (peak)") <| Some(0.0,1.0) <| Some(30.0,96.0)
//combine_chart collect_bat_avg.Head collect_bat_avg.Tail.Tail.Tail.Tail.Tail.Head "Average battery histogram (Proactive w/distance-rule)" ("Baseline","Proactive (dist)") <| Some(0.0,1.0) <| Some(30.0,96.0)
//combine_chart collect_bat_avg.Head collect_bat_avg.Tail.Tail.Tail.Tail.Tail.Tail.Head "Average battery histogram (Reactive w/distance-rule)" ("Baseline","Reactive (dist)") <| Some(0.0,1.0) <| Some(30.0,96.0)