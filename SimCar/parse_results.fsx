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


let fileResults = "c:\\SimCar\\SimCar\\data\\log\\experiments\\mixed-01.06.13.30\\"

let dashGrid = 
    Grid( LineColor = Color.Gainsboro, 
          LineDashStyle = ChartDashStyle.Dash )


let count_samples data = 
    [|for (f : double) in 1.00 .. 0.01 .. 1.5 do 
        yield f, data |> Array.fold (fun ac (x : Double) -> if (Math.Round(x, 2, MidpointRounding.AwayFromZero)) = f then ac+1.0 else ac) 0.0 |> (fun x -> if x = 0.0 then Double.NaN else x)|]
//FSharpChart.
let create_chart (data : float[]) (title : string) =     
    FSharpChart.Column(count_samples data)
    |> FSharpChart.WithSeries.Style(BorderWidth = 2)
    |> FSharpChart.WithArea.AxisY
        ( MajorGrid = dashGrid ) 
    |> FSharpChart.WithArea.AxisX(MajorGrid = dashGrid)
    |> FSharpChart.WithLegend
        (InsideArea=false, Font=new Font("Arial", 8.0f),
        Alignment = StringAlignment.Center, Docking=Docking.Top)

let (phevs_sum, phevs_ux, pnodes_sum, total_max, total_avg, total_sum,
        par, dayahead_sum, dif, ratio, trf_delta, trf_filtered) = 
            FileManager.IO.read_doubles (fileResults + "phevs_sum.dat"), FileManager.IO.read_doubles (fileResults + "phevs_ux.dat"), FileManager.IO.read_doubles (fileResults + "pnodes_sum.dat"),
            FileManager.IO.read_doubles (fileResults + "total_max.dat"), FileManager.IO.read_doubles (fileResults + "total_avg.dat"), FileManager.IO.read_doubles (fileResults + "total_sum.dat"),
            FileManager.IO.read_doubles (fileResults + "par.dat"), FileManager.IO.read_doubles (fileResults + "dayahead_sum.dat"), FileManager.IO.read_doubles (fileResults + "dif.dat"),
            FileManager.IO.read_doubles (fileResults + "ratio.dat"), FileManager.IO.read_doubles (fileResults + "trf_delta.dat"), FileManager.IO.read_doubles (fileResults + "trf_filtered.dat")

let phevs_stat, phevs_ux_stat, pnodes_stat, max_stat, avg_stat, sum_stat, 
    par_stat, dayahead_stat, dif_stat, ratio_stat, trf_stat, trf_fltr_stat = 
        new DescriptiveStatistics(phevs_sum),
        new DescriptiveStatistics(phevs_ux),
        new DescriptiveStatistics(pnodes_sum),
        new DescriptiveStatistics(total_max),
        new DescriptiveStatistics(total_avg),
        new DescriptiveStatistics(total_sum),
        new DescriptiveStatistics(par),
        new DescriptiveStatistics(dayahead_sum),
        new DescriptiveStatistics(dif),
        new DescriptiveStatistics(ratio),
        new DescriptiveStatistics(trf_delta),
        new DescriptiveStatistics(trf_filtered)