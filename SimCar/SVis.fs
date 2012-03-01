module SVis

open System
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting

type MapForm(title, data:(int*float*float)[]) = 
    inherit Form( Text=title )

    let map_chart = new Chart(Dock=DockStyle.Fill)
    let map_area = new ChartArea(Name="map")

    let map = new Series()

    do map.ChartType <- SeriesChartType.FastPoint
    do map.Color <- Drawing.Color.Black
    do for (i,x,y) in data do ignore(map.Points.AddXY(x,y))
    do map.ChartArea <- "map"
    
    do map_chart.Series.Add ( map )

    do map_chart.ChartAreas.Add(map_area)
    do base.Controls.Add(map_chart)