using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;

namespace WinChart
{
    public partial class SimChart : Form
    {
        private static string[] series = { "Total", "PowerNodes", "PHEVs", "PHEV PDF" };
        private int counter = 0;
        private int nstep = 0;

        void prob_Reset(object sender, EventArgs e)
        {
        }
        void prob_Calc(object sender, EventArgs e)
        {
            Double[] prob = (Double[]) sender;

            for (int i = 0; i < prob.Length; i++)
            {
                if (chart1.Series[3].Points.Count <= i)
                    chart1.Series[3].Points.Add(prob[i]);
                else if (prob[i] > chart1.Series[3].Points[i].YValues[0])
                    chart1.Series[3].Points[i].SetValueY(prob[i]);      
            }
        }

        void chart_Changed(object sender, EventArgs e)
        {
            Double[][] chart = (Double[][])sender;

            //Add series.
            for (int i = 0; i < chart.Length; i++)
            {
                chart1.Series[i].Points.Clear();

                for (int j = 0; j < chart[i].Length; j++)
                    chart1.Series[i].Points.Add(chart[i][j]);
            }

            chart1.SaveImage(String.Format("C:\\SimCar\\SimCar\\data\\img\\{0}.png", counter++), ChartImageFormat.Png);
        }

        void chart_moving_average_comparison_Changed(object sender, EventArgs e)
        {
            Double[][] chart = (Double[][])sender;

            //Add series.
            for (int i = 0; i < chart.Length; i++)
            {
                chart1.Series[i].Points.Clear();

                for (int j = 0; j < chart[i].Length; j++)
                    chart1.Series[i].Points.Add(chart[i][j]);
            }

            chart1.SaveImage(String.Format("C:\\SimCar\\SimCar\\data\\img\\{0}.png", counter++), ChartImageFormat.Png);
        }

        void dayahead_Init(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            this.chart1.Titles[0].Text = (String.Format("Iteration #{0} : Step # {0}", counter, nstep));

            chart1.Series[0].Points.Clear();

            for (int j = 0; j < chart.Length; j++)
                chart1.Series[0].Points.Add(chart[j]);
  
            chart1.SaveImage(String.Format("C:\\SimCar\\SimCar\\data\\img\\{0}.png", counter), ChartImageFormat.Png);
        }
        void dayahead_Progress(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            this.chart1.Titles[0].Text = (String.Format("Iteration #{0} : Step # {1}", counter++, nstep++));
            chart1.Series[1].Points.Clear();

            for (int j = 0; j < chart.Length; j++)
                chart1.Series[1].Points.Add(chart[j]);

            chart1.SaveImage(String.Format("C:\\SimCar\\SimCar\\data\\img\\{0}.png", counter), ChartImageFormat.Png);
        }

        void dayahead_Step(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            this.chart1.Titles[0].Text = (String.Format("Iteration #{0} : Step # {1}", counter++, nstep));
            chart1.Series[2].Points.Clear();

            for (int j = 0; j < chart.Length; j++)
                chart1.Series[2].Points.Add(chart[j]);

            chart1.SaveImage(String.Format("C:\\SimCar\\SimCar\\data\\img\\{0}.png", counter), ChartImageFormat.Png);
        }
        public SimChart()
        {
            InitializeComponent();

            const int nSim = 3;
            const int nTicks = 96;

            const int nRealTime = 0;
            const int nDayAhead = 1;
            const int nDayAheadStep = 2;
            const int nPHEV = 3;

            chart1.Titles.Add("Iteration # 0 : Step # 0");
            chart1.Titles.Add("alpha = 0.3, theta = 0.9");

            chart1.Series[0].LegendText = series[nRealTime];
            //chart1.Series.Add(series[nDayAhead]);
            chart1.Series.Add(series[nDayAhead]);
            chart1.Series.Add(series[nDayAheadStep]);
            chart1.Series.Add(series[nPHEV]);
            
            Series[] seriesArray = { chart1.Series[0], chart1.Series[1], chart1.Series[2], chart1.Series[3] };

            // customize series
            seriesArray[nRealTime].ChartType = SeriesChartType.Line;
            seriesArray[nRealTime].Color = Color.Red;
            seriesArray[nRealTime].BorderWidth = 2;
            seriesArray[nRealTime].BorderDashStyle = ChartDashStyle.Solid;

            seriesArray[nDayAhead].ChartType = SeriesChartType.Line;
            seriesArray[nDayAhead].Color = Color.Blue;
            seriesArray[nDayAhead].BorderWidth = 2;
            seriesArray[nDayAhead].BorderDashStyle = ChartDashStyle.Dot;

            seriesArray[nDayAheadStep].ChartType = SeriesChartType.Line;
            seriesArray[nDayAheadStep].Color = Color.Green;
            seriesArray[nDayAheadStep].BorderWidth = 2;
            seriesArray[nDayAhead].BorderDashStyle = ChartDashStyle.Dash;

            seriesArray[nPHEV].ChartType = SeriesChartType.Line;
            seriesArray[nPHEV].Color = Color.Black;
            seriesArray[nPHEV].BorderWidth = 2;
            seriesArray[nPHEV].BorderDashStyle = ChartDashStyle.Dash;

            Sim.Sim tSim = new Sim.Sim(nSim, nTicks);
            tSim.Init();
            tSim.RegisterEvents();
            tSim.RegisterProgress(chart_Changed);
            tSim.RegisterProb(prob_Calc);
            tSim.RegisterProbReset(prob_Reset);
            double[][] res = tSim.Run();
            //tSim.RegisterDayaheadInit(new EventHandler(dayahead_Init));
            //tSim.RegisterDayaheadStep(new EventHandler(dayahead_Step));
            //tSim.RegisterDayaheadProgress(new EventHandler(dayahead_Progress));
            //tSim.Test_dayahead(nSim);
        }
    }
}
