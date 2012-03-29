using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;
using Microsoft.FSharp.Core;

namespace WinChart
{
    public partial class SimChart : Form
    {
        const int nSim = 10;
        const int nTicks = 96;
        Sim.SimCar tSim = new Sim.SimCar(nSim, nTicks);

        private Mutex mut = new Mutex();
        private const int nRealTime = 0;
        private const int nPowerNodes = 1;
        private const int nPhev = 2;
        private const int nDayAhead = 3;
        private const int nPhevPDF = 4;

        private const int nPhevStatus = 0;
        private const int nPhevBattery = 1;

        private const int nTrfCapacity = 0;
        private const int nTrfCurrent = 1;
        private const int nTrfFiltered = 2;

        private static string[] series = { "Total", "PowerNodes", "PHEVs", "Dayahead", "PHEVs left (not fully charged)" };
        private static string[] _seriesPhev = { "PHEV status", "PHEV battery" };
        private static string[] _seriesTrf = {"Capacity", "Current", "Unfiltered"};

        private static int counter = 0;
        private static int nstep = 0;

        private delegate void saveImageDelegate(string fileName);
        private delegate void updateChartDelegate(Chart chart, int i, Double[] points);
        private delegate void updateChartSaveImageDelegate(Chart chart, int i, Double[] points, bool saveImage, String path);
        private delegate void resetChartDelegate(Chart chart, int i, int j);
        private delegate void resetChartSaveImageDelegate(Chart chart, int i, int j, bool saveImage, String path);
        private delegate void updatePDFDelegate(Double[] chart);
        private delegate void addPointDelegate(Chart chart, int i, Double point);
        private delegate void incrementPointDelegate(Chart chart, int i, int point, Double val);
        

        void saveImageControl(string fileName)
        {


        }

        void updateChart(Chart chart, int series, Double[] points, bool saveImage, String path)
        {
            if (chart.InvokeRequired)
            {
                chart.Invoke(new updateChartSaveImageDelegate(updateChart), new object[] { chart, series, points, saveImage, path });
            }
            else
            {
                updateChart(chart, series, points);

                if (saveImage)
                    chart.SaveImage(String.Format("C:\\SimCar\\SimCar\\data\\img\\{0}\\{1}.png", path, counter++), ChartImageFormat.Png);
                
                for (int j = 0; j < chart1.Series.Count; j++)
                    chart.Series[j].Points.Clear();
            }

        }

        void updateChart(Chart chart, int i, Double[] points)
        {
            if (chart.InvokeRequired)
            {
                chart.Invoke(new updateChartDelegate(updateChart), new object[] { chart, i, points });
            }
            else
            {
                if (chart.Series[i].Points.Count > 0)
                    chart.Series[i].Points.Clear();

                for (int j = 0; j < points.Length; j++)
                    chart.Series[i].Points.Add(points[j]);
            }
        }

        void updatePDF(Double[] prob)
        {
            if (chart2.InvokeRequired)
            {
                chart2.BeginInvoke(new updatePDFDelegate(updatePDF), new object[] { prob });
            }
            else
            {
                for (int i = 0; i < prob.Length; i++)
                {
                    if (chart2.Series[nPhevPDF].Points.Count <= i)
                        chart2.Series[nPhevPDF].Points.Add(prob[i]);
                    else if (prob[i] > chart1.Series[nPhevPDF].Points[i].YValues[0])
                        chart2.Series[nPhevPDF].Points[i].SetValueY(prob[i]);
                }
            }
        }

        void addPoint(Chart chart, int i, Double point)
        {
            if (chart.InvokeRequired)
            {
                chart.BeginInvoke(new addPointDelegate(addPoint), new object[] { chart, i, point });
            }
            else
            {
                if (chart.Series[i].Points.Count >= 96)
                    chart.Series[i].Points.Clear();

                chart.Series[i].Points.Add(point);
            }
        }

        void incrementPoint(Chart chart, int i, int point, double val)
        {
            if (chart.InvokeRequired)
            {
                chart.BeginInvoke(new incrementPointDelegate(incrementPoint), new object[] { chart, i, point, val });
            }
            else
            {
                if (chart.Series[i].Points.Count == 0)
                    for (int j = 0; j < 96; j++)
                        chart.Series[i].Points.Add(0);
                double temp = chart.Series[i].Points[point].YValues[0];
                chart.Series[i].Points[point].SetValueY(temp + 10.0);
            }
        }

        void resetChart(Chart chart, int from, int to, bool saveImage, String path)
        {
            if (chart.InvokeRequired)
            {
                chart.Invoke(new resetChartSaveImageDelegate(resetChart), new object[] { chart, from, to, saveImage, path });
            }
            else
            {
                if (saveImage)
                    chart.SaveImage(String.Format("C:\\SimCar\\SimCar\\data\\img\\{0}\\{1}.png", path, counter++), ChartImageFormat.Png);

                resetChart(chart, from, to);
            }
        }

        void resetChart(Chart chart, int from, int to)
        {
            if (chart.InvokeRequired)
            {
                chart.Invoke(new resetChartDelegate(resetChart), new object[] { chart, from, to });
            }
            else
            {
                for (int j = from; j < to; j++)
                    chart.Series[j].Points.Clear();
            }
        }

        void trfCapacity_Changed(object sender, EventArgs e)
        {
            Double point = (Double)sender;

            addPoint(chart3, nTrfCapacity, point);
        }

        void trfCurrent_Changed(object sender, EventArgs e)
        {
            Double point = (Double)sender;

            //incrementPoint(chart2, nPhevBattery, point, 1.0);
            addPoint(chart3, nTrfCurrent, point);
        }

        void trfFiltered_Changed(object sender, EventArgs e)
        {
            Double point = (Double)sender;

            //incrementPoint(chart2, nPhevBattery, point, 1.0);
            addPoint(chart3, nTrfFiltered, point);
        }

        void phevStatus_Changed(object sender, EventArgs e)
        {
            Double point = (Double) sender;

            addPoint(chart2, nPhevStatus, point);
        }

        void phevFailed_Changed(object sender, EventArgs e)
        {
            int point = (int)sender;

            incrementPoint(chart1, nPhevPDF, point, 1.0);
        }

        void phevBattery_Changed(object sender, EventArgs e)
        {
            Double point = (Double) sender;

            //incrementPoint(chart2, nPhevBattery, point, 1.0);
            addPoint(chart2, nPhevBattery, point);
        }

        void prob_Reset(object sender, EventArgs e)
        {

        }

        void prob_Calc(object sender, EventArgs e)
        {
            Double[] chart = (Double[]) sender;

            updatePDF(chart);
        }
        
        void phev_Changed(object sender, EventArgs e)
        {
            updateChart(chart1, nPhev, (Double[])sender);
        }

        void pnode_Changed(object sender, EventArgs e)
        {
            updateChart(chart1, nPowerNodes, (Double[])sender);
        }

        void total_Changed(object sender, EventArgs e)
        {
            updateChart(chart1, nRealTime, (Double[])sender, true, "power");
            resetChart(chart2, 0, 2, true, "phev");
            resetChart(chart3, 0, 3, true, "trf");
        }

        void chart_moving_average_comparison_Changed(object sender, EventArgs e)
        {
            Double[][] chart = (Double[][])sender;

            chart1.Series[0].Points.Clear();

            for (int j = 0; j < chart.Length; j++)
                chart1.Series[0].Points.Add(chart[0][j]);
        }

        void dayahead_Init(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            this.chart1.Titles[0].Text = (String.Format("Iteration #{0} : Step # {0}", counter, nstep));

            updateChart(chart1, 0, chart, true, "power");
        }
        void dayahead_Changed(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            this.chart1.Titles[0].Text = (String.Format("Iteration #{0} : Step # {1}", counter, nstep++));

            updateChart(chart1, nDayAhead, chart);
        }

        void dayahead_Step(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            this.chart1.Titles[0].Text = (String.Format("Iteration #{0} : Step # {1}", counter, nstep));

            updateChart(chart1, 2, chart, true, "power");
        }

        void setChartLabels(Chart chart)
        {
            chart.ChartAreas[0].AxisX.CustomLabels.Add(23, 24, "06:00");
            chart.ChartAreas[0].AxisX.CustomLabels.Add(47, 48, "12:00");
            chart.ChartAreas[0].AxisX.CustomLabels.Add(71, 73, "18:00");
            chart.ChartAreas[0].AxisX.CustomLabels.Add(90, 95, "24:00");
        }

        public SimChart()
        {
            InitializeComponent();

            chart3.Titles.Add("Transformer");
            chart2.Titles.Add("PHEV");
            chart1.Titles.Add("Iteration # 0 : Step # 0");
            //chart1.Titles.Add("alpha = 0.3, theta = 0.9");

            chart1.Series[0].LegendText = series[nRealTime];
            chart1.Series.Add(series[nPowerNodes]);
            chart1.Series.Add(series[nPhev]);
            chart1.Series.Add(series[nDayAhead]);
            chart1.Series.Add(series[nPhevPDF]);

            //chart1.ChartAreas.Add("ChartArea1");

            chart2.Series[0].LegendText = _seriesPhev[nPhevStatus];
            chart2.Series.Add(_seriesPhev[nPhevBattery]);

            chart3.Series[0].LegendText = _seriesTrf[nTrfCapacity];
            chart3.Series.Add(_seriesTrf[nTrfCurrent]);
            chart3.Series.Add(_seriesTrf[nTrfFiltered]);

            setChartLabels(chart1);
            setChartLabels(chart2);
            setChartLabels(chart3);
            
            Series[] seriesPhev = {chart2.Series[nPhevStatus], chart2.Series[nPhevBattery]};
            Series[] seriesArray = { chart1.Series[nPhevBattery], chart1.Series[nPowerNodes], chart1.Series[nPhev], chart1.Series[nDayAhead], chart1.Series[nPhevPDF] };
            Series[] seriesTrf = {chart3.Series[nTrfCapacity], chart3.Series[nTrfCurrent], chart3.Series[nTrfFiltered] };

            // customize series
            seriesArray[nRealTime].ChartType = SeriesChartType.Line;
            seriesArray[nRealTime].Color = Color.Red;
            seriesArray[nRealTime].BorderWidth = 2;
            seriesArray[nRealTime].BorderDashStyle = ChartDashStyle.Solid;

            seriesArray[nPowerNodes].ChartType = SeriesChartType.Line;
            seriesArray[nPowerNodes].Color = Color.Blue;
            seriesArray[nPowerNodes].BorderWidth = 2;
            seriesArray[nPowerNodes].BorderDashStyle = ChartDashStyle.Dot;

            seriesArray[nPhev].ChartType = SeriesChartType.Line;
            seriesArray[nPhev].Color = Color.Crimson;
            seriesArray[nPhev].BorderWidth = 2;
            seriesArray[nPhev].BorderDashStyle = ChartDashStyle.Dot;

            seriesArray[nPhevPDF].ChartType = SeriesChartType.Point;
            seriesArray[nPhevPDF].Color = Color.Black;
            seriesArray[nPhevPDF].BorderWidth = 2;
            seriesArray[nPhevPDF].BorderDashStyle = ChartDashStyle.Dash;

            seriesArray[nDayAhead].ChartType = SeriesChartType.Line;
            seriesArray[nDayAhead].Color = Color.Sienna;
            seriesArray[nDayAhead].BorderWidth = 2;
            seriesArray[nDayAhead].BorderDashStyle = ChartDashStyle.Solid;

            seriesPhev[nPhevStatus].ChartType = SeriesChartType.StepLine;
            seriesPhev[nPhevStatus].Color = Color.Red;
            seriesPhev[nPhevStatus].BorderWidth = 2;
            seriesPhev[nPhevStatus].BorderDashStyle = ChartDashStyle.Dash;

            seriesPhev[nPhevBattery].ChartType = SeriesChartType.Line;
            seriesPhev[nPhevBattery].Color = Color.Blue;
            seriesPhev[nPhevBattery].BorderWidth = 2;
            seriesPhev[nPhevBattery].BorderDashStyle = ChartDashStyle.Dash;

            seriesTrf[nTrfCapacity].ChartType = SeriesChartType.StepLine;
            seriesTrf[nTrfCapacity].Color = Color.Red;
            seriesTrf[nTrfCapacity].BorderWidth = 2;
            seriesTrf[nTrfCapacity].BorderDashStyle = ChartDashStyle.Dash;

            seriesTrf[nTrfCurrent].ChartType = SeriesChartType.Line;
            seriesTrf[nTrfCurrent].Color = Color.Blue;
            seriesTrf[nTrfCurrent].BorderWidth = 2;
            seriesTrf[nTrfCurrent].BorderDashStyle = ChartDashStyle.Dash;

            seriesTrf[nTrfFiltered].ChartType = SeriesChartType.Line;
            seriesTrf[nTrfFiltered].Color = Color.Sienna;
            seriesTrf[nTrfFiltered].BorderWidth = 2;
            seriesTrf[nTrfFiltered].BorderDashStyle = ChartDashStyle.Solid;
            
            tSim.Init();
            tSim.RegisterEvents();
            tSim.RegisterPhevBattery(phevBattery_Changed);
            tSim.RegisterPhevStatus(phevStatus_Changed);
            tSim.RegisterProgressTotal(total_Changed);
            tSim.RegisterProgressPhev(phev_Changed);
            tSim.RegisterProgressPnode(pnode_Changed);
            tSim.RegisterPhevFailed(phevFailed_Changed);
            //tSim.RegisterProb(prob_Calc);
            //tSim.RegisterProbReset(prob_Reset);
            tSim.RegisterDayaheadProgress(new EventHandler(dayahead_Changed));
            //tSim.RegisterDayaheadInit(new EventHandler(dayahead_Init));
            //tSim.RegisterDayaheadStep(new EventHandler(dayahead_Step));
            //tSim.TestDayahead(nSim);
            tSim.RegisterTrfCapacity(trfCapacity_Changed);
            tSim.RegisterTrfCurrent(trfCurrent_Changed);
            tSim.RegisterTrfFiltered(trfFiltered_Changed);

            Thread oThread = new Thread(new ThreadStart(Start));
            oThread.Start();

            while (!oThread.IsAlive) ;

        }

        public void Start()
        {
            tSim.ComputeDayahead(new FSharpOption<int>(nSim+1));
            resetChart(chart1, 0, chart1.Series.Count);
            resetChart(chart2, 0, chart2.Series.Count);
            tSim.Run(new FSharpOption<int>(nSim));
        }
    }
}
